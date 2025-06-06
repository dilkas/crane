/*
 * Copyright 2025 Paulius Dilkas (University of Toronto)
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.forclift

import collection._
import scala.collection.immutable.Stream
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._

/** @param excludedDomains
  *   is used by ImprovedDomainRecursion and AtomCounting to avoid using more
  *   than one such operation on the same domain.
  */
final case class CNF(
    val clauses: List[Clause],
    val excludedDomains: Set[Domain] = Set.empty
) extends SetProxy[Clause] {

  lazy val isSuitableForRecursion: Boolean =
    domains.forall(d1 => domains.forall(d2 => !d1.subDomain(d2)))

  lazy val self = clauses.toSet

  override lazy val hashCode = super.hashCode

  lazy val variables = clauses.flatMap { _.literalVariables }.toSet

  def isGround = variables.isEmpty

  lazy val constants = clauses.flatMap { _.constants }.toSet

  lazy val predicates = clauses.flatMap { _.predicates }.toSet

  def domains = clauses.flatMap { _.domains }.toSet

  def domainsWithVariablesInLiterals =
    clauses
      .flatMap { c => c.domainsFor(c.literalVariables) }
      .toSet
      .diff(excludedDomains)

  lazy val atoms = clauses.flatMap { _.atoms }.toSet

  def isSingleton = clauses.nonEmpty && clauses.tail.isEmpty

  def toLatex(showRootDomains: Boolean = false): String = {
    if (clauses.isEmpty) """$\top$"""
    else {
      clauses
        .map { _.toLatex(showRootDomains) }
        .mkString("""$\begin{array}{c} """, """ \\ """, """ \end{array}$""")
    }
  }

  def toFastWfomc = clauses.map { _.toFastWfomc }.mkString(" & ")

  override def +(clause: Clause) = new CNF(clause :: clauses, excludedDomains)

  def isTautology = clauses.isEmpty

  def removeTautologies: CNF = {
    val tautologyFreeClauses = clauses.filterNot { _.isTautology }
    if (tautologyFreeClauses.size == clauses.size) this
    else new CNF(tautologyFreeClauses, excludedDomains)
  }

  def toPositiveUnitClauses: Set[PositiveUnitClause] =
    clauses.flatMap { _.toPositiveUnitClauses }.toSet

  lazy val distinctPositiveUnitClauses = {
    val allCAtoms = clauses.flatMap { _.toPositiveUnitClauses }
    val distinctCAtoms = disjoin(allCAtoms)
    distinctCAtoms
  }

  def coShatter = shatterInternalEqualities.shatter

  def equiprobableClasses = {
    val coShatteredThis = coShatter
    val allCAtoms = coShatteredThis.clauses.flatMap {
      _.toPositiveUnitClauses.map(_.removeExternalConstraints)
    }
    disjoin(allCAtoms)
  }

  def shatterInternalEqualities: CNF = {
    new CNF(clauses.flatMap { _.shatterInternalEqualities }, excludedDomains)
  }

  lazy val shatter: CNF = {
    shatterIneqs.shatterDomains
  }

  private[this] final def disjoin(
      cAtoms: List[PositiveUnitClause]
  ): List[PositiveUnitClause] = {
    val atomsByPredicate = cAtoms.groupBy { _.atom.predicate }
    atomsByPredicate.values.flatMap { predAtoms =>
      val uniqueStringAtoms = predAtoms.map { a => (a.toString -> a) }.toMap
      disjoinHard(uniqueStringAtoms.values.toList)
    }.toList
  }

  private[this] final def disjoinHard(
      cAtoms: List[PositiveUnitClause]
  ): List[PositiveUnitClause] =
    cAtoms match {
      case Nil => Nil
      case cAtom :: restAtoms => {
        val standerdizedAtom = cAtom.standardizeApart
        standerdizedAtom :: disjoin(
          restAtoms.filterNot(standerdizedAtom.equivalent(_))
        )
      }

    }

  final def shatterIneqs: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          val shatteredForCAtom = shClause.flatMap {
            _.shatter(cAtom.atom, cAtom.constrs)
          }
          shClause = shatteredForCAtom
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap {
            _.toPositiveUnitClauses
          } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      cAtoms = disjoin(nextCAtoms.filter { next =>
        !cAtoms.exists { next.subsumes(_) }
      })
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(
        clauses.flatMap { _.shatterIneqDomains },
        excludedDomains
      )).shatterIneqs
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses, excludedDomains)
    }
  }

  final private def shatterDomains: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          shClause = shClause.flatMap {
            _.shatterDomains(cAtom.atom, cAtom.constrs)
          }
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap {
            _.toPositiveUnitClauses
          } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      cAtoms = disjoin(nextCAtoms.filter { next =>
        !cAtoms.exists { next.subsumes(_) }
      })
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(
        clauses.flatMap { _.shatterIneqDomains },
        excludedDomains
      )).shatterDomains
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses, excludedDomains)
    }
  }

  def ++(other: CNF) = {
    new CNF(clauses ++ other.clauses, excludedDomains ++ other.excludedDomains)
  }

  def ground(domainSizes: DomainSizes): CNF = {
    new CNF(clauses.flatMap { _.ground(domainSizes) }, excludedDomains)
  }

  def condition(cliteral: UnitClause): CNF = {
    val conditionedClauses = cliteral match {
      case posLit: PositiveUnitClause =>
        clauses.flatMap { _.condition(true, posLit.atom, posLit.constrs) }
      case negLit: NegativeUnitClause =>
        clauses.flatMap { _.condition(false, negLit.atom, negLit.constrs) }
      case _ =>
        throw new IllegalStateException("Clause not positive or negative")
    }
    new CNF(conditionedClauses, excludedDomains)
  }

  def simplify(ignoredClauses: Set[Clause] = Set.empty): CNF = {
    val propagatableUnitClauses = clauses.filter { propClause =>
      !ignoredClauses(propClause) && propClause.isUnitClause && clauses.exists {
        clause =>
          !(clause eq propClause) && !propClause.independent(clause)
      }
    }
    if (propagatableUnitClauses.nonEmpty) {
      def propagate(
          unitClauses: List[Clause],
          theory: List[Clause]
      ): (List[Clause], List[Clause]) =
        unitClauses match {
          case Nil => (Nil, theory)
          case unitClause :: otherUnitClauses => {
            val unitLiteral = unitClause.atoms.head
            val otherClauses: List[Clause] = theory filterNot (_ == unitClause)
            val propagatedClauses = otherClauses.flatMap { clause =>
              val conditionedClause = clause.condition(
                unitClause.isPositiveUnitClause,
                unitLiteral,
                unitClause.constrs
              )
              conditionedClause
            }.toList
            val propagatedOtherUnitClauses = otherUnitClauses.flatMap {
              _.condition(
                unitClause.isPositiveUnitClause,
                unitLiteral,
                unitClause.constrs
              )
            }
            val (propagatedUnitClauses, simplifiedRest) =
              propagate(propagatedOtherUnitClauses, propagatedClauses)
            (unitClause :: propagatedUnitClauses, simplifiedRest)
          }
        }
      val (propagated, simplifiedRest) =
        propagate(propagatableUnitClauses, clauses)
      val newCnf = (new CNF(propagated ++ simplifiedRest, excludedDomains))
        .simplify(ignoredClauses ++ propagated)
      newCnf
    } else this
  }

  def independent(other: Clause) = clauses.forall(_.independent(other))
  def dependent(other: Clause) = !independent(other)

  def independentSubtheories: List[CNF] = {
    def partition(
        depClauses: List[Clause],
        indepClauses: List[Clause]
    ): (List[Clause], List[Clause]) = {
      if (indepClauses.isEmpty) (depClauses, Nil)
      else
        depClauses match {
          case clause :: rest => {
            val (indep, dep) = indepClauses.partition(clause.independent(_))
            val (depAll, indepAll) = partition(rest ++ dep, indep)
            (clause :: depAll, indepAll)
          }
          case Nil => (Nil, indepClauses)
        }
    }
    val (dep, indep) = partition(List(clauses.head), clauses.tail)
    if (indep.isEmpty) List(this)
    else
      (new CNF(dep, excludedDomains)) :: (new CNF(
        indep,
        excludedDomains
      )).independentSubtheories
  }

  override def toString = {
    if (isTautology) """true"""
    else clauses.toArray.sortBy { _.toString }.mkString("\n")
  }

  def eqToConstraints: CNF = {
    new CNF(clauses.map(_.eqToConstraints), excludedDomains)
  }

}

/** Most of this object is the implementation of identifyRecursion. */
object CNF {

  def apply(clauses: Clause*) = new CNF(clauses.toList, Set.empty)
  def apply(excludedDomains: Set[Domain], clauses: Clause*) =
    new CNF(clauses.toList, excludedDomains)

  /** Describes the relationship between the domains of both (i.e., the new and
    * the old) formulas.
    *
    * Each domain d of the new formula is mapped to a domain d' of the old
    * formula.
    */

  final case class DomainNotMatchedException(
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  /** Tries to identify newFormula as oldFormula but with some domains replaced
    * by their subdomains, irrespective of variable names.
    *
    * @param newFormula
    *   the formula that corresponds to the new circuit node which is being
    *   added
    * @param oldFormula
    *   the formula for an already-existing node
    * @param partialMap
    *   maps domains of oldFormula to domains of newFormula
    *
    * Before returning the completed version of partialMap, we express all
    * domains of newFormula that appear in partialMap as subdomains of the
    * domains of oldFormula.
    */
  def identifyRecursion(
      newFormula: CNF,
      oldFormula: CNF,
      partialMap: Map[Domain, Domain] = Map.empty,
      foundConstraintRemoval: Boolean = false
  ): Option[Map[Domain, Domain]] = {
    if (
      newFormula.size != oldFormula.size ||
      newFormula.hashCode != oldFormula.hashCode
    ) {
      None
    } else if (oldFormula.isEmpty && newFormula.isEmpty) {
      Some(partialMap)
    } else {
      for (clause1 <- oldFormula) {
        val updatedOldFormula =
          new CNF((oldFormula - clause1).toList, oldFormula.excludedDomains)
        for (clause2 <- newFormula.filter(_.hashCode == clause1.hashCode)) {
          val updatedNewFormula =
            new CNF((newFormula - clause2).toList, newFormula.excludedDomains)

          val bijections =
            clause1.variableAndDomainBijections(clause2, partialMap)
          for {
            (bijection, domainBijection) <- bijections
            if clause1
              .substitute(bijection)
              .substituteDomains(domainBijection)
              .exactlyEquals(clause2)
          } {
            var foundConstraintRemoval2 = foundConstraintRemoval
            try {
              val recursion = identifyRecursion(
                updatedNewFormula,
                updatedOldFormula,
                partialMap ++ domainBijection,
                foundConstraintRemoval2
              )
              if (recursion.isDefined) {
                return recursion
              }
            } catch {
              case e: DomainNotMatchedException => {}
            }
          }
        }
        return None
      }
      None
    }
  }

}
