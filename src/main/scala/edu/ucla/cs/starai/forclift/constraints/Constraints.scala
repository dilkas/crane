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

package edu.ucla.cs.starai.forclift.constraints

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._

import collection._
import edu.ucla.cs.starai.forclift.inference._

object Constraints {

  def empty = new Constraints(IneqConstr.empty, ElemConstr.empty)

}

final case class Constraints(
    ineqConstrs: IneqConstr = IneqConstr.empty,
    elemConstrs: ElemConstr = ElemConstr.empty
) {

  // ========================= ONE-LINERS =====================================

  def addDomain(v: Var, d: Domain): Constraints =
    this.copy(elemConstrs = elemConstrs + (v -> d))

  def addInequality(v: Var, a: Term): Constraints = {
    this.copy(ineqConstrs = ineqConstrs.+(v, a))
  }

  def canBeEqual(x: Var, y: Var): Boolean = {
    !ineqConstrs(x).contains(y) && !elemConstrs(x).disjoint(elemConstrs(y))
  }

  def conflictsWith(eqClasses: List[EquivalenceClass]): Boolean = {
    ineqConstrs.conflictsWith(eqClasses) || elemConstrs.conflictsWith(eqClasses)
  }

  def constants = ineqConstrs.constants

  def differentConstants(eqClass: EquivalenceClass) =
    differentTerms(eqClass).collect { case c: Constant => c };

  def differentConstants(v: Var): Set[Constant] =
    ineqConstrs(v).collect { case c: Constant => c }

  def differentFrom(variables: Set[Var]) =
    ineqConstrs.differentFromVars(variables)

  def differentTerms(eqClass: EquivalenceClass) =
    ineqConstrs.differentFromTerms(eqClass);

  def differentVars(eqClass: EquivalenceClass) =
    differentTerms(eqClass).collect { case v: Var => v };

  def domains = elemConstrs.domains

  def domainFor(variable: Var) = elemConstrs(variable)

  def domainsFor(variables: Set[Var]) = variables.map(elemConstrs(_))

  def project(variables: Set[Var]) = {
    copy(
      ineqConstrs = ineqConstrs.project(variables),
      elemConstrs = elemConstrs.project(variables)
    )
  }

  def setDomain(variable: Var, domain: Domain): Constraints = {
    this.copy(elemConstrs = (elemConstrs + (variable -> domain)))
  }

  def substitute(substitution: Var => Term): Constraints =
    this.copy(
      ineqConstrs = ineqConstrs.substitute(substitution),
      elemConstrs = elemConstrs.substitute(substitution)
    )

  def substituteDomains(substitution: Domain => Domain): Constraints =
    this.copy(
      ineqConstrs = ineqConstrs,
      elemConstrs = elemConstrs.mapDomains(substitution)
    )

  lazy val variables = elemConstrs.variables ++ ineqConstrs.variables

  def variablesNotEqualTo(constant: Constant): Set[Var] =
    ineqConstrs.variablesNotEqualTo(constant)

  def variablesWithDomain(domain: Domain): List[Var] =
    elemConstrs.variablesWithDomain(domain)

  // ========================= EQUALITY CHECKING ==============================

  def canEqual(a: Any) = a.isInstanceOf[Constraints]

  /** Equality is based on the number of variables and the comparison of
    * inequality constraints.
    */
  override def equals(that: Any): Boolean =
    that match {
      case that: Constraints =>
        elemConstrs == that.elemConstrs && ineqConstrs == that.ineqConstrs
      case _ => false
    }

  /** Efficiency of evaluation is very important here. */
  override lazy val hashCode: Int =
    31 * (31 + ineqConstrs.size) + elemConstrs.variables.size

  // ========================= MISCELLANEOUS ==================================

  def addInequalities(va: List[(Var, Term)]): Constraints = {
    val ineqConstrClone = va.foldLeft { ineqConstrs } { (r, c) =>
      r.+(c._1, c._2)
    }
    this.copy(ineqConstrs = ineqConstrClone)
  }

  def addMissingConstraints(
      variables: Set[Var],
      literals: Seq[Atom]
  ): Constraints = {
    val newElemConstrs: ElemConstr = {
      val missingVars = variables -- elemConstrs.variables
      val missingDomains = missingVars.map { v =>
        val atom = literals.find { _.args.contains(v) }.get
        (v -> atom.domain(v))
      }
      new ElemConstr(elemConstrs ++ missingDomains)
    }
    val newIneqConstrs: IneqConstr = {
      // CHECK causes inequality shattering with non-root domains, which is not
      // supported by the implementation
      ineqConstrs.removeRedundant(newElemConstrs)
    }
    new Constraints(newIneqConstrs, newElemConstrs)
  }

  // performance optimisation
  lazy val domainsWithExclusions = elemConstrs.iterator.map { case (v, d) =>
    val excludedConstants = ineqConstrs(v).collect { case c: Constant => c }
    val excludedLogVars: Int = ineqConstrs(v).count { t =>
      t.isInstanceOf[Var] && t.hashCode > v.hashCode
    }
    (excludedConstants, excludedLogVars, d)
  }.toList

  def join(other: Constraints): Constraints = {
    if (this eq other) this
    else {
      val ineqConstr = ineqConstrs join other.ineqConstrs
      val elemConstr = elemConstrs join other.elemConstrs
      new Constraints(ineqConstr, elemConstr)
    }
  }

  /** Shatter inequalities so that both sides have the same set of groundings
    */
  lazy val needsIneqDomainShattering: Boolean = {
    ineqConstrs.exists { case (variable, terms) =>
      val variableDomain = elemConstrs(variable)
      terms.exists {
        case v: Var =>
          (elemConstrs(v).subDomain(variableDomain) ||
          ineqConstrs(v).exists { vineq =>
            vineq != variable && !terms.contains(vineq)
          })
        case _ => false
      }
    }
  }

  def ineqDomainShatteringVarTermPair: Option[(Var, Term)] = {
    var v1: Var = null
    var t2: Term = null
    val found: Boolean = ineqConstrs.exists { case (variable, terms) =>
      terms.exists {
        case v: Var =>
          val termOption = ineqConstrs(v).find { vineq =>
            vineq != variable && !terms.contains(vineq)
          }
          if (termOption.nonEmpty) {
            v1 = variable
            t2 = termOption.get
          }
          termOption.nonEmpty
        case _ => false
      }
    }
    if (found) Some(v1, t2) else None
  }

  def ineqDomainShatteringVarVarPair: Option[(Var, Var)] = {
    var v1: Var = null
    var v2: Var = null
    val found: Boolean = ineqConstrs.exists { case (variable, terms) =>
      val variableDomain = elemConstrs(variable)
      terms.exists { term =>
        val hit = (term.isInstanceOf[Var] &&
          elemConstrs(term.asInstanceOf[Var]).subDomain(variableDomain))
        if (hit) {
          v1 = variable
          v2 = term.asInstanceOf[Var]
        }
        hit
      }
    }
    if (found) Some(v1, v2) else None
  }

  // Functions used for smoothing

  def inverseSubstitution(
      c: Constant,
      v: Var,
      ineqs: Set[Constant],
      domain: Domain
  ): Constraints = {
    val newIneqConstrs = ineqs.foldLeft(ineqConstrs.inverseSubstitution(c, v)) {
      _.+(v, _)
    }
    val newElemConstrs = elemConstrs + (v -> domain)
    copy(ineqConstrs = newIneqConstrs, elemConstrs = newElemConstrs)
  }

  def removeConstraints(constant: Constant) =
    this.copy(ineqConstrs = IneqConstr(ineqConstrs.flatMap {
      case (variable, terms) =>
        terms.flatMap { term: Term =>
          if (term != constant)
            List((variable, term))
          else List()
        }
    }.toList: _*))

  def reverseDomainSplitting(
      from: Domain,
      subdomain: SubDomain
  ): Constraints = {
    val newElemConstrs = elemConstrs.mapDomains({ d =>
      if (d == subdomain || d == subdomain.complement) from else d
    })
    // don't forget to account for inequality constraints that might have been
    // removed because they are trivial
    val subdomainVars = variables.filter { elemConstrs(_) == subdomain }
    val subdomainComplementVars = variables.filter {
      elemConstrs(_) == subdomain.complement
    }
    val missingIneqs = subdomainVars.flatMap { v1 =>
      subdomainComplementVars.map { v2 => (v1, v2) }
    }
    val newIneqConstrs = missingIneqs.foldLeft(ineqConstrs) { (ineqC, pair) =>
      ineqC.+(pair._1, pair._2)
    }
    copy(ineqConstrs = newIneqConstrs, elemConstrs = newElemConstrs)
  }

  // split because the handling of inequality constraints between variables
  // only works when they have the same domain method critical for performance
  def hasSolutionAssumingShatteredDomains(domainSizes: DomainSizes): Boolean = {
    assume(!needsIneqDomainShattering)
    var dims: List[(collection.Set[Constant], Int, Domain)] =
      domainsWithExclusions
    while (dims.nonEmpty) {
      val dim = dims.head
      val domainSize: Int = dim._3.size(domainSizes, dim._1)
      if (domainSize == dim._2) return false
      dims = dims.tail
    }
    return true
  }

  // split because the handling of inequality constraints between variables
  // only works when they have the same domain method critical for performance
  def hasSolutionAssumingShatteredDomains(
      variableNames: Map[Domain, String]
  ): String = {
    assume(!needsIneqDomainShattering)
    var dims: List[(collection.Set[Constant], Int, Domain)] =
      domainsWithExclusions
        .groupBy(_._3)
        .mapValues(l => l.maxBy(_._2))
        .values
        .toList
    var expression: String = ""
    while (dims.nonEmpty) {
      val dim = dims.head
      val domainSize: String = dim._3.symbolicSize(variableNames, dim._1)
      val min = dim._1.size
      val max = dim._1.size + dim._2
      expression += s"$min, LessEqual, $domainSize, LessEqual, $max, "
      dims = dims.tail
    }
    expression.dropRight(2)
  }

  // split because the handling of inequality constraints between variables
  // only works when they have the same domain method critical for performance
  def nbGroundingsAssumingShatteredDomains(domainSizes: DomainSizes): GInt = {
    assume(!needsIneqDomainShattering)
    var nbGroundings: GInt = 1
    var dims: List[(collection.Set[Constant], Int, Domain)] =
      domainsWithExclusions
    while (dims.nonEmpty) {
      val dim = dims.head
      val domainSize: Int = dim._3.size(domainSizes, dim._1)
      nbGroundings *= (domainSize - dim._2)
      dims = dims.tail
    }
    nbGroundings
  }

  def nbGroundingsAssumingShatteredDomains(
      variableNames: Map[Domain, String]
  ): String = {
    assume(!needsIneqDomainShattering)
    var nbGroundings: String = ""
    var dims: List[(collection.Set[Constant], Int, Domain)] =
      domainsWithExclusions
    while (dims.nonEmpty) {
      val dim = dims.head
      val domainSize: String = dim._3.symbolicSize(variableNames, dim._1)
      if (dim._2 == 0) {
        nbGroundings += domainSize + "*"
      } else {
        nbGroundings += s"($domainSize - ${dim._2})*"
      }
      dims = dims.tail
    }
    if (nbGroundings == "") "1" else nbGroundings.dropRight(1)
  }

  // ========================= OUTPUT =========================================

  def toFastWfomc(nameSpace: VarNameSpace) = {
    val ineqConstrStr = ineqConstrs.toFastWfomc(nameSpace, """ != """)
    val elemConstrStr = elemConstrs.toString(nameSpace)
    List(ineqConstrStr, elemConstrStr).filter { _.nonEmpty }.mkString(" & ")
  }

  def toLatex(nameSpace: VarNameSpace, showRootDomains: Boolean = false) = {
    val ineqConstrStr = ineqConstrs.toString(nameSpace, """ \neq """)
    val elemConstrStr =
      elemConstrs.toString(nameSpace, """ \in """, showRootDomains)
    List(ineqConstrStr, elemConstrStr).filter { _.nonEmpty }.mkString(", ")
  }

  override def toString = toString(new VarNameSpace)

  def toString(nameSpace: NameSpace[Any, String]) = {
    val ineqConstrStr = ineqConstrs.toString(nameSpace)
    val elemConstrStr = elemConstrs.toString(nameSpace)
    List(ineqConstrStr, elemConstrStr).filter { _.nonEmpty }.mkString(", ")
  }

}
