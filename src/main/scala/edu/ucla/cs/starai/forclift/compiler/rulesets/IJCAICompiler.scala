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

package edu.ucla.cs.starai.forclift.compiler.rulesets

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler._
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import edu.ucla.cs.starai.forclift.nnf._
import constraints._

object IJCAI11Compiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new IJCAI11LiftedCompiler(sizeHint)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new IJCAI11GroundingCompiler(sizeHint)

}

abstract class IJCAI11Compiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends AbstractCompiler(nnfCache) {

  def tryTautology(cnf: CNF) = {
    if (cnf.isTautology) {
      logger.trace("\ntautology\n")
      List((Some(TrueNode), List[CNF]()))
    } else List[Result]()
  }

  def tryPositiveUnitClause(cnf: CNF) = {
    /* Only compile if the clause is unconditional. Otherwise do Shannon
     * decomposition to separate the conditions from the literals, to help with
     * smoothing.
     */
    val isPositiveUnit =
      cnf.isSingleton && cnf.clauses.head.isPositiveUnitClause && cnf.clauses.head.isUnconditional
    if (isPositiveUnit) {
      val unitClause = cnf.clauses.head
      val unitLeaf = new UnitLeaf(cnf, unitClause.toUnitClause, true)
      List((Some(unitLeaf), List[CNF]()))
    } else List[Result]()
  }

  def tryNegativeUnitClause(cnf: CNF) = {
    val isPositiveUnit =
      cnf.isSingleton && cnf.clauses.head.isNegativeUnitClause && cnf.clauses.head.isUnconditional
    if (isPositiveUnit) {
      val unitClause = cnf.clauses.head
      val unitLeaf = new UnitLeaf(cnf, unitClause.toUnitClause, false)
      List((Some(unitLeaf), List[CNF]()))
    } else List[Result]()
  }

  def tryContradictionClause(cnf: CNF) = {
    val isConditionalContradiction =
      cnf.clauses.size == 1 && cnf.clauses.head.isConditionalContradiction
    if (isConditionalContradiction) {
      logger.trace("\ncontradiction clause")
      val contradiction = cnf.clauses.head
      val contradictionLeaf =
        new ContradictionLeaf(cnf, contradiction.toContradictionClause, true)
      logger.trace(cnf.toString + "\n")
      List((Some(contradictionLeaf), List[CNF]()))
    } else List[Result]()
  }

  def tryPositiveUnitPropagation(cnf: CNF) = {
    val unitClauseOption = cnf.clauses.find { c =>
      c.isPositiveUnitClause && c.isUnconditional
    }
    if (unitClauseOption.nonEmpty) {
      val unitClause = unitClauseOption.get
      val unitLiteral = unitClause.atoms.head
      val otherClauses: List[Clause] = cnf.clauses filterNot (_ == unitClause)
      val propagatedClauses = otherClauses.flatMap {
        _.condition(true, unitLiteral, unitClause.constrs)
      }
      val branchCnf = new CNF(propagatedClauses, cnf.excludedDomains)
      val unitCNF = CNF(cnf.excludedDomains, unitClause)
      val msg = "unit propagation of $" + unitClause.toLatex() + "$."
      val node = new And(cnf, None, None, msg)
      logger.trace("\nPositive " + msg + " Before:")
      logger.trace(cnf.toString)
      logger.trace("After:")
      logger.trace(branchCnf + "\n")
      List((Some(node), List(unitCNF, branchCnf)))
    } else List[Result]()
  }

  def tryNegativeUnitPropagation(cnf: CNF) = {
    val unitClauseOption = cnf.clauses.find { c =>
      c.isNegativeUnitClause && c.isUnconditional
    }
    if (unitClauseOption.nonEmpty) {
      val unitClause = unitClauseOption.get
      val unitLiteral = unitClause.atoms.head
      val otherClauses: List[Clause] = cnf.clauses filterNot (_ == unitClause)
      val propagatedClauses = otherClauses.flatMap {
        _.condition(false, unitLiteral, unitClause.constrs)
      }
      val branchCnf = new CNF(propagatedClauses, cnf.excludedDomains)
      val unitCNF = CNF(cnf.excludedDomains, unitClause)
      val msg = "unit propagation of $" + unitClause.toLatex() + "$."
      val node = new And(cnf, None, None, msg)
      logger.trace("\nNegative " + msg + " Before:")
      logger.trace(cnf.toString)
      logger.trace("After:")
      logger.trace(branchCnf + "\n")
      List((Some(node), List(unitCNF, branchCnf)))
    } else List[Result]()
  }

  def tryRemoveDoubleClauses(cnf: CNF): InferenceResult = {
    val newClauses = cnf.clauses.toSet.toList
    if (newClauses.size < cnf.clauses.size) {
      logger.trace("\nremove double clauses. Before:")
      logger.trace(cnf + "\n")
      val newCnf = new CNF(newClauses, cnf.excludedDomains)
      logger.trace("After:")
      logger.trace(newCnf + "\n")
      List((None, List(newCnf)))
    } else List[Result]()
  }

  def tryIndependentSubtheories(cnf: CNF): InferenceResult = {
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
    val (dep, indep) = partition(List(cnf.clauses.head), cnf.clauses.tail)
    if (indep.isEmpty) List[Result]()
    else {
      val msg = "Independence."
      val node = new And(cnf, None, None, msg)
      val child1 = new CNF(dep, cnf.excludedDomains)
      val child2 = new CNF(indep, cnf.excludedDomains)

      logger.trace("\n" + msg + " Before:")
      logger.trace(cnf.toString)
      logger.trace("After 1:")
      logger.trace(child1.toString)
      logger.trace("After 2:")
      logger.trace(child2 + "\n")

      List((Some(node), List(child1, child2)))
    }
  }

  // separate function to aid profiling
  def shatter(cnf: CNF) = cnf.shatter

  def onlyIfGround(f: InferenceRule)(cnf: CNF) = {
    if (cnf.isGround) f(cnf)
    else List[Result]()
  }

  def tryShannonDecomposition(cnf: CNF): InferenceResult = {
    val groundLiterals = cnf.clauses.flatMap { _.groundLiterals }
    if (groundLiterals.nonEmpty) {
      val groupedAtoms = groundLiterals.groupBy(a => a)
      val atomCounts = groupedAtoms.mapValues(list => list.size)
      val ordering = new Ordering[(Atom, Int)] {
        def compare(t1: (Atom, Int), t2: (Atom, Int)) = {
          t1._2 - t2._2
        }
      }
      val (literal, _) = atomCounts.max(ordering)
      val trueBranch = cnf + Clause(List(literal), List())
      val falseBranch = cnf + Clause(List(), List(literal))
      val msg =
        "Shannon decomposition on $" + literal.toLatex(new VarNameSpace) + "$."
      val node = new Or(cnf, None, None, msg)
      logger.trace("\n" + msg + " Before:")
      logger.trace(cnf.toString)
      logger.trace("After 1:")
      logger.trace(trueBranch.toString)
      logger.trace("After 2:")
      logger.trace(falseBranch + "\n")
      List((Some(node), List(trueBranch, falseBranch)))
    } else List[Result]()
  }

  def tryInclusionExclusion(cnf: CNF) = {
    val decomposableClauseOption = cnf.clauses.find {
      _.independentLiterals.nonEmpty
    }
    if (decomposableClauseOption.nonEmpty) {
      val Some(clause) = decomposableClauseOption
      val otherClauses = cnf.clauses filterNot (_ == clause)
      val Some((cl1, cl2)) = clause.independentLiterals
      val plus1Branch = new CNF(cl1 :: otherClauses, cnf.excludedDomains)
      val plus2Branch = new CNF(cl2 :: otherClauses, cnf.excludedDomains)
      val minBranch = new CNF(cl1 :: cl2 :: otherClauses, cnf.excludedDomains)
      val msg = "Inclusion-exclusion on $" + clause.toLatex() + "$."
      val node = new InclusionExclusion(cnf, None, None, None, msg)

      logger.trace("\nInclusion-exclusion. Before:")
      logger.trace(cnf.toString)
      logger.trace("After 1:")
      logger.trace(plus1Branch.toString)
      logger.trace("After 2:")
      logger.trace(plus2Branch.toString)
      logger.trace("After 3:")
      logger.trace(minBranch.toString)

      List((Some(node), List(plus1Branch, plus2Branch, minBranch)))
    } else List[Result]()
  }

  def tryShatter(cnf: CNF) = {
    val shatteredCnf = shatter(cnf)
    if (shatteredCnf eq cnf) List[Result]()
    else {
      logger.trace("\nShattering. Before:")
      logger.trace(cnf.toString)
      logger.trace("After:")
      logger.trace(shatteredCnf + "\n")
      List((Some(new ShatterNode(cnf, None)), List(shatteredCnf)))
    }
  }

  case class IndexedConstant(val i: Int) {
    override def toString = "x" + (if (i > 0) ("'" * i) else "")
  }

  def groundingConstant(i: Int, domain: Domain) = {
    val c = new Constant(new IndexedConstant(i))
    c.setDomain(domain)
  }

  def groundingConstantFor(cnf: CNF, domain: Domain) = {
    val existingIndices = cnf.constants
      .filter {
        _.value.isInstanceOf[IndexedConstant]
      }
      .map { _.value.asInstanceOf[IndexedConstant].i }
      .toSet
    val newIndex = Stream.from(0).find { index => !existingIndices(index) }.get
    groundingConstant(newIndex, domain)
  }

  type ChoiceMap = collection.mutable.Map[Clause, Var]

  def tryIndependentPartialGrounding(cnf: CNF): InferenceResult = {
    /* in the future, this should be implemented by finding all binding classes
    and then checking size and root */
    if (cnf.clauses.exists(_.rootVars(cnf.excludedDomains).isEmpty))
      return List[Result]()
    else {
      // every clause has a root variable -- we can try
      val chosenVariables = collection.mutable.Map.empty[Clause, Var]
      val (unaryClauses, multiClauses) = cnf.clauses.partition {
        _.literalVariables.size == 1
      }
      /* first choose the root variables of unary clauses. They cannot lead to
      conflicts */
      for (c <- unaryClauses) chosenVariables(c) = c.literalVariables.head
      var chosenClauses = unaryClauses
      // one by one add all variables that are the only roots in their clause
      val (singletonRoots, multiRoots) = multiClauses.partition {
        _.rootVars(cnf.excludedDomains).size == 1
      }
      for (c <- singletonRoots) {
        if (
          !tryAddingVariable(
            chosenVariables,
            chosenClauses,
            c,
            c.rootVars(cnf.excludedDomains).head,
            cnf.excludedDomains
          )
        ) {
          return List[Result]()
        }
        chosenClauses = c :: chosenClauses
      }
      // now do search for the remainder
      val finalChoice = searchChoices(
        chosenVariables,
        chosenClauses,
        multiRoots.sortBy(_.rootVars(cnf.excludedDomains).size),
        cnf.excludedDomains
      )
      if (finalChoice.isEmpty) return List[Result]()
      else {
        val solution = finalChoice.get
        val rootVars = solution.values.toSet
        val rootVarDomains = rootVars.flatMap { rootVar =>
          cnf.clauses.filter { _.literalVariables(rootVar) }.map {
            _.constrs.domainFor(rootVar)
          }
        }.toSet
        assume(rootVarDomains.size == 1) // because of independence
        val rootVarDomain = rootVarDomains.head
        val constant = groundingConstantFor(cnf, rootVarDomain)
        def substituteRootVars(v: Var): Term = {
          if (rootVars.contains(v)) constant
          else v
        }
        val invertedClauses = cnf.clauses.map { clause =>
          val substitutedClause = clause.substitute(solution(clause), constant)
          substitutedClause
        }
        val invertedCNF =
          new CNF(invertedClauses, cnf.excludedDomains + rootVarDomain)
        val rootVarIneqs = cnf.clauses.flatMap { clause =>
          clause.constrs.ineqConstrs(solution(clause)).collect {
            case c: Constant => c
          }
        }.toSet
        val msg =
          ("""Independent partial grounding of $ X \in """ + rootVarDomain + """ $""" +
            (if (rootVarIneqs.isEmpty) "."
             else
               """, $ """ + rootVarIneqs
                 .map { """X \neq """ + _.toString }
                 .mkString(" , ") + " $."))
        val inversionNode = new IndependentPartialGroundingNode(
          cnf,
          None,
          constant,
          rootVarIneqs,
          rootVarDomain,
          msg
        )
        logger.trace("\nIndependent partial grounding")
        logger.trace(cnf.toString + "\n")
        List((Some(inversionNode), List(invertedCNF)))
      }
    }
  }

  /** Optimization to reduce the size of the search tree: propagate choices
    */
  private[this] def searchChoices(
      choices: ChoiceMap,
      chosenClauses: List[Clause],
      otherClauses: List[Clause],
      excludedDomains: Set[Domain]
  ): Option[ChoiceMap] = {
    if (otherClauses.isEmpty) Some(choices)
    else {
      val clause :: tailClauses = otherClauses
      val roots = clause.rootVars(excludedDomains)
      for (root <- roots) {
        val newChoices = choices.clone
        if (
          tryAddingVariable(
            newChoices,
            chosenClauses,
            clause,
            root,
            excludedDomains
          )
        ) {
          // now also assign variables from clauses that depend on the just assigned clause (requires no search)
          val (dependentClauses, independentClauses) =
            tailClauses.partition(_ dependent clause)
          val newChosenClausesOption = propagateBindingClass(
            newChoices,
            clause,
            root,
            clause :: chosenClauses,
            dependentClauses,
            excludedDomains
          )
          if (newChosenClausesOption.nonEmpty) {
            val completeChoices = searchChoices(
              newChoices,
              newChosenClausesOption.get,
              independentClauses,
              excludedDomains
            )
            if (completeChoices.nonEmpty) return completeChoices
          } // else try next root
        } // else try next root
      }
      return None
    }
  }

  private[this] def propagateBindingClass(
      choices: ChoiceMap,
      clause: Clause,
      root: Var,
      chosenClauses: List[Clause],
      dependentClauses: List[Clause],
      excludedDomains: Set[Domain]
  ): Option[List[Clause]] = {
    var newChosenClauses = chosenClauses
    for (depClause <- dependentClauses) {
      val bindingVariable = boundVars(clause, root, depClause)
      require(
        bindingVariable.size > 0,
        "Input dependentClauses have to depend on clause"
      )
      if (bindingVariable.size != 1) return None
      val newRoot = bindingVariable.head
      if (!depClause.rootVars(excludedDomains)(newRoot)) return None
      if (
        !tryAddingVariable(
          choices,
          newChosenClauses,
          depClause,
          newRoot,
          excludedDomains
        )
      )
        return None
      newChosenClauses = depClause :: newChosenClauses
    }
    return Some(newChosenClauses)
  }

  private[this] def tryAddingVariable(
      choices: ChoiceMap,
      chosenClauses: List[Clause],
      clause: Clause,
      v: Var,
      excludedDomains: Set[Domain]
  ): Boolean = {
    require(clause.rootVars(excludedDomains)(v))
    require(!choices.contains(clause))
    // check if v appears in two incompatible positions in clause
    // for example: friends(x,y) => friends(y,x), x != y.
    // where x and y do not unify
    val boundSelfVars = boundVars(clause, v, clause.standardizeApart)
    assume(boundSelfVars.size >= 1, "v should at least unify with its copy v'")
    if (boundSelfVars.size > 1) {
      return false // v unifies with more than its copy v' when standardizing apart
    }
    for (chosenClause <- chosenClauses) {
      if (chosenClause dependent clause) {
        val chosenClauseVar = choices(chosenClause)
        val bindingInNewClause =
          boundVars(chosenClause, chosenClauseVar, clause)
        assume(bindingInNewClause.size > 0)
        if (bindingInNewClause.size > 1 || !bindingInNewClause.contains(v)) {
          return false // bad bindings in new clause
        }
        val bindingInChosenClause = boundVars(clause, v, chosenClause)
        assume(bindingInChosenClause.size > 0)
        if (
          bindingInChosenClause.size > 1 || !bindingInChosenClause.contains(
            chosenClauseVar
          )
        ) {
          return false // bad bindings in already chosen clause
        }
      }
    }
    choices(clause) = v
    return true
  }

  // function to list all variables bound
  private[this] def boundVars(c1: Clause, c1Var: Var, c2: Clause): Set[Var] = {
    val equivalences = c1.atoms.flatMap { lit: Atom =>
      c2.atoms.flatMap { atom2: Atom =>
        atom2.unifyConstrained(lit, c1.constrs, c2.constrs).getOrElse(List())
      }
    }
    equivalences.filter { eq => eq(c1Var) }.foldLeft(Set[Var]()) {
      _ union _.variables
    } - c1Var
  }

  def tryAtomCounting(cnf: CNF) = {
    val singletons = cnf.clauses.flatMap { clause =>
      clause.singletonLiterals(cnf.excludedDomains).map { literal =>
        (clause, literal, clause.constrs)
      }
    }
    if (singletons.nonEmpty) {
      logger.trace("\nAtom counting. Before:")
      logger.trace(cnf.toString)
      // the heuristic is: split on the atom with highest #occurences - domain size
      val groupedAtoms = singletons.map { case (clause1, lit1, constrs1) =>
        // ordering by occurence
        val nbOccurence = singletons.count { case (clause2, lit2, constrs2) =>
          (clause1 ne clause2) && lit1.unifies(lit2, constrs2, constrs1)
        }
        val domain = constrs1.domainFor(lit1.variables.head)
        val domainSize = sizeHint(domain.root)
        (lit1, constrs1, nbOccurence - domainSize)
      }
      val ordering = new Ordering[(Atom, Constraints, Int)] {
        def compare(
            t1: (Atom, Constraints, Int),
            t2: (Atom, Constraints, Int)
        ) = t1._3 - t2._3
      }
      val (bestLit, bestConstrs, _) = groupedAtoms.max(ordering)
      val unitConstrs = bestConstrs.project(bestLit.variables)
      val logVar = bestLit.variables.head
      val excludedConstants =
        unitConstrs.ineqConstrs(logVar).map { _.asInstanceOf[Constant] }
      val domain = unitConstrs.domainFor(logVar)
      val singletonName = Clause(List(bestLit), List(), unitConstrs).toLatex()
      val splitIndex = domain.nbSplits + 1
      val subdomain = domain.subdomain(
        """ \top """,
        """ \bot """,
        splitIndex.toString,
        splitIndex.toString,
        excludedConstants.toSet
      )
      val msg = "Atom counting on $" + singletonName + "$."

      val trueUnitClause = Clause(
        List(bestLit),
        List(),
        unitConstrs.setDomain(logVar, subdomain)
      ).standardizeApart
      val falseUnitClause = Clause(
        List(),
        List(bestLit),
        unitConstrs.setDomain(logVar, subdomain.complement)
      ).standardizeApart
      val childCNF = new CNF(
        trueUnitClause :: falseUnitClause :: cnf.clauses,
        cnf.excludedDomains + domain
      )

      val node = new CountingNode(cnf, None, domain, subdomain, msg)
      logger.trace("After:")
      logger.trace(childCNF + "\n")
      List((Some(node), List(childCNF)))
    } else List[Result]()
  }

  def greedyRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation
  )

  def nonGreedyRules: List[InferenceRule] = List(
    tryIndependentSubtheories,
    tryShannonDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding,
    tryAtomCounting
  )

}

class IJCAI11LiftedCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends IJCAI11Compiler(sizeHint, nnfCache)
    with LiftedCompiler {

  def myClone(): IJCAI11LiftedCompiler =
    new IJCAI11LiftedCompiler(sizeHint, cloneCache())

}

class IJCAI11GroundingCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends IJCAI11Compiler(sizeHint, nnfCache)
    with GroundingCompiler {

  def myClone(): IJCAI11GroundingCompiler =
    new IJCAI11GroundingCompiler(sizeHint, cloneCache())

}
