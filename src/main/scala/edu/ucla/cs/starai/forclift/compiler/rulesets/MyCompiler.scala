/*
 * Copyright 2023 Paulius Dilkas (National University of Singapore)
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
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

/** The latest version of the compiler.
  *
  * Adds three more rules:
  * 1) an improved version of domain recursion capable of creating cycles,
  * 2) constraint removal that removes a bunch of 'X != x' constraints by
  *    creating a new domain without domain value x,
  * 3) contradiction filter that finds a completely empty clause and removes
  *    all other clauses so that the formula would be immediately identified as
  *    unsatisfiable by another rule.
  *
  * @param nnfCache see the AbstractCompiler class
  */
abstract class MyCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends V1_1Compiler(sizeHint, nnfCache) {

  def tryImprovedDomainRecursion(cnf: CNF) =
    cnf.domainsWithVariablesInLiterals.map {
      domain => {
        val constant = groundingConstantFor(cnf, domain)
        val mixedClauses = cnf.clauses.flatMap { clause =>
          {
            // only consider the subset of variables that come from the same
            // domain
            val vars = clause.literalVariables.filter {
              clause.constrs.domainFor(_).equals(domain)
            }
            vars.subsets.flatMap { equalVariables =>
              {
                val substitutedClause =
                  clause.substituteOption((variable: Var) =>
                    if (equalVariables.contains(variable))
                      constant
                    else variable
                  )
                substitutedClause match {
                  case Some(s) => {
                    val ineqVars = vars -- equalVariables
                    List(ineqVars.foldLeft(s) { _.addInequality(_, constant) })
                  }
                  case None => List()
                }
              }
            }
          }
        }
        val mixedCNF = new CNF(mixedClauses)
        val msg = "Improved domain recursion on $" + domain + "$."
        val node = new ImprovedDomainRecursionNode(
          cnf,
          None,
          constant,
          domain,
          msg
        )
        logger.trace("\n" + msg + " Before:")
        logger.trace(cnf.toString)
        logger.trace("After:")
        logger.trace(mixedCNF + "\n")
        (Some(node), List(mixedCNF))
      }
    }.toList

  def tryConstraintRemoval(cnf: CNF): InferenceResult = {
    for (originalClause <- cnf) {
      for ((variable, terms) <- originalClause.constrs.ineqConstrs) {
        val originalDomain = originalClause.constrs.domainFor(variable)

        for (term <- terms) {
          term match {
            case constant: Constant => {
              // We have a "v != c" constraint. Does it apply to all
              // variables from the same domain across all clauses? And does
              // c occur in atoms? I.e., for each clause, for each variable,
              // either domain is different or there is the same inequality
              // constraint.
              if (
                cnf.forall { clause =>
                  clause.atoms.forall { atom: Atom =>
                    !atom.constants.contains(constant)
                  }
                } &&
                  cnf.forall { clause =>
                    clause.allVariables.forall { variable: Var =>
                      clause.constrs.domainFor(variable) !=
                        originalDomain ||
                        clause.constrs.ineqConstrs(variable).contains(constant)
                    }
                  }
              ) {
                val newIndex = (originalDomain.nbSplits + 1).toString
                val newDomain = originalDomain.subdomain(
                  newIndex,
                  newIndex,
                  excludedConstants = Set(constant)
                )
                val newCnf = CNF(cnf.map { clause =>
                                   clause
                                     .removeConstraints(constant)
                                     .replaceDomains(originalDomain, newDomain)
                                 }.toList: _*)
                val node = new ConstraintRemovalNode(
                  cnf,
                  None,
                  originalDomain,
                  newDomain
                )

                logger.trace("\nConstraint removal. Before:")
                logger.trace(cnf.toString)
                logger.trace("Constraint removal. After:")
                logger.trace(newCnf + "\n")

                return List((Some(node), List(newCnf)))
              }
            }
            case _ => {}
          }
        }
      }
    }
    List[Result]()
  }

  def tryContradictionFilter(cnf: CNF) = {
    val contradictionClauseOption = cnf.clauses.find { c =>
      c.isConditionalContradiction && c.isUnconditional
    }
    if (contradictionClauseOption.nonEmpty) {
      List((None, List(new CNF(List(contradictionClauseOption.get)))))
    } else {
      List[Result]()
    }
  }

  override def greedyRules: List[InferenceRule] =
    List(
      tryTautology,
      tryContradictionClause,
      tryPositiveUnitClause,
      tryNegativeUnitClause,
      tryContradictionFilter, // new
      tryTautologyClauseElimination,
      tryRemoveDoubleClauses, // revived
      tryPositiveUnitPropagation,
      tryNegativeUnitPropagation,
      tryConstraintRemoval, // new
      tryIndependentSubtheories, // +1
      tryIndependentSubtheoriesAfterShattering
    )

  override def nonGreedyRules: List[InferenceRule] =
    List(
      tryCache, // revamped
      tryGroundDecomposition, // +1
      tryInclusionExclusion, // +2
      tryShatter, // 0
      tryIndependentPartialGrounding, // 0
      tryCounting, // 0
      tryImprovedDomainRecursion // 0, new
    )

}

class MyLiftedCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends MyCompiler(sizeHint, nnfCache)
    with LiftedCompiler {

  def myClone(): MyLiftedCompiler =
    new MyLiftedCompiler(sizeHint, cloneCache())

}

class MyGroundingCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends MyCompiler(sizeHint, nnfCache)
    with GroundingCompiler {

  def myClone(): MyGroundingCompiler =
    new MyGroundingCompiler(sizeHint, cloneCache())

}
