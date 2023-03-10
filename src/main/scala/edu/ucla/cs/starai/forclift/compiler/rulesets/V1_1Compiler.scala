/*
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
import edu.ucla.cs.starai.forclift.nnf._

object V1_1Compiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new V1_1LiftedCompiler(sizeHint)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new V1_1GroundingCompiler(sizeHint)

}

abstract class V1_1Compiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends NIPS11Compiler(sizeHint, nnfCache) {

  def tryTautologyClauseElimination(cnf: CNF) = {
    val newCnf = cnf.removeTautologies
    if (newCnf eq cnf) List[Result]()
    else List((None, List(newCnf)))
  }

  override def greedyRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation
  )

  override def nonGreedyRules: List[InferenceRule] = List(
    tryTautologyClauseElimination, // added wrt NIPS11
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding, // O(log(n))
    tryCounting, // O(n)
    tryDomainRecursion // is O(log(n)) now! But assumes no unary predicates
  )

}

class V1_1LiftedCompiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends V1_1Compiler(sizeHint, nnfCache) with LiftedCompiler {

  def myClone(): V1_1LiftedCompiler =
    new V1_1LiftedCompiler(sizeHint, cloneCache())

}

class V1_1GroundingCompiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends V1_1Compiler(sizeHint, nnfCache) with GroundingCompiler {

  def myClone(): V1_1GroundingCompiler =
    new V1_1GroundingCompiler(sizeHint, cloneCache())

}
