/*
 * Copyright 2025 Paulius Dilkas (University of Toronto)
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

/** A 'compiler' for performing Skolemization and simplifying the resulting
  * formula using unit propagation.
  *
  * We use this as Step 1 for converting MLN instances into the format used by
  * FastWFOMC.
  *
  * @param nnfCache
  *   see the AbstractCompiler class
  */
abstract class Skolemizer(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends V1_1Compiler(sizeHint, nnfCache) {

  override def greedyRules: List[InferenceRule] =
    List(
      tryTautology,
      tryContradictionClause,
      tryPositiveUnitClause,
      tryNegativeUnitClause,
      tryPositiveUnitPropagation,
      tryNegativeUnitPropagation
    )

  override def nonGreedyRules: List[InferenceRule] = List()

}

final case class SkolemizationFinishedException(
    val formula: String = "",
    val filename: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(formula, cause)

class LiftedSkolemizer(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    skolemize: String = "",
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends Skolemizer(sizeHint, nnfCache)
    with LiftedCompiler {

  def myClone(): LiftedSkolemizer =
    new LiftedSkolemizer(sizeHint, skolemize, cloneCache())

  override def cannotCompile(cnf: CNF): NNFNode = {
    throw new SkolemizationFinishedException(cnf.toFastWfomc, skolemize)
  }

}

class GroundingSkolemizer(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends Skolemizer(sizeHint, nnfCache)
    with GroundingCompiler {

  def myClone(): GroundingSkolemizer =
    new GroundingSkolemizer(sizeHint, cloneCache())

}
