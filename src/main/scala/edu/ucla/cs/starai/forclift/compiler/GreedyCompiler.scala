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

package edu.ucla.cs.starai.forclift.compiler

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler.rulesets._
import edu.ucla.cs.starai.forclift.nnf._

/** The original way of constructing circuits via greedily applying whichever
  * inference rule is noticed to apply first.
  *
  * The inferenceRules are arranged in a particular order, with greedyRules at
  * the front and nonGreedyRules afterwards (see AbstractCompiler and the rulesets
  * subpackage).
  */
class GreedyCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    grounding: Boolean = false,
    skolemize: String = ""
) extends Compiler {

  lazy val compiler = if (skolemize.nonEmpty) {
    new LiftedSkolemizer(sizeHint, skolemize)
  } else if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  override def compile(cnf: CNF): List[NNFNode] = {
    Compiler.checkCnfInput(cnf)
    var rules = compiler.inferenceRules
    var nnf = List[NNFNode]()
    while (nnf.isEmpty && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) {
        val (node, successors) = tryRule.head
        if (node.isEmpty) {
          require(successors.size == 1)
          nnf = compile(successors.head)
        } else {
          nnf = List(node.get)
          compiler.updateCache(cnf, nnf.head)
          nnf.head.update(
            successors.map(successor => Some(compile(successor).head))
          )
        }
      } else rules = rules.tail
    }
    if (nnf.isEmpty) {
      nnf = List(compiler.cannotCompile(cnf))
    }
    nnf
  }

}
