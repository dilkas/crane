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

package edu.ucla.cs.starai.forclift.compiler

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.nnf.visitors._

/** A convenient way to switch between greedy and breadth-first search. */
class CompilerWrapper(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    grounding: Boolean = false
) extends Compiler with LazyLogging {

  /** Search type is read from an environmental variable, with greedy search as
    * the default.
    *
    * We use environmental variables instead of command-line arguments because
    * creating a path from the cli package to the compiler package via
    * constructor/method arguments would require changing lots of unrelated
    * pieces of code.
    */
  lazy val greedy: Boolean = {
    val g = Try(sys.env.get("GREEDY").get.toBoolean).getOrElse(true)
    if (g) {
      logger.info("Starting greedy search.")
    } else {
      logger.info("Starting breadth-first search.")
    }
    g
  }

  lazy val compiler = if (greedy) {
    new GreedyCompiler(sizeHint, grounding)
  } else {
    new BreadthCompiler(sizeHint, grounding)
  }

  /** After compilation completes, run two visitors on the circuit that
    * together update the 'domains' field of NNFNode.
    */
  override def compile(cnf: CNF): List[NNFNode] = {
    val nnfs = compiler.compile(cnf)
    nnfs.foreach { nnf =>
      {
        val postOrderVisitor = new PostOrderVisitor
        postOrderVisitor.visit(nnf)
        val domainsVisitor = new DomainsVisitor(postOrderVisitor.nodeOrder)
        domainsVisitor.updateDomains()
      }
    }
    nnfs
  }

}
