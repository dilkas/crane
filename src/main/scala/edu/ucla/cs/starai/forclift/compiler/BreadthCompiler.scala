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

import scala.collection.mutable._
import scala.util.Try

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler.rulesets._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._

/** A variation of breadth-first search but with some rules applied in a greedy
  * manner.
  *
  * Can easily be adjusted to use a priority queue with some kind of
  * heuristics. Runs until either the required number of solution circuits is
  * found or the maximum depth of the search tree is exhausted.
  */
class BreadthCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    grounding: Boolean = false,
    skolemize: String = ""
) extends Compiler {

  /** Found solutions */
  private[this] var circuits: List[NNFNode] = List[NNFNode]()

  /** Two parameters for the extensiveness of search */
  lazy val maxDepth: Int = Try(sys.env.get("DEPTH").get.toInt).getOrElse(-1)
  lazy val numSolutions: Int = Try(sys.env.get("SOLUTIONS").get.toInt).
    getOrElse(1)

  private[this] final case class EndSearchException(
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  def compilerBuilder = if (skolemize.nonEmpty) {
      new LiftedSkolemizer(sizeHint, skolemize)
    } else if (grounding) {
      new MyGroundingCompiler(sizeHint)
    } else {
      new MyLiftedCompiler(sizeHint)
    }

  override def foundSolution(circuit: NNFNode): Unit = {
    circuits = circuit :: circuits
    println("FOUND " + circuits.size + " SOLUTION(S)")
    if (circuits.size >= numSolutions)
      throw new EndSearchException
  }

  override def compile(cnf: CNF): List[NNFNode] = {
    val compiler = compilerBuilder
    val initialCircuit = compiler.applyGreedyRules(cnf)


    try {
    if (initialCircuit.formulas.isEmpty) {
      foundSolution(initialCircuit.circuit.get)
    } else {
      val q = Queue(initialCircuit)
      var depth = 0
        while ((maxDepth < 0 || depth <= maxDepth) && q.nonEmpty) {
          println("depth: " + depth)
          val partialCircuit = q.dequeue
          if (partialCircuit.depth > depth) {
            depth = partialCircuit.depth
          }
          if (maxDepth < 0 || depth <= maxDepth)
            q ++= partialCircuit.nextCircuits(this)
        }
    }
      } catch {
        case e: EndSearchException => {}
      }

    if (!circuits.isEmpty) {
      circuits
    } else {
      List(compiler.cannotCompile(cnf))
    }
  }

}
