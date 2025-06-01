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

package edu.ucla.cs.starai.forclift.compiler

import collection._
import com.typesafe.scalalogging.LazyLogging
import java.util.concurrent._

import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object Compiler {

  /** The data type of nnfCache: a Map that acts as a collection of buckets. */
  type Buckets = mutable.HashMap[Int, List[(CNF, NNFNode)]]

  object Builder {
    val default: Builder = (sizeHint: SizeHints) =>
      new CompilerWrapper(sizeHint, false)
    val defaultWithGrounding: Builder = (sizeHint: SizeHints) =>
      new CompilerWrapper(sizeHint, true)
  }

  type Builder = ((Domain => Int) => Compiler)

  def default: Compiler = new CompilerWrapper(grounding = false)

  object SizeHints {
    def unknown(d: Domain) = 1
  }

  type SizeHints = (Domain => Int)

  def checkCnfInput(cnf: CNF) {
    require(
      !cnf.domains.contains(Universe),
      s"Cannot compile CNFs containing the universe domain: $cnf"
    )
    require(
      !cnf.domains.contains(EmptyDomain),
      s"Cannot compile CNFs containing the empty domain: $cnf"
    )
  }

}

/** Compilation methods can now return a list of solutions. */
trait Compiler {

  def compile(cnf: CNF): List[NNFNode] = {
    throw new IllegalStateException(
      "The compiler you are trying to use does " +
        "not implement the 'compile' method."
    )
  }

  def compileSmooth(cnf: CNF): List[NNFNode] = {
    compileSmooth(cnf, cnf.predicates, Set.empty)
  }

  def compileSmooth(
      cnf: CNF,
      predicates: Set[Predicate],
      excluded: Set[PositiveUnitClause] = Set.empty
  ): List[NNFNode] = {
    val nnfs = compile(cnf)
    nnfs.map(_.smoothWithPredicates(predicates, excluded))
  }

  /** Used by BreadthCompiler to announce each solution as soon as it's found.
    */
  def foundSolution(circuit: NNFNode): Unit = {}

}

trait GroundingCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    new GroundingNode(cnf, "No rule fires.")
  }

}

trait LiftedCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    throw new IllegalArgumentException(
      "Cannot compile " + cnf + " without grounding."
    )
  }

}

/** The parent class of all compilers.
  *
  * Implements tryCache and some of the code used to apply rules to formulas
  * (the rest is in PartialCircuit).
  * @param nnfCache
  *   maps hash codes of formulas to pairs of formulas and their circuit nodes.
  *   It is used to identify formulas potentially suitable for a Ref node, i.e.,
  *   to add additional arcs to the circuit that make it no longer a tree.
  */
abstract class AbstractCompiler(
    val nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends Compiler
    with LazyLogging {

  // ============================== TYPES ====================================

  /** The result type of applying an inference rule to a formula.
    *
    * None means that the inference rule doesn't apply. (None, _) means that a
    * new node wasn't created but the formula was updated (we assume that in
    * this case the list of successor formulas has exactly one element). In all
    * other cases, the list holds formulas that will be compiled into direct
    * successors of the returned circuit node.
    */
  protected type Result = (Option[NNFNode], List[CNF])
  protected type InferenceResult = List[Result]

  protected type InferenceRule = CNF => InferenceResult

  // ============================== MISC METHODS ==============================

  /** The main purpose of myClone is to call cloneCache. */
  def myClone(): AbstractCompiler

  def cannotCompile(cnf: CNF): NNFNode

  // ============================== CACHE MANAGEMENT ==========================

  /** The Compiler (nnfCache) part of the cloning process that starts in
    * PartialCircuit.
    *
    * Replaces references to circuit nodes in nnfCache with their new
    * alternatives as defined in NNFNode.cloningCache. Assumes that
    * NNFNode.cloningCache is already populated.
    */
  protected def cloneCache(): Compiler.Buckets =
    nnfCache.map {
      case (key, value) => {
        val newValue = value.map {
          case (formula, node) => (formula, NNFNode.cloningCache(node))
        }
        (key, newValue)
      }
    }

  /** Put the (cnf, nnf) pair in nnfCache. */
  def updateCache(cnf: CNF, nnf: NNFNode): Unit = {
    assume(nnf != null)
    if (cnf.isSuitableForRecursion &&
      !nnf.isInstanceOf[Ref] && (
        !nnfCache.contains(cnf.hashCode) || !nnfCache(cnf.hashCode).exists {
          case (_, node) => node == nnf
        }
      )
    ) {
      nnfCache(cnf.hashCode) = (cnf, nnf) ::
        nnfCache.getOrElse(cnf.hashCode, List[(CNF, NNFNode)]())
    }
  }

  /** An inference rule for creating Ref nodes, i.e., edges that deviate from
    * the otherwise tree-like structure of the circuit.
    *
    * Uses nnfCache and CNF.identifyRecursion to find an already-discovered
    * formula compatible with the input one. Can be computationally costly for
    * even moderate size formulas.
    */
  def tryCache(cnf: CNF): InferenceResult = {
    if (!nnfCache.contains(cnf.hashCode)) {
      List[Result]()
    } else {
      nnfCache(cnf.hashCode).toStream
        .map {
          case (formula, circuit) => {
            CNF.identifyRecursion(cnf, formula) match {
              case Some(recursion) => Some((circuit, recursion))
              case None            => None
            }
          }
        }
        .collectFirst { case Some(x) => x } match {
        case Some(results) => {
          logger.trace("\nCache hit.")
          logger.trace("Before:")
          logger.trace(cnf.toString)
          logger.trace("After:")
          logger.trace(results._1.cnf.toString)
          logger.trace("Domain map:")
          logger.trace(results._2 + "\n")

          val node = new Ref(cnf, Some(results._1), results._2, "Cache hit.")
          /* don't cache the Ref node because the node targeted by this Ref
           * will do
           */
          List((Some(node), List[CNF]()))
        }
        case None => {
          List[Result]()
        }
      }
    }
  }

  // ============================== INFERENCE RULES ===========================

  /** Rules that should be applied as soon as possible. */
  def greedyRules: List[InferenceRule]

  /** Rules that are treated as decisions in the search for a circuit. */
  def nonGreedyRules: List[InferenceRule]

  def inferenceRules: List[InferenceRule] = greedyRules ++ nonGreedyRules

  /** Applies the i-th non-greedy inference rule to the given formula. */
  def applyIthRule(i: Int, cnf: CNF): InferenceResult = try {
    nonGreedyRules(i)(cnf)
  } catch {
    // This works around some bugs related to shattering
    case _: IllegalStateException         => List[Result]()
    case _: UnsupportedOperationException => List[Result]()
  }

  /** Constructs the maximal PartialCircuit that can be built using only greedy
    * rules.
    */
  def applyGreedyRules(cnf: CNF): PartialCircuit = {
    var rules = greedyRules
    while (rules.nonEmpty) {
      try {
        rules.head(cnf) match {
          case Nil => {
            rules = rules.tail
          }
          case (node, successors) :: tail => {
            // We assume that all greedy rules produce at most one solution
            require(tail.isEmpty)
            node match {
              case None => {
                require(successors.size == 1)
                return applyGreedyRules(successors.head)
              }
              case Some(nnf) => {
                updateCache(cnf, nnf)
                val newSuccessors =
                  applyGreedyRulesToAllFormulas(nnf, successors)
                return new PartialCircuit(this, Some(nnf), newSuccessors)
              }
            }
          }
        }
      } catch {
        // This works around a bug in the implementation of shattering
        case _: IllegalStateException         => rules = rules.tail
        case _: UnsupportedOperationException => rules = rules.tail
      }
    }
    new PartialCircuit(this, None, List(cnf))
  }

  /** Runs applyGreedyRules on all the given formulas, adding the resulting
    * subcircuits as direct successors of the input node and returning new
    * successor formulas that were created in the process.
    */
  def applyGreedyRulesToAllFormulas(
      node: NNFNode,
      formulas: List[CNF]
  ): List[CNF] = if (formulas.nonEmpty) {
    require(formulas.size == node.directSuccessors.size)
    require(node.directSuccessors.forall(_.isEmpty))
    val partialCircuits = formulas.map(applyGreedyRules(_))
    val newNodes = partialCircuits.map(_.circuit)
    node.update(newNodes)
    partialCircuits.flatMap(_.formulas)
  } else {
    List[CNF]()
  }

}
