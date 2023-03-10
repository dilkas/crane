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

package edu.ucla.cs.starai.forclift.compiler

import collection._

import java.util.concurrent._

import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object Compiler {

  /** The data type of nnfCache: a Map that acts as a collection of buckets. */
  type Buckets = mutable.HashMap[Int, List[(CNF, NNFNode)]]

  object Builder {
    val default: Builder = CompilerWrapper.builder
    val defaultWithGrounding: Builder = CompilerWrapper.builderWithGrounding
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
  * @param nnfCache maps hash codes of formulas to pairs of formulas and their
  *                 circuit nodes. It is used to identify formulas potentially
  *                 suitable for a Ref node, i.e., to add additional arcs to
  *                 the circuit that make it no longer a tree.
  */
abstract class AbstractCompiler(
    val nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends Compiler {

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

  // ============================== DATA ======================================

  /** A hacky way to turn a bunch of println statements on and off. */
  private[this] val Verbose = true

  // ============================== MISC METHODS ==============================

  /** The main purpose of myClone is to call cloneCache. */
  def myClone(): AbstractCompiler

  /** Used to log what compilation rules are being applied and how they change
    * the formula. */
  @inline protected final def log(s: => Any): Unit = if (Verbose) println(s)

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
          case (formula, node) => {
            // println("Does cloningCache of size " + NNFNode.cloningCache.size +
            //           " contain the " + node.getClass.getSimpleName + " " +
            //           node.hashCode + ": " + NNFNode.cloningCache.contains(node))
            (formula, NNFNode.cloningCache(node))
          }
        }
        (key, newValue)
      }
    }

  /** Put the (cnf, nnf) pair in nnfCache. */
  def updateCache(cnf: CNF, nnf: NNFNode): Unit = {
    assume(nnf != null)

    // println("updateCache: trying to add " + nnf.getClass.getSimpleName + " " +
    //           nnf.hashCode)
    // println("updateCache: hash code already exists: " +
    //           nnfCache.contains(cnf.hashCode))
    // println("updateCache: a cache entry for the same node exists: " +
    //           (nnfCache.contains(cnf.hashCode) && nnfCache(cnf.hashCode).exists
    //              { case (_, node) => node == nnf }))

    if (
      !nnf.isInstanceOf[Ref] && (
        !nnfCache.contains(cnf.hashCode) || !nnfCache(cnf.hashCode).exists {
          case (_, node) => node == nnf
        }
      )) {
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
    // println("tryCache started for formula:")
    // println(cnf)
    if (!nnfCache.contains(cnf.hashCode)) {
      // println("tryCache: not found")
      List[Result]()
    } else {
      // println("tryCache: found. The bucket has " + nnfCache(cnf.hashCode).size +
      //           " elements.")
      nnfCache(cnf.hashCode).toStream.map {
        case (formula, circuit) => {
          // println("tryCache: formula:")
          // println(formula)
          // println("tryCache: the other formula:")
          // println(circuit.cnf)

          CNF.identifyRecursion(cnf, formula) match {
            case Some(recursion) => Some((circuit, recursion))
            case None            => None
          }
        }
      }.collectFirst { case Some(x) => x } match {
        case Some(results) => {
          log("\nCache hit.")
          log("Before:")
          log(cnf)
          log("After:")
          log(results._1.cnf)
          log("Domain map:")
          log(results._2 + "\n")

          val node = new Ref(cnf, Some(results._1), results._2, "Cache hit.")
          // don't cache the Ref node because the node targeted by this Ref
          // will do
          // updateCache(cnf, node)
          // println("tryCache finished")
          List((Some(node), List[CNF]()))
        }
        case None => {
          // println("tryCache finished")
          List[Result]()
        }
      }
    }
  }

  /* A wrapper for tryCache that adds a timeout feature.
   *
   * To use it, uncomment the code below, set the desired timeout value, and
   * replace tryCache with tryCache2 in the relevant list of inference rules
   * (e.g., in MyCompiler).
   */
  // val Timeout = 1 // s, for tryCache
  // def tryCache2(cnf: CNF): InferenceResult = {
  //   val task = new FutureTask(new Callable[InferenceResult]() {
  //     def call() = tryCache(cnf)
  //   })

  //   try {
  //     new Thread(task).start()
  //     task.get(Timeout, TimeUnit.SECONDS)
  //   } catch {
  //     case _: TimeoutException => None
  //   }
  // }

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
    case _: IllegalStateException => List[Result]()
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
          case (node, successors)::tail => {
            // We assume that all greedy rules produce at most one solution
            require(tail.isEmpty)
            node match {
              case None => {
                require(successors.size == 1)
                return applyGreedyRules(successors.head)
                }
              case Some(nnf) => {
                // println("applyGreedyRules: adding")
                // println(cnf)
                // println("AND")
                // println(nnf.cnf)
                updateCache(cnf, nnf)
                val newSuccessors = applyGreedyRulesToAllFormulas(nnf,
                                                                  successors)
                return new PartialCircuit(this, Some(nnf), newSuccessors)
              }
            }
          }
        }
      } catch {
        // This works around a bug in the implementation of shattering
        case _: IllegalStateException => rules = rules.tail
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
