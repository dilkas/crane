package edu.ucla.cs.starai.forclift.compiler

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._

/** Represents a search tree node of the search for a circuit, i.e., a
  * more-or-less incomplete circuit.
  *
  * @param compiler has to be part of the search state because we need to make
  *                 copies of its nnfCache
  * @param circuit  the circuit itself (if there is one)
  * @param formulas formulas that are yet to be compiled, arranged in a
  *                 particular order (the implementation of NNFNode::addNode
  *                 explains this order)
  * @param depth    the depth of the search tree (used as one of the reasons to
  *                 stop the search and because it's an interesting number to
  *                 keep track of)
  */
class PartialCircuit(
    private val compiler: AbstractCompiler,
    val circuit: Option[NNFNode],
    val formulas: List[CNF],
    val depth: Int = 0
) {

  /** Priority value for the priority queue (if it's used).
    *
    *  First we minimise the largest number of constants, then the largest
    * number of clauses (both across all uncompiled formulas). This is just
    * something that seemed to work after a few trial runs.
    */
  lazy val priority: (Int, Int) = (-formulas.map(_.constants.size).max, -length)

  /** Used to compute priority (and nothing else). */
  private[this] lazy val length: Int = formulas.map(_.size).max

  /** Mostly just a wrapper for applyAllRules, but also announces each solution
    * as it's found to the given compiler.
    */
  def nextCircuits(compiler: Compiler): List[PartialCircuit] = {
    val newPartialCircuits = applyAllRules().flatMap { newPartialCircuit =>
      if (newPartialCircuit.formulas.isEmpty) {
        compiler.foundSolution(newPartialCircuit.circuit.get)
        None
      } else {
        Some(newPartialCircuit)
      }
    }
    List(newPartialCircuits: _*)
  }

  // ==================== HELPERS FOR nextCircuits ============================

  /** Attempts to apply each non-greedy rule to the first uncompiled formula.
    *
    * Greedy rules are applied after each successful application of a
    * non-greedy rule. Makes a copy of the PartialCircuit before applying each
    * rule. A recursive function that's only used by nextCircuits. The return
    * type has to be a Stream so that BreadthCompiler can react to each found
    * full circuit with no delay.
    *
    * @return a Stream of PartialCircuits that have been augmented by the
    *         application of one non-greedy rule and any number of greedy rules.
    */
  private def applyAllRules(): Stream[PartialCircuit] = {
    // println("applyAllRules: started")
    val cnf = formulas.head
    Compiler.checkCnfInput(cnf)
    var circuitCopy = myClone()
    val circuits = (0 until compiler.nonGreedyRules.size).toStream.flatMap {
      ruleIndex =>
      {
        circuitCopy.compiler.applyIthRule(ruleIndex, cnf).flatMap {
          case (node: Option[NNFNode], successors: List[CNF]) => {
            val answer = node match {
              case None => {
                // rerun on the updated formula
                require(successors.size == 1)
                new PartialCircuit(
                  circuitCopy.compiler,
                  circuitCopy.circuit,
                  successors ++ circuitCopy.formulas.tail,
                  depth
                ).applyAllRules().toList
              }
              case Some(node) => {
                // println("Node " + node.getClass.getSimpleName + " has " +
                //           successors.size + " successors")
                circuitCopy.compiler.updateCache(cnf, node)
                val newSuccessors = circuitCopy.compiler
                  .applyGreedyRulesToAllFormulas(node, successors)
                List(circuitCopy.add(node, newSuccessors))
              }
            }
            circuitCopy = myClone()
            answer
          }
        }
      }
    }
    // println("applyAllRules: finished")
    circuits
  }

  /** A wrapper for NNFNode::addNode that supports PartialCircuits with empty
    * (None) circuits.
    *
    * Used by applyAllRules.
    */
  private def add(node: NNFNode, newFormulas: List[CNF]): PartialCircuit =
    if (circuit.isDefined) {
      require(circuit.get.addNode(node))
      new PartialCircuit(
        compiler,
        circuit,
        newFormulas ++ formulas.tail,
        depth + 1
      )
    } else {
      new PartialCircuit(
        compiler,
        Some(node),
        newFormulas ++ formulas.tail,
        depth + 1
      )
    }

  /** Make a copy of both the circuit and the compiler.
    *
    * Used by applyAllRules.
    */
  private[this] def myClone(): PartialCircuit = {
    val newCircuit = circuit.map(_.myClone())
    val newCompiler = compiler.myClone()
    new PartialCircuit(newCompiler, newCircuit, formulas, depth)
  }

}
