package edu.ucla.cs.starai.forclift.compiler

import scala.collection.mutable._
import scala.util.Try

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler.rulesets._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._

object BreadthCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, true)

}

/** A variation of breadth-first search but with some rules applied in a greedy
  * manner.
  *
  * Can easily be adjusted to use a priority queue with some kind of
  * heuristics. Runs until either the required number of solution circuits is
  * found or the maximum depth of the search tree is exhausted.
  */
class BreadthCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    grounding: Boolean = false
) extends Compiler {

  /** Found solutions */
  private[this] var circuits: List[NNFNode] = List[NNFNode]()

  /** Two parameters for the extensiveness of search */
  lazy val maxDepth: Int = Try(sys.env.get("DEPTH").get.toInt).getOrElse(5)
  lazy val numSolutions: Int = Try(sys.env.get("SOLUTIONS").get.toInt).
    getOrElse(100)

  private[this] final case class EndSearchException(
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  def compilerBuilder =
    if (grounding) new MyGroundingCompiler(sizeHint)
    else new MyLiftedCompiler(sizeHint)

  override def foundSolution(circuit: NNFNode): Unit = {
    circuits = circuit :: circuits
    println("FOUND " + circuits.size + " SOLUTION(S)")
    if (circuits.size >= numSolutions)
      throw new EndSearchException
  }

  override def compile(cnf: CNF): List[NNFNode] = {
    val compiler = compilerBuilder
    val initialCircuit = compiler.applyGreedyRules(cnf)

    if (initialCircuit.formulas.isEmpty) {
      foundSolution(initialCircuit.circuit.get)
    } else {
      val q = Queue(initialCircuit)
      // val q = PriorityQueue(compiler.applyGreedyRules(cnf))(Ordering.by(_.priority))
      var depth = 0
      try {
        while (depth <= maxDepth && q.nonEmpty) {
          println("depth: " + depth)
          val partialCircuit = q.dequeue
          if (partialCircuit.depth > depth) {
            depth = partialCircuit.depth
          }
          if (depth <= maxDepth) {
            q ++= partialCircuit.nextCircuits(this)
          }
        }
      } catch {
        case e: EndSearchException => {}
      }
    }

    if (!circuits.isEmpty) {
      circuits
    } else {
      List(compiler.cannotCompile(cnf))
    }
  }

}
