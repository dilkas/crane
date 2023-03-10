package edu.ucla.cs.starai.forclift.compiler

import scala.util.Try

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.nnf.visitors._

object CompilerWrapper {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new CompilerWrapper(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new CompilerWrapper(sizeHint, true)

}

/** A convenient way to switch between greedy and breadth-first search. */
class CompilerWrapper(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    grounding: Boolean = false
) extends Compiler {

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
      println("Starting greedy search")
    } else {
      println("Starting breadth-first search")
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
