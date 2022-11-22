package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

import edu.ucla.cs.starai.forclift.nnf._

class OutputVisitor extends NnfVisitor[Unit, (String, List[String])] {

  var nextFunctionIndex = 1
  var nesxtVariableIndex = 1 // TODO: maybe replace with the number of input domains (+1?)

  def visit(node: NNFNode): (String, List[String]) =
    if (/* TODO: indegree > 1 */) {
      // TODO: add function arguments
      val functionName = "f_{" + nextFunctionIndex + "}"
      nextFunctionIndex++
      val (main, additional) = super.visit(node, ())
      (functionName, [functionName + " = " + main] + additional)
    } else {
      super.visit(node, ())
    }

  // ========================= NON-SINK NODES =================================

  protected def visitAndNode(and: And, u: Unit): (String, List[String]) = {
    val (main1, additional1) = visit(and.l.get)
    val (main2, additional2) = visit(and.r.get)
    (main1 + " * " + main2, additional1 + additional2)
  }

  protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode,
    u: Unit
  ): (String, List[String]) = visit(cr.child.get)

  // TODO: remove domain recursion
  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit): (String, List[String]) = {
    visit(dr.mixedChild.get)
    visit(dr.groundChild.get)
  }

  protected def visitExists(exists: CountingNode, u: Unit): (String, List[String]) = {
    val variableName = "v_{" + nesxtVariableIndex + "}"
    nesxtVariableIndex++
    val (main, additional) = visit(exists.child.get)
    ("sum_{" + variableName + " = 0}^{" + domain??? + "} " + main, additional)
  }

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      u: Unit
  ): (String, List[String]) = {
    // TODO: multiplication?
    visit(forall.child.get)
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      u: Unit
  ): (String, List[String]) = visit(idr.mixedChild.get)

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      u: Unit
  ): (String, List[String]) = {
    val (main1, additional1) = visit(ie.plus1.get)
    val (main2, additional2) = visit(ie.plus2.get)
    val (main3, additional3) = visit(ie.min.get)
    (main1 + " + " + main2 + " - " + main3, additional1 + additional2 + additional3)
  }

  protected def visitOrNode(or: Or, u: Unit): (String, List[String]) = {
    val (main1, additional1) = visit(or.l.get)
    val (main2, additional2) = visit(or.r.get)
    (main1 + " + " + main2, additional1 + additional2)
  }

  protected def visitRefNode(ref: Ref, u: Unit): (String, List[String]) = visit(ref.nnfNode.get)

  // ========================= SINK NODES =====================================

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit): (String, List[String]) = TODO

  protected def visitFalse(u: Unit): (String, List[String]) = ("0", [])

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): (String, List[String]) = TODO

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): (String, List[String]) = TODO

  protected def visitTrue(u: Unit): (String, List[String]) = ("1", [])

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): (String, List[String]) = TODO

}
