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

package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

import edu.ucla.cs.starai.forclift.nnf._

/** Initialises 'nodes' to be the set of all direct successors of Ref nodes.
  *
  * Needed by LatexOutputVisitor to know which nodes introduce new functions in
  * the algebraic description of the FCG.
  */
class FunctionIntroductionFinder extends NnfVisitor[Unit, Unit] {

  /** The set of direct successors of Ref nodes that is being constructed. */
  val nodes = Set[NNFNode]()

  /** The set of nodes encountered so far. */
  private[this] val visited = Set[NNFNode]()

  /** A wrapper method to avoid duplicate effort (as well as infinite loops). */
  def visit(node: NNFNode): Unit =
    if (!visited.contains(node)) {
      visited += node
      super.visit(node, ())
    }

  // ========================= NON-SINK NODES =================================

  protected def visitAndNode(and: And, u: Unit): Unit = {
    visit(and.l.get)
    visit(and.r.get)
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      u: Unit
  ): Unit = visit(cr.child.get)

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit): Unit = {
    visit(dr.mixedChild.get)
    visit(dr.groundChild.get)
  }

  protected def visitExists(exists: CountingNode, u: Unit): Unit =
    visit(exists.child.get)

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      u: Unit
  ): Unit = visit(forall.child.get)

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      u: Unit
  ): Unit = visit(idr.mixedChild.get)

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      u: Unit
  ): Unit = {
    visit(ie.plus1.get)
    visit(ie.plus2.get)
    visit(ie.min.get)
  }

  protected def visitOrNode(or: Or, u: Unit): Unit = {
    visit(or.l.get)
    visit(or.r.get)
  }

  protected def visitRefNode(ref: Ref, u: Unit): Unit = {
    val node = ref.nnfNode.get
    if (!nodes.contains(node)) {
      nodes += node
      visit(node)
    }
  }

  // ========================= SINK NODES =====================================

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit): Unit =
    ()

  protected def visitFalse(u: Unit): Unit = ()

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Unit = ()

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Unit = ()

  protected def visitTrue(u: Unit): Unit = ()

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Unit = ()

}
