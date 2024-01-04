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
import edu.ucla.cs.starai.forclift.PositiveUnitClause

/** Updates the 'domains' field of all nodes in a way that avoids infinite loops
  * caused by cycles in the circuit.
  *
  * @param nodes
  *   all circuit nodes (as returned by PostOrderVisitor)
  *
  * Each visitor method returns 'true' if the 'domains' field of the input node
  * was updated and 'false' otherwise.
  */
class DomainsVisitor(val nodes: ListBuffer[NNFNode])
    extends NnfVisitor[Unit, Boolean] {

  /** Keeps iterating over all circuit nodes as long as at least one of them
    * updates its 'domains' field.
    */
  def updateDomains(): Unit = {
    var changesMade = true
    while (changesMade) {
      changesMade = false
      nodes.foreach(changesMade |= visit(_, ()))
    }
  }

  // ========================= NON-SINK NODES =================================

  // TODO (Paulius): reorder the methods (for other Visitors as well)

  protected def visitShatterNode(
      node: ShatterNode,
      u: Unit
  ): Boolean = {
    val newDomains = node.child.get.domains
    val returnValue = node.domains != newDomains
    node.domains = newDomains
    returnValue
  }

  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      u: Unit
  ): Boolean = {
    val newDomains = (dr.mixedChild.get.domains union
      dr.groundChild.get.domains + dr.domain)
    val returnValue = dr.domains != newDomains
    dr.domains = newDomains
    returnValue
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      u: Unit
  ): Boolean = {
    val newDomains = idr.mixedChild.get.domains + idr.domain
    val returnValue = idr.domains != newDomains
    idr.domains = newDomains
    returnValue
  }

  protected def visitExists(exists: CountingNode, u: Unit): Boolean = {
    val newDomains = (exists.child.get.domains - exists.subdomain -
      exists.subdomain.complement) + exists.domain
    val returnValue = exists.domains != newDomains
    exists.domains = newDomains
    returnValue
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      u: Unit
  ): Boolean = {
    val newDomains = cr.child.get.domains + cr.domain - cr.subdomain
    val returnValue = cr.domains != newDomains
    cr.domains = newDomains
    returnValue
  }

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      u: Unit
  ): Boolean = {
    val newDomains = forall.child.get.domains + forall.d
    val returnValue = forall.domains != newDomains
    forall.domains = forall.child.get.domains + forall.d
    returnValue
  }

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      u: Unit
  ): Boolean = {
    val newDomains = (ie.plus1.get.domains union ie.plus2.get.domains union
      ie.min.get.domains)
    val returnValue = ie.domains != newDomains
    ie.domains = newDomains
    returnValue
  }

  protected def visitOrNode(or: Or, u: Unit): Boolean = {
    val newDomains = or.l.get.domains union or.r.get.domains
    val returnValue = or.domains != newDomains
    or.domains = newDomains
    returnValue
  }

  protected def visitAndNode(and: And, u: Unit): Boolean = {
    val newDomains = and.l.get.domains union and.r.get.domains
    val returnValue = and.domains != newDomains
    and.domains = newDomains
    returnValue
  }

  protected def visitRefNode(ref: Ref, u: Unit): Boolean = {
    val newDomains = ref.domainMap.values.toSet
    val returnValue = ref.domains != newDomains
    ref.domains = newDomains
    returnValue
  }

  // ========================= SINK NODES =====================================

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Boolean =
    false

  protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      u: Unit
  ): Boolean = false

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Boolean = false

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Boolean =
    false

  protected def visitFalse(u: Unit): Boolean = false

  protected def visitTrue(u: Unit): Boolean = false

}
