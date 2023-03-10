package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.PositiveUnitClause

/** Updates the 'variablesForSmoothing' field of all nodes in a way that avoids
  * infinite loops caused by cycles in the circuit.
  *
  * @param nodes all circuit nodes (as returned by PostOrderVisitor)
  *
  * Each visitor method returns 'true' if the 'variablesForSmoothing' field of
  * the input node was updated and 'false' otherwise.
  */
class SmoothingVariablesVisitor(val nodes: ListBuffer[NNFNode])
    extends NnfVisitor[Unit, Boolean] {

  /** A hacky way to turn a bunch of println statements on and off. */
  private[this] val Verbose = true

  @inline private[this] final def log(s: => Any): Unit =
    if (Verbose) println(s)

  /** Keeps iterating over all circuit nodes as long as at least one of them
    * updates its 'variablesForSmoothing' field.
    */
  def updateVariables: Unit = {
    var changesMade = true
    log("Smoothing started")
    while (changesMade) {
      changesMade = false
      log("Starting a new round of smoothing")
      nodes.foreach(changesMade |= visit(_, ()))
    }
    log("Finished smoothing")
  }

  // ========================= NON-SINK NODES =================================

  protected def visitAndNode(and: And, u: Unit): Boolean = {
    val thisVars: collection.Set[PositiveUnitClause] =
      and.l.get.variablesForSmoothing union and.r.get.variablesForSmoothing
    val returnValue = and.variablesForSmoothing != thisVars

    log(
      "and: " + returnValue + ". before: " + and.variablesForSmoothing +
        ", after: " + thisVars + ". Hash codes equal: " +
        (and.variablesForSmoothing.hashCode == thisVars.hashCode)
    )

    and.variablesForSmoothing = thisVars
    returnValue
  }

  protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode,
    u: Unit
  ): Boolean = {
    val countedSubdomainParents =
      NNFNode.removeSubsumed(cr.child.get.variablesForSmoothing.map {
                               _.reverseDomainSplitting(cr.domain, cr.subdomain)
                             })
    val returnValue = cr.variablesForSmoothing != countedSubdomainParents
    cr.variablesForSmoothing = countedSubdomainParents
    log("constraint removal: " + returnValue)
    returnValue
  }

  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      u: Unit
  ): Boolean = {
    val ungroundedMixedChildvars = dr.mixedChild.get.variablesForSmoothing.map {
      _.inverseSubstitution(dr.c, dr.ineqs, dr.domain)
    }
    val ungroundedGroundChildVars =
      dr.groundChild.get.variablesForSmoothing.map {
        _.inverseSubstitution(dr.c, dr.ineqs, dr.domain)
      }
    val allVars = ungroundedMixedChildvars ++ ungroundedGroundChildVars
    val returnValue = dr.variablesForSmoothing != allVars
    dr.variablesForSmoothing = allVars
    log("domain recursion: " + returnValue)
    returnValue
  }

  protected def visitExists(exists: CountingNode, u: Unit): Boolean = {
    val countedSubdomainParents =
      NNFNode.removeSubsumed(exists.child.get.variablesForSmoothing.map {
        _.reverseDomainSplitting(exists.domain, exists.subdomain)
      })
    val returnValue = exists.variablesForSmoothing != countedSubdomainParents
    exists.variablesForSmoothing = countedSubdomainParents

    log(
      "exists/counting: " + returnValue + ". before: " +
        exists.variablesForSmoothing + ", after: " +
        countedSubdomainParents + "."
    )

    returnValue
  }

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      u: Unit
  ): Boolean = {
    val ungroundedChildVars = forall.child.get.variablesForSmoothing.map {
      _.inverseSubstitution(forall.c, forall.ineqs, forall.d)
    }

    val returnValue = forall.variablesForSmoothing != ungroundedChildVars
    forall.variablesForSmoothing = ungroundedChildVars
    log("forall / independent partial grounding: " + returnValue)
    returnValue
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      u: Unit
  ): Boolean = {
    val allVars = idr.mixedChild.get.variablesForSmoothing.map {
      _.inverseSubstitution(idr.c, idr.ineqs, idr.domain)
    }
    val returnValue = idr.variablesForSmoothing != allVars

    if (returnValue) {
      log(
        "visitImprovedDomainRecursion: the child is " +
          idr.mixedChild.getClass.getSimpleName
      )
      log(
        "visitImprovedDomainRecursion: before the transformation: " +
          idr.mixedChild.get.variablesForSmoothing
      )
      log(
        "visitImprovedDomainRecursion: after the transformation: " +
          allVars
      )
      log(
        "visitImprovedDomainRecursion: replacing " +
          idr.variablesForSmoothing + " with " + allVars
      )
    }

    idr.variablesForSmoothing = allVars
    returnValue
  }

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      u: Unit
  ): Boolean = {
    val plus1Missing = NNFNode
      .removeSubsumed(
        ie.plus2.get.variablesForSmoothing union
          ie.min.get.variablesForSmoothing
      )
      .flatMap {
        _.minus(ie.plus1.get.variablesForSmoothing)
      }
    val plus2Missing = NNFNode
      .removeSubsumed(
        ie.plus1.get.variablesForSmoothing union
          ie.min.get.variablesForSmoothing
      )
      .flatMap {
        _.minus(ie.plus2.get.variablesForSmoothing)
      }
    val minMissing = NNFNode
      .removeSubsumed(
        ie.plus1.get.variablesForSmoothing union
          ie.plus2.get.variablesForSmoothing
      )
      .flatMap {
        _.minus(ie.min.get.variablesForSmoothing)
      }
    val plus1VarsAll = ie.plus1.get.variablesForSmoothing union plus1Missing
    val plus2VarsAll = ie.plus2.get.variablesForSmoothing union plus2Missing
    val minVarsAll = ie.min.get.variablesForSmoothing union minMissing
    val bestOf2 =
      if (plus1VarsAll.size < plus2VarsAll.size) plus1VarsAll
      else plus2VarsAll
    val bestOf3 = if (minVarsAll.size < bestOf2.size) minVarsAll else bestOf2
    val returnValue = ie.variablesForSmoothing != bestOf3
    ie.variablesForSmoothing = bestOf3

    log("inclusion-exclusion: " + returnValue)

    returnValue
  }

  protected def visitOrNode(or: Or, u: Unit): Boolean = {
    val lMissing = or.r.get.variablesForSmoothing.flatMap {
      _.minus(or.l.get.variablesForSmoothing)
    }
    val rMissing = or.l.get.variablesForSmoothing.flatMap {
      _.minus(or.r.get.variablesForSmoothing)
    }
    val lVarsAll = or.l.get.variablesForSmoothing union lMissing
    val rVarsAll = or.r.get.variablesForSmoothing union rMissing
    val thisVars = if (lVarsAll.size < rVarsAll.size) lVarsAll else rVarsAll
    val returnValue = or.variablesForSmoothing != thisVars
    or.variablesForSmoothing = thisVars

    log("or: " + returnValue)

    returnValue
  }

  protected def visitRefNode(ref: Ref, u: Unit): Boolean = {
    val returnValue = ref.variablesForSmoothing !=
      ref.nnfNode.get.variablesForSmoothing

    log(
      "ref: " + returnValue + ". before: " + ref.variablesForSmoothing +
        ", after: " + ref.nnfNode.get.variablesForSmoothing +
        ". Hash codes equal: " +
        (ref.variablesForSmoothing.hashCode ==
          ref.nnfNode.get.variablesForSmoothing.hashCode)
    )

    ref.variablesForSmoothing = ref.nnfNode.get.variablesForSmoothing
    returnValue
  }

  // ========================= SINK NODES =====================================

  protected def visitContradictionLeaf(
    leaf: ContradictionLeaf,
    u: Unit
  ): Boolean = false

  protected def visitFalse(u: Unit): Boolean = false

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Boolean =
    false

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Boolean =
    false

  protected def visitTrue(u: Unit): Boolean = false

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Boolean = false

}
