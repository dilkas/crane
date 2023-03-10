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

package edu.ucla.cs.starai.forclift.nnf.visitors

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference
import scala.language.implicitConversions
import scala.util.Try

import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.propositional.C2DError
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift._

// ========================= OBJECTS & SUPERCLASSES ===========================

object NnfVisitorCache {

  var hit = 0;
  var miss = 0;

  @inline def addHit() {}
  @inline def addMiss() {}
  @inline def info() {}

  class Key(val node: NNFNode, val domainSizes: IndexedSeq[Int]) {

    override def equals(that: Any) = {
      if (that == null || !that.isInstanceOf[Key]) false
      else {
        val thatKey = that.asInstanceOf[Key]
        this.node == thatKey.node && this.domainSizes == thatKey.domainSizes
      }
    }

    override val hashCode = (node, domainSizes).hashCode

  }

}

trait WmcVisitor {

  /** Logs how numbers propagate through circuit nodes. */
  @inline protected final def log(s: => Any): Unit =
    if (WmcVisitor.Verbose) println(s)

  def wmc(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDouble
}

object WmcVisitor {

  /** A hacky way to turn a bunch of println statements on and off. */
  protected val Verbose = false

  /** This latch is reduced to zero as soon as one of the threads finishes
    * computing the weighted model count.
    *
    * Each visitor periodically checks the condition of the latch, and throws
    * an InterruptedException soon after it's set to zero.
    */
  val latch = new CountDownLatch(1)

  /** Should WMC computations be executed in parallel (with one thread for
    * every circuit) or sequentially?
    *
    * For greedy search, it should make no difference, but keeping it
    * sequential is more genuine. For breadth-first search, use parallel mode
    * if you want to find the answer as soon as possible and sequential mode
    * for debugging or otherwise investigative purposes. Another situation
    * where sequential mode makes more sense is if the breadth-first search
    * algorithm is set to return only one solution. NOTE: only LogDoubleWmc and
    * SignLogDoubleWmc have been extended with multithreading.
    */
  lazy val parallelMode: Boolean =  {
    val p = Try(sys.env.get("PARALLEL").get.toBoolean).getOrElse(false)
    if (p) {
      println("Finished search. Counting in parallel.")
    } else {
      println("Finished search. Counting sequentially.")
    }
    p
  }

  /** This reference will hold the value of the weighted model count as soon as
    * it's computed.
    */
  val wmc = new AtomicReference[SignLogDouble]

  def apply(predicateWeights: PredicateWeights): WmcVisitor = {
    val hasNegativeWeight = predicateWeights.values.exists(w =>
      w.negW < 0 ||
        w.posW < 0
    )

    // Caching needs to be disabled for cycles to make sense
    if (hasNegativeWeight) {
      new SignLogDoubleWmc
    } else {
      new LogDoubleWmc
    }
  }

}

/** All of the code related to multithreading.
  *
  * Type parameter O is the type that holds numbers during intermediate
  * computations, i.e., either LogDouble or SignLogDouble (the output type is
  * always SignLogDouble).
  */
trait MyRunnable[O]
    extends NnfVisitor[
      (DomainSizes, PredicateWeights),
      O
    ]
    with Runnable
    with WmcVisitor {

  /** The main (outer) function called to compute the WMC.
    *
    * Implements the WmcVisitor trait. If running in parallel mode, creates a
    * thread for each circuit. The main thread then just waits for one of the
    * auxiliary threads to finish its computations. If in sequential mode,
    * computes the WMC for all circuits and returns the first result (which is
    * of the circuit that was found last). We use all circuits for sequential
    * inference so that their answers and running times could be compared.
    */
  def wmc(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDouble =
    if (WmcVisitor.parallelMode) {
      val executor: ExecutorService = Executors.newFixedThreadPool(nnfs.size)
      nnfs.foreach(nnf =>
        executor.execute(simpleClone(nnf, domainSizes, predicateWeights))
      )
      try {
        WmcVisitor.latch.await
        executor.shutdownNow
      } catch {
        case e: InterruptedException => println(e)
      }
      WmcVisitor.wmc.get
    } else {
      println("Computing " + nnfs.size + " answer(s)\n")
      val answers = nnfs.map {
        nnf => {
          val answer = Timer {
            visitWrapper(
              nnf,
              (domainSizes, predicateWeights)
            )
          }("Circuit evaluation took " + _ + " ms")
          println("Answer: " + answer.toDouble)
          answer
        }
      }
      answers.head
    }

  /** The beginning of a WMC-computing thread.
    *
    * Starts computing the WMC, waits for computations to finish, sets the WMC
    * as a thread-safe field, and signals all other threads to stop via a
    * latch. Implements Runnable.
    */
  def run {
    try {
      val wmc = runMe()
      WmcVisitor.wmc.set(wmc)
      WmcVisitor.latch.countDown
    } catch {
      case e: InterruptedException => {}
    }
  }

  /** Serves the same function as run() but is subclass-specific so that it can
    * make use of fields.
    */
  def runMe(): SignLogDouble

  /** Creates a new instance of the (sub)class, with all fields replaced by the
    * arguments.
    */
  def simpleClone(
      circuit: NNFNode,
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): MyRunnable[O]

  /** A wrapper for the visit() method that implicitly converts LogDouble to
    * SignLogDouble.
    */
  def visitWrapper(
      node: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble

}

// ========================= ACTIVELY USED VISITORS ===========================

protected class LogDoubleWmc(
    val circuit: NNFNode = null,
    val domainSizes: DomainSizes = null,
    val predicateWeights: PredicateWeights = null
) extends MyRunnable[LogDouble] {

  import edu.ucla.cs.starai.forclift.util.LogDouble._

  def runMe(): SignLogDouble =
    visit(
      circuit,
      (domainSizes, predicateWeights)
    )

  def simpleClone(
      circuit: NNFNode,
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): LogDoubleWmc =
    new LogDoubleWmc(circuit, domainSizes, predicateWeights)

  def visitWrapper(
      node: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble =
    visit(node, params)

  protected def visitAndNode(
      and: And,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      val answer = llwmcc * rlwmcc
      log(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) {
      log("0 (constraint removal, base case)")
      zero
    } else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1)
      val child = visit(
        cr.child.get,
        (
          newDomainSizes,
          predicateWeights
        )
      )
      log(s"$child (constraint removal)")
      child
    }
  }

  protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      val power = (maxSize * (maxSize - 1)) / 2
      val answer = groundChildWmc.pow(maxSize) * childchildWmc.pow(power)
      log(
        s"$groundChildWmc ^ $maxSize * $childchildWmc ^ $power = $answer (domain recursion)"
      )
      answer
    }
  }

  protected def visitExists(
      exists: CountingNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero
    log("exists/counting:")
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement,
        (maxSize - nbTrue)))
      val newParams = (
        newDomainSizes,
        predicateWeights
      )
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
      log(s" + $maxSize C $nbTrue * $childWeight")
    }
    log(s"= $logWeight\n")
    logWeight
  }

  protected def visitFalse(
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = zero

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      log(
        s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)"
      )
      answer
    }
  }

  protected def visitGroundingNode(
      leaf: GroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc.toLogDouble
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      log("1 (improved domain recursion, base case)")
      one
    } else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      log(s"$childchildWmc (improved domain recursion)")
      childchildWmc
    }
  }

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    val answer = plus1lwmc + plus2lwmc - minlwmc
    log(s"$plus1lwmc + $plus2lwmc - $minlwmc = $answer (inclusion-exclusion)")
    answer
  }

  protected def visitOrNode(
      or: Or,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    val answer = llwmcc + rlwmcc
    log(s"$llwmcc + $rlwmcc = $answer (or)")
    answer
  }

  protected def visitRefNode(
      ref: Ref,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap)
    val answer =
      visit(ref.nnfNode.get, (newDomainSizes, predicateWeights))
    log(s"$answer (ref)")
    answer
  }

  protected def visitSmoothingNode(
      leaf: SmoothingNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    val weight = weights.negWPlusPosWLogDouble
    val answer = weight.pow(nbGroundings)
    log(s"$weight ^ $nbGroundings = $answer (smoothing for " + leaf.cnf + ")")
    answer
  }

  protected def visitTrue(
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = one

  protected def visitUnitLeaf(
      leaf: UnitLeaf,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) {
      one
    } else if (leaf.positive) {
      val weight = weights.posWLogDouble
      val answer = weight.pow(nbGroundings)
      log(s"$weight ^ $nbGroundings = $answer (positive leaf)")
      answer
    } else {
      val weight = weights.negWLogDouble
      val answer = weight.pow(nbGroundings)
      log(s"$weight ^ $nbGroundings = $answer (negative leaf)")
      answer
    }
  }

}

protected class SignLogDoubleWmc(
    val circuit: NNFNode = null,
    val domainSizes: DomainSizes = null,
    val predicateWeights: PredicateWeights = null
) extends MyRunnable[SignLogDouble] {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._

  def runMe(): SignLogDouble = {
    visit(
      circuit,
      (domainSizes, predicateWeights)
    )
  }

  def simpleClone(
      circuit: NNFNode,
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDoubleWmc =
    new SignLogDoubleWmc(circuit, domainSizes, predicateWeights)

  def visitWrapper(
      node: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble =
    visit(node, params)

  protected def visitAndNode(
      and: And,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      val answer = llwmcc * rlwmcc
      log(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) {
      log("0 (constraint removal, base case)")
      zero
    } else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1) +
        (cr.subdomain.complement, 1)
      val child = visit(
        cr.child.get,
        (
          newDomainSizes,
          predicateWeights
        )
      )
      log(s"$child (constraint removal)")
      child
    }
  }

  protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      val power = (maxSize * (maxSize - 1)) / 2
      val answer = groundChildWmc.pow(maxSize) * childchildWmc.pow(power)
      log(
        s"$groundChildWmc ^ $maxSize * $childchildWmc ^ $power = $answer (domain recursion)"
      )
      answer
    }
  }

  protected def visitExists(
      exists: CountingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement,
        (maxSize - nbTrue)));
      val newParams = (
        newDomainSizes,
        predicateWeights
      )
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue).toSignDouble
      logWeight += binomialCoeff * childWeight
      log(s" + $maxSize C $nbTrue * $childWeight")
    }
    log(s"= $logWeight\n")
    logWeight
  }

  protected def visitFalse(
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = zero

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      log(
        s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)"
      )
      answer
    }
  }

  protected def visitGroundingNode(
      leaf: GroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      log("1 (improved domain recursion, base case)")
      one
    } else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      log(s"$childchildWmc (improved domain recursion)")
      childchildWmc
    }
  }

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    val answer = plus1lwmc + plus2lwmc - minlwmc
    log(s"$plus1lwmc + $plus2lwmc - $minlwmc = $answer (inclusion-exclusion)")
    answer
  }

  protected def visitOrNode(
      or: Or,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    val answer = llwmcc + rlwmcc
    log(s"$llwmcc + $rlwmcc = $answer (or)")
    answer
  }

  protected def visitRefNode(
      ref: Ref,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap)
    val answer =
      visit(ref.nnfNode.get, (newDomainSizes, predicateWeights))
    log(s"$answer (ref)")
    answer
  }

  protected def visitSmoothingNode(
      leaf: SmoothingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    val weight = weights.negWPlusPosW
    val answer = weight.pow(nbGroundings)
    log(s"$weight ^ $nbGroundings = $answer (smoothing for " + leaf.cnf + ")")
    answer
  }

  protected def visitTrue(
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = one

  protected def visitUnitLeaf(
      leaf: UnitLeaf,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) {
      one
    } else if (leaf.positive) {
      val weight = weights.posW
      val answer = weight.pow(nbGroundings)
      log(s"$weight ^ $nbGroundings = $answer (positive leaf)")
      answer
    } else {
      val weight = weights.negW
      val answer = weight.pow(nbGroundings)
      log(s"$weight ^ $nbGroundings = $answer (negative leaf)")
      answer
    }
  }

}

// ========================= CACHING VISITORS =================================

protected class CachingLogDoubleWmc extends LogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.LogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, LogDouble]

  override def wmc(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDouble = {
    cache.clear()
    super.wmc(nnfs, domainSizes, predicateWeights)
  }

  // only decomposition nodes can reduce the number of relevant domains!

  override protected def visitAndNode(
      and: And,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val llwmcc = retrieveWmc(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r.get, params)
      val answer = llwmcc * rlwmcc
      log(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  override protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = retrieveWmc(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      log(
        s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)"
      )
      answer
    }
  }

  @inline private def retrieveWmc(
      node: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): LogDouble = {
    if (node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1){
      return visit(node, params)
    } else {
      val domains: IndexedSeq[Domain] = node.orderedDomains
      val (domainSizes, predicateWeights) = params
      val key = new Key(node, domains.map(domainSizes(_).size))
      val result = cache.get(key)
      if (result.nonEmpty) {
        addHit()
        result.get
      } else {
        addMiss()
        val childWeight = visit(node, params)
        cache.update(key, childWeight)
        childWeight
      }
    }
  }

}

protected class CachingSignLogDoubleWmc extends SignLogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, SignLogDouble]

  override def wmc(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDouble = {
    cache.clear()
    super.wmc(nnfs, domainSizes, predicateWeights)
  }

  // only decomposition nodes can reduce the number of relevant domains!

  override protected def visitAndNode(
      and: And,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val llwmcc = retrieveWmc(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r.get, params)
      val answer = llwmcc * rlwmcc
      log(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  override protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = retrieveWmc(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      log(
        s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)"
      )
      answer
    }
  }

  @inline private def retrieveWmc(
      node: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    if (node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1)
      return visit(node, params)
    } else {
      val domains: IndexedSeq[Domain] = node.orderedDomains
      val (domainSizes, predicateWeights) = params
      val key = new Key(node, domains.map(domainSizes(_).size))
      val result = cache.get(key)
      if (result.nonEmpty) {
        addHit()
        result.get
      } else {
        addMiss()
        val childWeight = visit(node, params)
        cache.update(key, childWeight)
        childWeight
      }
    }
  }

}

// ========================= OTHER VISITORS ===================================

class SafeSignLogDoubleWmc extends SignLogDoubleWmc {

  import SignLogDouble._

  override protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(cr.domain)) {
      super.visitConstraintRemovalNode(cr, params)
    } else NaN
  }

  override protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitContradictionLeaf(leaf, params)
    } else NaN
  }

  override protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ) = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(dr.domain)) {
      super.visitDomainRecursion(dr, params)
    } else NaN
  }

  override protected def visitExists(
      exists: CountingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(exists.domain)) {
      super.visitExists(exists, params)
    } else NaN
  }

  override protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(forall.d)) {
      super.visitForallNode(forall, params)
    } else NaN
  }

  override protected def visitGroundingNode(
      leaf: GroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.cnf.domains.forall { domainSizes.contains(_) }) {
      super.visitGroundingNode(leaf, params)
    } else NaN
  }

  override protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ) = {
    val (domainSizes, predicateWeights) = params
    if (domainSizes.contains(idr.domain)) {
      super.visitImprovedDomainRecursion(idr, params)
    } else NaN
  }

  override protected def visitSmoothingNode(
      leaf: SmoothingNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitSmoothingNode(leaf, params)
    } else NaN
  }

  override protected def visitUnitLeaf(
      leaf: UnitLeaf,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val (domainSizes, predicateWeights) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitUnitLeaf(leaf, params)
    } else NaN
  }

}

/** Compute WMC while also verifying the counts of every intermediate step
  * using C2D
  */

object VerifyWmcVisitor {

  class VerificationFailedException extends Exception

  def verify(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ) {
    require(nnfs.size == 1)
    (new VerifyWmcVisitor()).visit(
      nnfs.head,
      (domainSizes, predicateWeights)
    )
  }

}

protected class VerifyWmcVisitor extends SignLogDoubleWmc {

  val nonVerifyingWmcVisitor = new SignLogDoubleWmc

  override def visit(
      nnfNode: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ): SignLogDouble = {
    val wmc = super.visit(nnfNode, params)
    verifyLocal(nnfNode, params)
    wmc
  }

  protected def verifyLocal(
      nnfNode: NNFNode,
      params: (DomainSizes, PredicateWeights)
  ) {
    val (domainSizes, predicateWeights) = params
    try {
      val cnf = nnfNode.cnf
      val weightedCNF = WeightedCNF(cnf, domainSizes, predicateWeights)
      // ground truth (pun intended)
      val groundCNF = cnf.ground(domainSizes)
      // smooth with everything appearing in the ground cnf, not with every
      // predicate grounding!!
      val atomsInPropWmc = groundCNF.atoms.map {
        new PositiveUnitClause(_, Constraints.empty)
      }
      val thisSmooth = nnfNode.smooth
      // not used -- debugging purposes only
      val onceSmoothedLiftedLogWmc =
        nonVerifyingWmcVisitor.visit(thisSmooth, params)
      // must also ground before subtracting, otherwise constants will unify
      // with empty domain variables
      val atomsInLiftedWmc = nnfNode.variablesForSmoothing.flatMap {
        _.ground(domainSizes).map { _.toPositiveUnitClause }
      }
      val atomsMissingFromLiftedWmc = atomsInPropWmc.flatMap {
        _.minus(atomsInLiftedWmc)
      }
      val atomsMissingFromPropWmc = atomsInLiftedWmc.flatMap {
        _.minus(atomsInPropWmc)
      }

      val thisTwiceSmoothed = thisSmooth.smoothWith(atomsMissingFromLiftedWmc)
      val twiceSmoothedLiftedLogWmc =
        nonVerifyingWmcVisitor.visit(thisTwiceSmoothed, params)

      val weightsMissingFromPropWmc = atomsMissingFromPropWmc.toList.map {
        clause =>
          nonVerifyingWmcVisitor.visit((new SmoothingNode(clause)), params)
      }
      val propLogWmc = weightedCNF.logPropWmc
      val twiceSmoothedPropLogWmc = {
        weightsMissingFromPropWmc.foldLeft(propLogWmc) { _ * _ }
      }

      val correct = ((twiceSmoothedPropLogWmc == twiceSmoothedLiftedLogWmc)
        || twiceSmoothedPropLogWmc.isZero && twiceSmoothedLiftedLogWmc.isZero
        || (twiceSmoothedPropLogWmc.logToDouble -
          twiceSmoothedLiftedLogWmc.logToDouble).abs < 0.0000001
        || (twiceSmoothedPropLogWmc -
          twiceSmoothedLiftedLogWmc).abs.logToDouble < 0.0000001)
      if (!correct) {
        println("WMC:")
        println(weightedCNF)
        println("Ground CNF:")
        println(weightedCNF.groundCnf)
        println("correct wmc = " + twiceSmoothedPropLogWmc.exp)
        println("our wmc = " + twiceSmoothedLiftedLogWmc.exp)
        println("test wmc = " + onceSmoothedLiftedLogWmc.exp)
        println("debug")
        thisTwiceSmoothed.showPDF(
          domainSizes,
          predicateWeights,
          false,
          file = "bug.nnf"
        )
        throw new VerifyWmcVisitor.VerificationFailedException
      }

    } catch {
      case c2d: C2DError => {
        // C2D refuses: can't verify this node.
      }
    }
  }

}

/**
  * Goes out of memory
  */
protected class BigIntWmc(val decimalPrecision: Int = 100)
    extends NnfVisitor[
      (DomainSizes, PredicateWeights),
      BigInt
    ]
    with WmcVisitor {

  val LOG2 = Math.log(2.0)
  val one: BigInt = 1
  val zero: BigInt = 0

  // special cache for bigint factorials (cf. Binomial class)
  private[this] val factorialCache =
    new collection.mutable.ArrayBuffer[BigInt] ++ List(one, one)

  // see http://stackoverflow.com/questions/6827516/logarithm-for-BigInt
  def bigInt2SignLogDouble(bigint: BigInt): SignLogDouble = {
    var v = bigint
    val blex = v.bitLength - 1022; // any value in 60..1023 is ok
    if (blex > 0)
      v = v >> blex;
    val res = Math.log(v.doubleValue());
    return SignLogDouble.fromLog(if (blex > 0) res + blex * LOG2 else res);
  }

  def coeff(n: Int, k: Int): BigInt =
    factorial(n) / factorial(k) / factorial(n - k)

  def factorial(n: Int): BigInt = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def numRandVars(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): Int = {
    predicateWeights.predicates.map { _.toAtom.nbGroundings(domainSizes) }.sum
  }

  def wmc(
      nnfs: List[NNFNode],
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights
  ): SignLogDouble = {
    val normalization: LogDouble = LogDouble
      .doubleToLogDouble(decimalPrecision)
      .pow(numRandVars(domainSizes, predicateWeights))
    bigInt2SignLogDouble(
      visit(
        nnfs.head,
        (domainSizes, predicateWeights)
      )
    ) / normalization
  }

  protected def visitAndNode(
      and: And,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc == zero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      llwmcc * rlwmcc
    }
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) zero
    else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1) +
        (cr.subdomain.complement, 1)
      val child = visit(
        cr.child.get,
        (
          newDomainSizes,
          predicateWeights
        )
      )
      child
    }
  }

  protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      groundChildWmc.pow(maxSize) *
        childchildWmc.pow((maxSize * (maxSize - 1)) / 2)
    }
  }

  protected def visitExists(
      exists: CountingNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
        + (exists.subdomain, nbTrue)
        + (exists.subdomain.complement,
        (maxSize - nbTrue)));
      val newParams = (
        newDomainSizes,
        predicateWeights
      )
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
    }
    logWeight
  }

  protected def visitFalse(
      params: (DomainSizes, PredicateWeights)
  ): BigInt = zero

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  protected def visitGroundingNode(
      leaf: GroundingNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    throw new UnsupportedOperationException
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      one
    } else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      childchildWmc
    }
  }

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    plus1lwmc + plus2lwmc - minlwmc
  }

  protected def visitOrNode(
      or: Or,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    llwmcc + rlwmcc
  }

  protected def visitRefNode(
      ref: Ref,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap)
    val answer =
      visit(ref.nnfNode.get, (newDomainSizes, predicateWeights))
    answer
  }

  protected def visitSmoothingNode(
      leaf: SmoothingNode,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    BigInt((decimalPrecision * weights.negWPlusPosWDouble).toInt)
      .pow(nbGroundings)
  }

  protected def visitTrue(
      params: (DomainSizes, PredicateWeights)
  ): BigInt = one

  protected def visitUnitLeaf(
      leaf: UnitLeaf,
      params: (DomainSizes, PredicateWeights)
  ): BigInt = {
    val (domainSizes, predicateWeights) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) one
    else if (leaf.positive)
      BigInt((decimalPrecision * weights.posWDouble).toInt).pow(nbGroundings)
    else BigInt((decimalPrecision * weights.negWDouble).toInt).pow(nbGroundings)
  }

}
