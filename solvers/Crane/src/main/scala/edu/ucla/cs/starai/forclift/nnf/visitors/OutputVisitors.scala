package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.nnf._

/** Constructs a list of Strings, where each String is a definition of a
  * function in LaTeX syntax.
  *
  * @param initialDomains
  *   the set of domains that occur in the input formula
  * @param directSuccessorsOfRef
  *   the set of all nodes that are direct successors of some Ref node
  *   (equivalently, the nodes that have in-degree greater than one)
  *
  * NOTE: The support for predicate weights is incomplete. We need an algorithm
  * that computes the number of groundings of a clause just like nbGroundings
  * but returns an algebraic expression over domain sizes. The methods
  * visitSmoothingNode and visitUnitLeaf would then use this algorithm.
  *
  * Functions are named f_{0}, f_{1},... and variables (that denote domain
  * sizes) are named x_{0}, x_{1},...
  *
  * As input, this visitor carries a map that stores the algebraic
  * name/description of each domain size that has been introduced so far. In
  * some cases, that's just a variable name, e.g., x_{1}. In other cases, it
  * could be an expression such as ((x_{1} - x_{2}) - 1). This map needs to be
  * passed around as an input parameter because in some cases (when defining a
  * new function) we introduce temporary variable names to substitute for more
  * complex expressions.
  *
  * Each call to 'visit' returns a String and a List of Strings. The former is
  * an algebraic expression that can be inserted into any other algebraic
  * expression inside the definition of a function. The latter contains the list
  * of functions that were introduced while preparing the former.
  */
class LatexOutputVisitor(
    val initialDomains: Set[Domain],
    val directSuccessorsOfRef: Set[NNFNode],
    val predicateWeights: PredicateWeights
) extends NnfVisitor[(Map[Domain, String], PredicateWeights), (String, List[String])] {

  private[this] val functionNames = collection.mutable.Map[NNFNode, String]()
  private[this] var nextFunctionIndex = 0
  private[this] var nextVariableIndex = 0

  override def visit(
      node: NNFNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    if (directSuccessorsOfRef.contains(node)) {
      // Start the definition of a new function
      val functionName = newFunctionName(node)
      val newVariableNames = variableNames.map { case (domain, name) =>
        if (node.domains.contains(domain) && name.contains(" ")) {
          (domain, newVariableName())
        } else {
          (domain, name)
        }
      }
      val (expression, functions) = super.visit(node, (newVariableNames, predicateWeights))
      val functionCall = functionName + "(" + node.orderedDomains
        .map { variableNames(_) }
        .mkString(", ") + ")"
      val functionSignature = functionName + "(" + node.orderedDomains
        .map { newVariableNames(_) }
        .mkString(", ") + ")"
      (functionCall, (functionSignature + " = " + expression) :: functions)
    } else {
      super.visit(node, params)
    }
  }

  private def newFunctionName(node: NNFNode): String = {
    val name = s"f_{$nextFunctionIndex}"
    nextFunctionIndex += 1
    functionNames(node) = name
    name
  }

  private def newVariableName(): String = {
    val v = s"x_{$nextVariableIndex}"
    nextVariableIndex += 1
    v
  }

  // ========================= NON-SINK NODES =================================

  protected def visitAndNode(
      and: And,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(and.l.get, params)
    val (expression2, functions2) = visit(and.r.get, params)
    (s"$expression1 \\times $expression2", functions1 ++ functions2)
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    val newVariableNames = variableNames + (cr.subdomain -> ("(" + variableNames(cr.domain) + " - 1)"), cr.subdomain.complement -> "1")
    visit(cr.child.get, (newVariableNames, predicateWeights))
  }

  // We're not using regular domain recursion anyway, so it doesn't matter
  // what this method does
  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    visit(dr.mixedChild.get, params)
    visit(dr.groundChild.get, params)
  }

  protected def visitExists(
      exists: CountingNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    val n = variableNames(exists.domain)
    val m = newVariableName()
    val newVariableNames = variableNames + (exists.subdomain -> m, exists.subdomain.complement -> s"($n - $m)")
    val (expression, functions) = visit(exists.child.get, (newVariableNames, predicateWeights))
    (s"\\sum_{$m = 0}^{$n} \\binom{$n}{$m} \\times $expression", functions)
  }

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    val (expression, functions) = visit(forall.child.get, params)
    (s"$expression^{" + variableNames(forall.d) + "}", functions)
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = visit(idr.mixedChild.get, params)

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(ie.plus1.get, params)
    val (expression2, functions2) = visit(ie.plus2.get, params)
    val (expression3, functions3) = visit(ie.min.get, params)
    (
      s"($expression1 + $expression2 - $expression3)",
      functions1 ++ functions2 ++ functions3
    )
  }

  protected def visitOrNode(
      or: Or,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(or.l.get, params)
    val (expression2, functions2) = visit(or.r.get, params)
    (s"($expression1 + $expression2)", functions1 ++ functions2)
  }

  protected def visitRefNode(
      ref: Ref,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    (
      functionNames(ref.nnfNode.get) + "(" + ref.nnfNode.get.orderedDomains
        .map { ref.domainMap(_) }
        .map { variableNames(_) }
        .mkString(", ") + ")",
      Nil
    )
  }

  // ========================= SINK NODES =====================================

  /** Output [c <= variableNames(d) < c + v], where d is the unique domain, c is
    * the number of constants, and v is the number of variables.
    *
    * We assume that:
    *   1. There is only one domain. (Otherwise we would need to produce the
    *      disjunction of the domain size restrictions for each domain.)
    *   2. All variables are constrained to be unequal.
    *   3. All variables are constrained to be unequal to all constants (if any).
    */
  protected def visitContradictionLeaf(
      leaf: ContradictionLeaf,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    if (leaf.clause.domains.size != 1)
      throw new IllegalStateException(
        "Contradiction clauses with more than one domain are not supported (but support for them could easily be added if need-be)"
      )
    for (variable <- leaf.clause.constrVariables) {
      if (
        !leaf.clause.constrs
          .differentFrom(Set(variable))
          .equals(
            leaf.clause.constrs.constants
              .asInstanceOf[Set[Term]] | leaf.clause.constrVariables
              .asInstanceOf[Set[Term]] - variable.asInstanceOf[Term]
          )
      )
        throw new IllegalStateException(
          "The contradiction doesn't fit the expected format"
        )
    }
    (
      "[" + leaf.clause.constrs.constants.size + " \\le " + variableNames(
        leaf.clause.domains.head
      ) + " < " + (leaf.clause.constrs.constants.size + leaf.clause.constrVariables.size) + "]",
      Nil
    )
  }

  protected def visitFalse(
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = ("0", Nil)

  protected def visitGroundingNode(
      leaf: GroundingNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = throw new IllegalStateException(
    "Grounding is incompatible with OutputVisitors"
  )

  // A smoothing node always has one predicate
  protected def visitSmoothingNode(
      leaf: SmoothingNode,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    if (leaf.clause.constrs.ineqConstrs.isEmpty) {
      val (variableNames, predicateWeights) = params
      val weight = predicateWeights(leaf.clause.predicate).posWDouble + predicateWeights(leaf.clause.predicate).negWDouble
      (
        s"($weight)^{" + leaf.clause.literalVariables.map {
          case variable => variableNames(leaf.clause.domainsFor(Set(variable)).head)
        }.mkString(" \\times ") + "}",
        Nil
      )
    } else if (leaf.clause.domains.size == 1 && leaf.clause.constrs.constants.isEmpty && leaf.clause.allVarsAreDifferent) {
    } else {
    throw new IllegalStateException("This type of smoothing node is not supported")
    }
  }

  protected def visitTrue(
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = ("1", Nil)

  // There is significant overlap with visitSmoothingNode
  protected def visitUnitLeaf(
      leaf: UnitLeaf,
      params: (Map[Domain, String], PredicateWeights)
  ): (String, List[String]) = {
    val (variableNames, predicateWeights) = params
    val weight = if (leaf.positive) predicateWeights(leaf.clause.predicate).posWDouble else predicateWeights(leaf.clause.predicate).negWDouble
    if (approxEqual(weight, 1)) {
      ("1", Nil)
    } else {
      if (!leaf.clause.constrs.ineqConstrs.isEmpty)
        throw new IllegalStateException("Unit leaves with non-empty constraints and a predicate weight not equal to one are currently not supported")
      (
        s"($weight)^{" + leaf.clause.literalVariables.map {
          case variable => {
            val domains = leaf.clause.domainsFor(Set(variable))
            if (domains.size != 1)
              throw new IllegalStateException("If this exception is ever triggered, something with the definition of Clause or Constraints must be bugged")
            variableNames(domains.head)
          }
        }.mkString(" \\times ") + "}",
        Nil
      )
    }
  }

  private[this] def approxEqual(m: Double, n: Double): Boolean =
    (m - n).abs <= 0.001

}

object LatexOutputVisitor {

  def apply(
      initialDomains: Set[Domain],
      directSuccessorsOfRef: Set[NNFNode],
      predicateWeights: PredicateWeights,
      source: NNFNode
  ): List[String] = {
    val visitor = new LatexOutputVisitor(initialDomains, directSuccessorsOfRef, predicateWeights)
    val variableNames = Map(initialDomains.toSeq.zipWithIndex.map {
      case (d, i) => (d, visitor.newVariableName())
    }: _*)
    if (directSuccessorsOfRef.contains(source)) {
      visitor.visit(source, (variableNames, predicateWeights))._2
    } else {
      val functionName = visitor.newFunctionName(source)
      val (expression, functions) = visitor.visit(source, (variableNames, predicateWeights))
      (functionName + "(" + initialDomains
        .map { variableNames(_) }
        .mkString(", ") + ") = " + expression) :: functions
    }
  }

}
