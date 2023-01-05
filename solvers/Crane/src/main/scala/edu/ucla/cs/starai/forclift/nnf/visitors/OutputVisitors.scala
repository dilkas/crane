package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection._

import edu.ucla.cs.starai.forclift._
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
  * NOTE: There is no support for predicate weights as of yet, but it should be
  * easy to add.
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
    val directSuccessorsOfRef: Set[NNFNode]
) extends NnfVisitor[Map[Domain, String], (String, List[String])] {

  private[this] val functionNames = collection.mutable.Map[NNFNode, String]()
  private[this] var nextFunctionIndex = 0
  private[this] var nextVariableIndex = 0

  override def visit(
      node: NNFNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) =
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
      val (expression, functions) = super.visit(node, newVariableNames)
      val functionCall = functionName + "(" + node.orderedDomains
        .map { variableNames(_) }
        .mkString(", ") + ")"
      val functionSignature = functionName + "(" + node.orderedDomains
        .map { newVariableNames(_) }
        .mkString(", ") + ")"
      (functionCall, (functionSignature + " = " + expression) :: functions)
    } else {
      super.visit(node, variableNames)
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
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(and.l.get, variableNames)
    val (expression2, functions2) = visit(and.r.get, variableNames)
    (s"$expression1 \\times $expression2", functions1 ++ functions2)
  }

  protected def visitConstraintRemovalNode(
      cr: ConstraintRemovalNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = visit(
    cr.child.get,
    variableNames + (cr.subdomain -> ("(" + variableNames(cr.domain) + " - 1)"))
  )

  // We're not using regular domain recursion anyway, so it doesn't matter
  // what this method does
  protected def visitDomainRecursion(
      dr: DomainRecursionNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    visit(dr.mixedChild.get, variableNames)
    visit(dr.groundChild.get, variableNames)
  }

  protected def visitExists(
      exists: CountingNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    val n = variableNames(exists.domain)
    val m = newVariableName()
    val (expression, functions) = visit(
      exists.child.get,
      variableNames + (exists.subdomain -> m, exists.subdomain.complement -> s"($n - $m)")
    )
    (s"\\sum_{$m = 0}^{$n} \\binom{$n}{$m} \\times $expression", functions)
  }

  protected def visitForallNode(
      forall: IndependentPartialGroundingNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    val (expression, functions) = visit(forall.child.get, variableNames)
    (s"$expression^{" + variableNames(forall.d) + "}", functions)
  }

  protected def visitImprovedDomainRecursion(
      idr: ImprovedDomainRecursionNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = visit(idr.mixedChild.get, variableNames)

  protected def visitInclusionExclusionNode(
      ie: InclusionExclusion,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(ie.plus1.get, variableNames)
    val (expression2, functions2) = visit(ie.plus2.get, variableNames)
    val (expression3, functions3) = visit(ie.min.get, variableNames)
    (
      s"($expression1 + $expression2 - $expression3)",
      functions1 ++ functions2 ++ functions3
    )
  }

  protected def visitOrNode(
      or: Or,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
    val (expression1, functions1) = visit(or.l.get, variableNames)
    val (expression2, functions2) = visit(or.r.get, variableNames)
    (s"($expression1 + $expression2)", functions1 ++ functions2)
  }

  protected def visitRefNode(
      ref: Ref,
      variableNames: Map[Domain, String]
  ): (String, List[String]) =
    (
      functionNames(ref.nnfNode.get) + "(" + ref.nnfNode.get.orderedDomains
        .map { ref.domainMap(_) }
        .map { variableNames(_) }
        .mkString(", ") + ")",
      Nil
    )

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
      variableNames: Map[Domain, String]
  ): (String, List[String]) = {
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
      variableNames: Map[Domain, String]
  ): (String, List[String]) = ("0", Nil)

  protected def visitGroundingNode(
      leaf: GroundingNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = throw new IllegalStateException(
    "Grounding is incompatible with OutputVisitors"
  )

  protected def visitSmoothingNode(
      leaf: SmoothingNode,
      variableNames: Map[Domain, String]
  ): (String, List[String]) = ("1", Nil)

  protected def visitTrue(
      variableNames: Map[Domain, String]
  ): (String, List[String]) = ("1", Nil)

  protected def visitUnitLeaf(
      leaf: UnitLeaf,
      variableNames: Map[Domain, String]
  ): (String, List[String]) =
    if (
      !leaf.positive && leaf.clause.predicate.name.toString.startsWith("'sef_")
    ) {
      (
        "(-1)^{" + leaf.domains
          .map { variableNames(_) }
          .mkString(" \\times ") + "}",
        Nil
      )
    } else {
      ("1", Nil)
    }

}

object LatexOutputVisitor {

  def apply(
      initialDomains: Set[Domain],
      directSuccessorsOfRef: Set[NNFNode],
      source: NNFNode
  ): List[String] = {
    val visitor = new LatexOutputVisitor(initialDomains, directSuccessorsOfRef)
    val variableNames = Map(initialDomains.toSeq.zipWithIndex.map {
      case (d, i) => (d, visitor.newVariableName())
    }: _*)
    if (directSuccessorsOfRef.contains(source)) {
      visitor.visit(source, variableNames)._2
    } else {
      val functionName = visitor.newFunctionName(source)
      val (expression, functions) = visitor.visit(source, variableNames)
      (functionName + "(" + source.orderedDomains
        .map { variableNames(_) }
        .mkString(", ") + ") = " + expression) :: functions
    }
  }

}
