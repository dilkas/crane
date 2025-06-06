/*
 * Copyright 2025 Paulius Dilkas (University of Toronto)
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

package edu.ucla.cs.starai.forclift.nnf

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import scala.sys.process._
import System._
import collection.mutable.ListBuffer
import breeze.math._
import edu.ucla.cs.starai.forclift.nnf.visitors.SafeSignLogDoubleWmc
import edu.ucla.cs.starai.forclift.nnf.visitors.WmcVisitor

class And(
    val cnf: CNF,
    var l: Option[NNFNode],
    var r: Option[NNFNode],
    val explanation: String = "",
    var domains: Set[Domain] = Set()
) extends NNFNode {

  override def directSuccessors = List(l, r)

  def simpleClone(): NNFNode = new And(cnf, None, None, explanation, domains)

  def size = l.get.size + r.get.size + 1

  lazy val evalOrder = {
    val rOrder = r.get.evalOrder
    val lOrder = l.get.evalOrder
    rOrder max lOrder
  }

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 2)
    l = children.head
    r = children.tail.head
  }

  override def updateFirst(child: NNFNode) =
    if (l.isEmpty) {
      l = Some(child)
      true
    } else if (r.isEmpty) {
      r = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new And(
      cnf,
      Some(l.get.condition(pos, neg)),
      Some(r.get.condition(pos, neg)),
      explanation,
      domains
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new And(cnf, None, None, explanation, domains)
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(Some(l.get.smooth), Some(r.get.smooth)))
    newNode
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = l.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val (nr, er) = r.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val myNodes = if (compact) {
        "  " + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" +
          "  " + "and" + getName(
            nameSpace
          ) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + l.get.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + r.get.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val llwmc =
          try {
            wmcVisitor.visit(
              l.get.smooth,
              (domainSizes, predicateWeights)
            )
          } catch {
            case e: UnsupportedOperationException => Complex.nan
          }
        val rlwmc =
          try {
            wmcVisitor.visit(
              r.get.smooth,
              (domainSizes, predicateWeights)
            )
          } catch {
            case e: UnsupportedOperationException => Complex.nan
          }
        "  " + getName(nameSpace) + " -> " + "and" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "and" + getName(nameSpace) + " -> " + l.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel(" $ " + llwmc + " $ ") + """];""" + "\n" +
          "  " + "and" + getName(nameSpace) + " -> " + r.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel(" $ " + rlwmc + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl + nr)
      val edges = (myEdges + el + er)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + l.get.getName(nameSpace) + " Λ " + r.get
        .getName(nameSpace) + "\n" +
      "\n" +
      l.get.toString(nameSpace) +
      "\n" +
      r.get.toString(nameSpace))

}

/** Constraints of the form 'v != c' are removed by introducing a new domain
  * without c.
  *
  * @param child
  *   the first node of the subscircuit for the remaining formula (after
  *   constraint removal, etc.)
  * @param domain
  *   the domain that's being replaced
  * @param subdomain
  *   what it's being replaced with
  */
class ConstraintRemovalNode(
    val cnf: CNF,
    var child: Option[NNFNode],
    val domain: Domain,
    val subdomain: SubDomain,
    val constant: Constant,
    val explanation: String = ""
) extends NNFNode {

  // ========================= ONE-LINERS =====================================

  override def directSuccessors = List(child)

  var domains: Set[Domain] = Set(domain)

  lazy val evalOrder = child.get.evalOrder + 1

  def simpleClone(): NNFNode = {
    val n = new ConstraintRemovalNode(
      cnf,
      None,
      domain,
      subdomain,
      constant,
      explanation
    )
    n.domains = domains
    n
  }

  def size = child.get.size + 1

  // ========================= EVERYTHING ELSE ================================

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 1)
    child = children.head
  }

  override def updateFirst(newChild: NNFNode) =
    if (child.isEmpty) {
      child = Some(newChild)
      true
    } else {
      false
    }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new ConstraintRemovalNode(
      cnf,
      None,
      domain,
      subdomain,
      constant,
      explanation
    )
    newNode.domains = domains
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(Some(child.get.smooth)))
    newNode
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new ConstraintRemovalNode(
      cnf,
      Some(child.get.condition(pos, neg)),
      domain,
      subdomain,
      constant,
      explanation
    )
    returnValue.domains = domains
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $""" +
          subdomain + """ = """ + domain +
          """\setminus\{\,""" + constant + """\,\}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ +
          cnf.toLatex() + """"];""" + "\n" + "  " + "reduce" +
          getName(nameSpace) + """ [texlbl="""" + fontsize + """ $""" +
          subdomain + """ = """ + domain +
          """\setminus\{\,""" + constant + """\,\}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(nameSpace) +
          ";\n"
      } else {
        "  " + getName(nameSpace) + " -> " + "count" + getName(nameSpace) +
          """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "count" + getName(nameSpace) + " -> " +
          child.get.getName(nameSpace) + ";\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(
        nameSpace
      ) + " = reduce " + domain + " to " + subdomain)

}

class InclusionExclusion(
    val cnf: CNF,
    var plus1: Option[NNFNode],
    var plus2: Option[NNFNode],
    var min: Option[NNFNode],
    val explanation: String = "",
    var domains: Set[Domain] = Set()
) extends NNFNode {

  override def directSuccessors = List(plus1, plus2, min)

  def simpleClone(): NNFNode =
    new InclusionExclusion(cnf, None, None, None, explanation, domains)

  def size = plus1.get.size + plus2.get.size + min.get.size + 1

  lazy val evalOrder = {
    val plus1Order = plus1.get.evalOrder
    val plus2Order = plus2.get.evalOrder
    val minOrder = min.get.evalOrder
    plus1Order max plus2Order max minOrder
  }

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 3)
    plus1 = children.head
    plus2 = children.tail.head
    min = children.tail.tail.head
  }

  override def updateFirst(child: NNFNode) =
    if (plus1.isEmpty) {
      plus1 = Some(child)
      true
    } else if (plus2.isEmpty) {
      plus2 = Some(child)
      true
    } else if (min.isEmpty) {
      min = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new InclusionExclusion(
      cnf,
      Some(plus1.get.condition(pos, neg)),
      Some(plus2.get.condition(pos, neg)),
      Some(min.get.condition(pos, neg)),
      explanation,
      domains
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode =
      new InclusionExclusion(cnf, None, None, None, explanation, domains)
    NNFNode.smoothingCache(this) = newNode
    val plus1Missing = NNFNode
      .removeSubsumed(
        plus2.get.variablesForSmoothing union min.get.variablesForSmoothing
      )
      .flatMap { _.minus(plus1.get.variablesForSmoothing) }
    val plus2Missing = NNFNode
      .removeSubsumed(
        plus1.get.variablesForSmoothing union min.get.variablesForSmoothing
      )
      .flatMap { _.minus(plus2.get.variablesForSmoothing) }
    val minMissing = NNFNode
      .removeSubsumed(
        plus1.get.variablesForSmoothing union plus2.get.variablesForSmoothing
      )
      .flatMap { _.minus(min.get.variablesForSmoothing) }
    val plus1SmoothAll = plus1.get.smooth.smoothWith(plus1Missing)
    val plus2SmoothAll = plus2.get.smooth.smoothWith(plus2Missing)
    val minSmoothAll = min.get.smooth.smoothWith(minMissing)
    newNode.update(
      List(Some(plus1SmoothAll), Some(plus2SmoothAll), Some(minSmoothAll))
    )
    newNode
  }

  def ieSymbol =
    """$\begin{tikzpicture}[scale=0.08] \draw (0,0) circle (1.6cm); \draw (0:2cm) circle (1.6cm); \end{tikzpicture}$"""

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (n1, e1) = plus1.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val (n2, e2) = plus2.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val (n3, e3) = min.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val myNodes = if (compact) {
        "  " + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ """ + ieSymbol + """", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" +
          "  " + "ie" + getName(
            nameSpace
          ) + """ [texlbl="""" + fontsize + """ """ + ieSymbol + """", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + plus1.get.getName(
          nameSpace
        ) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + getName(nameSpace) + " -> " + plus2.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + getName(nameSpace) + " -> " + min.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel("-") + """];""" + "\n"
      } else {
        "  " + getName(nameSpace) + " -> " + "ie" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + plus1.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + plus2.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel("+") + """];""" + "\n" +
          "  " + "ie" + getName(nameSpace) + " -> " + min.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel("-") + """];""" + "\n"
      }
      val nodes = (myNodes + n1 + n2 + n3)
      val edges = (myEdges + e1 + e2 + e3)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + plus1.get.getName(nameSpace) +
      ", " + plus2.get.getName(nameSpace) +
      ", " + min.get.toString(nameSpace) + "\n" +
      "\n" +
      plus1.get.toString(nameSpace) +
      "\n" +
      plus2.get.toString(nameSpace) +
      "\n" +
      min.get.toString(nameSpace))

}

class Or(
    val cnf: CNF,
    var l: Option[NNFNode],
    var r: Option[NNFNode],
    val explanation: String = "",
    var domains: Set[Domain] = Set()
) extends NNFNode {

  override def directSuccessors = List(l, r)

  def simpleClone(): NNFNode = new Or(cnf, None, None, explanation, domains)

  def size = l.get.size + r.get.size + 1

  lazy val evalOrder = {
    val rOrder = r.get.evalOrder
    val lOrder = l.get.evalOrder
    rOrder max lOrder
  }

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 2)
    l = children.head
    r = children.tail.head
  }
  override def updateFirst(child: NNFNode) =
    if (l.isEmpty) {
      l = Some(child)
      true
    } else if (r.isEmpty) {
      r = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new Or(
      cnf,
      Some(l.get.condition(pos, neg)),
      Some(r.get.condition(pos, neg)),
      explanation,
      domains
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new Or(cnf, None, None, explanation, domains)
    NNFNode.smoothingCache(this) = newNode
    val lMissing = r.get.variablesForSmoothing.flatMap {
      _.minus(l.get.variablesForSmoothing)
    }
    val rMissing = l.get.variablesForSmoothing.flatMap {
      _.minus(r.get.variablesForSmoothing)
    }
    val lSmoothAll = l.get.smooth.smoothWith(lMissing)
    val rSmoothAll = r.get.smooth.smoothWith(rMissing)
    newNode.update(List(Some(lSmoothAll), Some(rSmoothAll)))
    newNode
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = l.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val (nr, er) = r.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val myNodes = if (compact) {
        "  " + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\lor$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" +
          "  " + "or" + getName(
            nameSpace
          ) + """ [texlbl="""" + fontsize + """ $\lor$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + l.get.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + r.get.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val llwmc =
          try {
            wmcVisitor.visit(
              l.get.smooth,
              (domainSizes, predicateWeights)
            )
          } catch {
            case e: UnsupportedOperationException => Complex.nan
          }
        val rlwmc =
          try {
            wmcVisitor.visit(
              r.get.smooth,
              (domainSizes, predicateWeights)
            )
          } catch {
            case e: UnsupportedOperationException => Complex.nan
          }
        "  " + getName(nameSpace) + " -> " + "or" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "or" + getName(nameSpace) + " -> " + l.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel(" $ " + llwmc + " $ ") + """];""" + "\n" +
          "  " + "or" + getName(nameSpace) + " -> " + r.get.getName(
            nameSpace
          ) + """ [""" + edgeLabel(" $ " + rlwmc + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl + nr)
      val edges = (myEdges + el + er)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + l.get.getName(nameSpace) + " v " + r.get
        .getName(nameSpace) + "\n" +
      "\n" +
      l.get.toString(nameSpace) +
      "\n" +
      r.get.toString(nameSpace))

}

class Ref(
    val cnf: CNF,
    var nnfNode: Option[NNFNode],
    val domainMap: Map[Domain, Domain],
    val explanation: String = "",
    var domains: Set[Domain] = Set()
) extends NNFNode {

  override def directSuccessors = List(nnfNode)

  // Cycles created by Ref nodes can complicate the order of the circuit in
  // various ways. Since evalOrder is not used for anything important, let's
  // not bother computing the exact order.
  lazy val evalOrder = 0

  def simpleClone(): NNFNode =
    new Ref(cnf, None, domainMap, explanation, domains)

  def size = 0

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length < 2)
    if (!children.isEmpty) nnfNode = children.head
  }

  override def updateFirst(child: NNFNode) =
    if (nnfNode.isEmpty) {
      nnfNode = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new Ref(
      cnf,
      Some(NNFNode.conditionCache((nnfNode.get, pos, neg))),
      domainMap,
      explanation,
      domains
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new Ref(cnf, None, domainMap, explanation, domains)
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(Some(nnfNode.get.smooth)))
    newNode
  }

  override def getName(nameSpace: NameSpace[NNFNode, String]) =
    nnfNode.get.getName(nameSpace)

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) = {
    nameSpace.getName(nnfNode.get)
    ("", "")
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    ""
  }

}

/** Needed so that recursive calls can reference the formula after shattering */
class ShatterNode(
    val cnf: CNF,
    var child: Option[NNFNode],
    val explanation: String = ""
) extends NNFNode {

  // ========================= ONE-LINERS =====================================

  override def directSuccessors = List(child)

  var domains: Set[Domain] = Set.empty

  lazy val evalOrder = child.get.evalOrder + 1

  def simpleClone(): NNFNode = {
    val n = new ShatterNode(cnf, None, explanation)
    n.domains = domains
    n
  }

  def size = child.get.size + 1

  // ========================= EVERYTHING ELSE ================================

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 1)
    child = children.head
  }

  override def updateFirst(newChild: NNFNode) =
    if (child.isEmpty) {
      child = Some(newChild)
      true
    } else {
      false
    }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new ShatterNode(cnf, None, explanation)
    newNode.domains = domains
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(Some(child.get.smooth)))
    newNode
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new ShatterNode(
      cnf,
      Some(child.get.condition(pos, neg)),
      explanation
    )
    returnValue.domains = domains
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val myNodes = if (compact) {
        "  " + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ shattering", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" + "  shattering" + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ shattering", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(nameSpace) +
          ";\n"
      } else {
        "  " + getName(nameSpace) + " -> " + "shatter" + getName(nameSpace) +
          """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "shatter" + getName(nameSpace) + " -> " +
          child.get.getName(nameSpace) + ";\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) + getName(nameSpace) + " = shatter")

}
