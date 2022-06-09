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

package edu.ucla.cs.starai.forclift.nnf

import scala.collection._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf.visitors.SafeSignLogDoubleWmc
import edu.ucla.cs.starai.forclift.nnf.visitors.WmcVisitor
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries

class IndependentPartialGroundingNode(
    val cnf: CNF,
    var child: Option[NNFNode],
    val c: Constant,
    val ineqs: Set[Constant],
    val d: Domain,
    val explanation: String = ""
) extends NNFNode {

  var domains = Set(d)

  override def directSuccessors = List(child)

  lazy val evalOrder = child.get.evalOrder

  def simpleClone(): NNFNode =
    new IndependentPartialGroundingNode(cnf, None, c, ineqs, d, explanation)

  def size = child.get.size + 1

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

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new IndependentPartialGroundingNode(
      cnf,
      Some(child.get.condition(pos, neg)),
      c,
      ineqs,
      d,
      explanation
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth =
    if (NNFNode.smoothingCache.contains(this)) {
      NNFNode.smoothingCache(this)
    } else {
      val newNode =
        new IndependentPartialGroundingNode(cnf, None, c, ineqs, d, explanation)
      NNFNode.smoothingCache(this) = newNode
      newNode.update(List(Some(child.get.smooth)))
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
      // println("toDotNode: IndependentPartialGroundingNode")
      val (nl, el) = child.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val subscript = ((c.toString + """ \in """ + d) :: ineqs.map {
        c.toString + """ \neq """ + _.toString
      }.toList).mkString(""" \land """)
      val myNodes = if (compact) {
        "  " + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" +
          "  " + "exp" + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(
          nameSpace
        ) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val childlwmc = wmcVisitor.visit(
          child.get.smooth,
          (domainSizes, predicateWeights)
        )
        "  " + getName(nameSpace) + " -> " + "exp" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "exp" + getName(nameSpace) + " -> " + child.get.getName(
          nameSpace
        ) + """ [""" + edgeLabel(
          " $ " + childlwmc.exp + " $ "
        ) + """];""" + "\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(nameSpace) + " = " + child.get.getName(
      nameSpace
    ) + " ^ |" + d + "|" + ineqs
      .map { """X \neq """ + _.toString }
      .mkString(" , ") + "\n" +
      "\n" +
      child.get.toString(nameSpace))

}

class CountingNode(
    val cnf: CNF,
    var child: Option[NNFNode],
    val domain: Domain,
    val subdomain: SubDomain,
    val explanation: String = ""
) extends ParametrisedNode {

  // ========================= ONE-LINERS =====================================

  override def directSuccessors = List(child)

  var domains = Set(domain)

  lazy val evalOrder = child.get.evalOrder + 1

  def excludedConstants = subdomain.excludedConstants

  def mainIntroducedDomain: Domain = subdomain

  def simpleClone(): NNFNode =
    new CountingNode(cnf, None, domain, subdomain, explanation)

  def size = child.get.size + 1

  // ========================= EQUALITY =======================================

  // override lazy val hashCode: Int = cnf.hashCode

  // def canEqual(a: Any) = a.isInstanceOf[CountingNode]

  // override def equals(that: Any): Boolean =
  //   that match {
  //     case that: CountingNode => cnf == that.cnf
  //     case _                  => false
  //   }

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

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new CountingNode(
      cnf,
      Some(child.get.condition(pos, neg)),
      domain,
      subdomain,
      explanation
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  def smooth =
    if (NNFNode.smoothingCache.contains(this)) {
      NNFNode.smoothingCache(this)
    } else {
      val newNode = new CountingNode(cnf, None, domain, subdomain, explanation)
      NNFNode.smoothingCache(this) = newNode
      // this is fine, but does not mean the result will be non-overlapping
      // two catoms might overlap but not one subsumes the other
      val countedSubdomainParents =
        NNFNode.removeSubsumed(child.get.variablesForSmoothing.map {
          _.reverseDomainSplitting(domain, subdomain)
        })
      val disjCounted = makeDisjoint(countedSubdomainParents.toList)
      val childMissing = disjCounted.flatMap {
        _.minus(child.get.variablesForSmoothing)
      }
      val childSmoothAll = child.get.smooth.smoothWith(childMissing.toSet)
      newNode.update(List(Some(childSmoothAll)))
      newNode
    }

  def minDomainSizeString = "|" + subdomain + "| = 0"
  def maxDomainSizeString = "|" + domain + "|"

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
      // println("toDotNode: CountingNode")
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
        ) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
          .toLatex() + """"];""" + "\n" +
          "  " + "count" + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(
          nameSpace
        ) + ";\n"

      } else {
        "  " + getName(nameSpace) + " -> " + "count" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "count" + getName(nameSpace) + " -> " + child.get.getName(
          nameSpace
        ) + ";\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(
        nameSpace
      ) + " = count " + subdomain + " from " + domain)
  //+ " " + child.get.getName(nameSpace) + "\n" + "\n"
  // + child.get.toString(nameSpace))

}

class DomainRecursionNode(
    val cnf: CNF,
    var mixedChild: Option[IndependentPartialGroundingNode],
    var groundChild: Option[NNFNode],
    val c: Constant,
    val ineqs: Set[Constant],
    val domain: Domain,
    val explanation: String = ""
) extends NNFNode {

  override def directSuccessors = List(mixedChild, groundChild)

  var domains = Set(domain)

  lazy val evalOrder = mixedChild.get.evalOrder // assume constant eval

  def simpleClone(): NNFNode =
    new DomainRecursionNode(cnf, None, None, c, ineqs, domain, explanation)

  def size = mixedChild.get.size + groundChild.get.size + 1

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 2)
    mixedChild = children.head.map {
      _.asInstanceOf[IndependentPartialGroundingNode]
    }
    groundChild = children.tail.head
  }

  override def updateFirst(child: NNFNode) =
    if (mixedChild.isEmpty) {
      mixedChild = Some(child.asInstanceOf[IndependentPartialGroundingNode])
      true
    } else if (groundChild.isEmpty) {
      groundChild = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new DomainRecursionNode(
      cnf,
      Some(
        mixedChild.get
          .condition(pos, neg)
          .asInstanceOf[IndependentPartialGroundingNode]
      ),
      Some(groundChild.get.condition(pos, neg)),
      c,
      ineqs,
      domain,
      explanation
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  def smooth =
    if (NNFNode.smoothingCache.contains(this)) {
      NNFNode.smoothingCache(this)
    } else {
      val newNode =
        new DomainRecursionNode(cnf, None, None, c, ineqs, domain, explanation)
      NNFNode.smoothingCache(this) = newNode
      newNode.update(
        List(Some(mixedChild.get.smooth), Some(groundChild.get.smooth))
      )
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
      // println("toDotNode: DomainRecursionNode")
      val (n1, e1) = mixedChild.get.toDotNode(
        domainSizes,
        predicateWeights,
        nameSpace,
        compact,
        depth + 1,
        maxDepth
      )
      val (n2, e2) = groundChild.get.toDotNode(
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
          "  " + "domainrec" + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + mixedChild.get.getName(
          nameSpace
        ) + ";\n" +
          "  " + getName(nameSpace) + " -> " + groundChild.get.getName(
          nameSpace
        ) + ";\n" +
          "  " + getName(nameSpace) + " -> " + getName(
          nameSpace
        ) + """ [""" + edgeLabel(
          "$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$"""
        ) + """];""" + "\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val groundChildWmc = wmcVisitor.visit(
          groundChild.get.smooth,
          (domainSizes, predicateWeights)
        )
        val mixedChildWmc = wmcVisitor.visit(
          mixedChild.get.smooth,
          (
            domainSizes - domain,
            predicateWeights
          )
        )
        "  " + getName(nameSpace) + " -> " + "domainrec" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.get
          .getName(nameSpace) + """ [""" + edgeLabel(
          " $ " + mixedChildWmc.exp + " $ "
        ) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + groundChild.get
          .getName(nameSpace) + """ [""" + edgeLabel(
          " $ " + groundChildWmc.exp + " $ "
        ) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + getName(
          nameSpace
        ) + """ [""" + edgeLabel(
          "$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$"""
        ) + """];""" + "\n"
      }
      val nodes = (myNodes + n1 + n2)
      val edges = (myEdges + e1 + e2)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
      getName(
        nameSpace
      ) + " = domainrec " + c + " from " + domain + " " + mixedChild.get
      .getName(nameSpace) + " " + groundChild.get.getName(nameSpace) + "\n" +
      "\n" + "\n" +
      mixedChild.get.toString(nameSpace) +
      "\n" +
      groundChild.get.toString(nameSpace))

}

/** A generalised version of DomainRecursionNode with out-degree equal to one
  * instead of two and no requirements for the subsequent formula.
  */
class ImprovedDomainRecursionNode(
    val cnf: CNF,
    var mixedChild: Option[NNFNode],
    val c: Constant,
    val domain: Domain,
    val explanation: String = ""
) extends NNFNode {

  var domains = Set(domain)

  override def directSuccessors = List(mixedChild)

  lazy val evalOrder = mixedChild.get.evalOrder // assume constant eval

  // It's easier just to leave this here
  lazy val ineqs = Set[Constant]()

  def simpleClone(): NNFNode =
    new ImprovedDomainRecursionNode(cnf, None, c, domain, explanation)

  def size = mixedChild.size + 1

  override def update(children: List[Option[NNFNode]]) = {
    require(children.length == 1)
    mixedChild = children.head
  }

  override def updateFirst(child: NNFNode) =
    if (mixedChild.isEmpty) {
      mixedChild = Some(child)
      true
    } else {
      false
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new ImprovedDomainRecursionNode(
      cnf,
      Some(mixedChild.get.condition(pos, neg)),
      c,
      domain,
      explanation
    )
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = simpleClone()
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(Some(mixedChild.get.smooth)))
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
      // println("toDotNode: ImprovedDomainRecursionNode")
      val (n1, e1) = mixedChild.get.toDotNode(
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
          "  " + "domainrec" + getName(
          nameSpace
        ) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + mixedChild.get.getName(
          nameSpace
        ) + ";\n" +
          "  " + getName(nameSpace) + " -> " + getName(
          nameSpace
        ) + """ [""" + edgeLabel(
          "$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$"""
        ) + """];""" + "\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val mixedChildWmc = wmcVisitor.visit(
          mixedChild.get.smooth,
          (
            domainSizes - domain,
            predicateWeights
          )
        )
        "  " + getName(nameSpace) + " -> " + "domainrec" + getName(
          nameSpace
        ) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.get
          .getName(nameSpace) + """ [""" + edgeLabel(
          " $ " + mixedChildWmc.exp + " $ "
        ) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + getName(
          nameSpace
        ) + """ [""" + edgeLabel(
          "$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$"""
        ) + """];""" + "\n"
      }
      val nodes = (myNodes + n1)
      val edges = (myEdges + e1)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) + getName(nameSpace) + " = domainrec " + c +
       " from " + domain)
  // + " " + mixedChild.get.getName(nameSpace) + "\n" + "\n" +
  // mixedChild.get.toString(nameSpace))

}
