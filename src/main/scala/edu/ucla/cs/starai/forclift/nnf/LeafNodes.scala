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

class GroundingNode(val cnf: CNF, val explanation: String = "")
    extends NNFNode(NNFNode.removeSubsumed(cnf.toPositiveUnitClauses)) {

  def condition(pos: Set[Atom], neg: Set[Atom]): NNFNode =
    throw new UnsupportedOperationException

  lazy val domains = cnf.domains

  def evalOrder = throw new UnsupportedOperationException

  def simpleClone(): NNFNode = new GroundingNode(cnf, explanation)

  def smooth = this

  def size = 1

  def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) = {
    // println("toDotNode: GroundingNode")
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=red!40",texlbl="""" + fontsize + """ """ + cnf
        .toLatex() + """"];""" + "\n"),
      ""
    )
  }

}

object TrueNode extends NNFNode {

  val cnf = CNF()

  lazy val domains = Set.empty[Domain]

  def evalOrder = 0

  def explanation: String = ""

  def simpleClone(): NNFNode = this

  def size = 1

  val smooth = this

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    NNFNode.conditionCache((this, pos, neg)) = this
    this
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) = {
    // println("toDotNode: TrueNode")
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=green!40",texlbl="""" + fontsize + """ $\top$"];""" + "\n"),
      ""
    )
  }
}

object FalseNode extends NNFNode {

  val cnf = CNF()

  lazy val domains = Set.empty[Domain]

  def evalOrder = 0

  def explanation: String = ""

  def simpleClone(): NNFNode = this

  def size = 1

  val smooth = this

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    NNFNode.conditionCache((this, pos, neg)) = this
    this
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) = {
    // println("toDotNode: FalseNode")
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=red!40",texlbl="""" + fontsize + """ $\bot$"];""" + "\n"),
      ""
    )
  }
}

class ContradictionLeaf(
    val cnf: CNF,
    val clause: ContradictionClause,
    val positive: Boolean,
    val explanation: String = ""
) extends NNFNode {

  lazy val domains = clause.domains

  def evalOrder = 0

  def simpleClone(): NNFNode =
    new ContradictionLeaf(cnf, clause, positive, explanation)

  def size = 1

  val smooth = this

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    NNFNode.conditionCache((this, pos, neg)) = this
    this
  }

  override def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String) = {
    // println("toDotNode: ContradictionLeaf")
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=red!40",texlbl="""" + fontsize + """ """ + cnf
        .toLatex(true) + """"];""" + "\n"),
      ""
    )
  }

}

class UnitLeaf(
    val cnf: CNF,
    val clause: UnitClause,
    val positive: Boolean,
    val explanation: String = ""
) extends NNFNode(Set(clause.toPositiveUnitClause)) {

  require(
    clause.isUnconditional,
    "Unit leafs have to be unconditional for smoothing to be correct: " + clause
  )

  lazy val domains = clause.domains

  def evalOrder = 0

  def simpleClone(): NNFNode = new UnitLeaf(cnf, clause, positive, explanation)

  def size = 1

  val smooth = this

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = if (clause.atom.isGround) {
      if (pos.contains(clause.atom)) TrueNode
      else if (neg.contains(clause.atom)) FalseNode
      else this
    } else this
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
  ): (String, String) = {
    // println("toDotNode: UnitLeaf for the formula: " + cnf)
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=green!20",texlbl="""" + fontsize + """ """ + cnf
        .toLatex(true) + """"];""" + "\n"),
      ""
    )
  }

}

class SmoothingNode(val clause: PositiveUnitClause)
    extends NNFNode(Set(clause.toPositiveUnitClause)) {

  lazy val cnf = CNF(
    Clause(List(clause.atom), List(clause.atom), clause.constrs)
  )

  lazy val domains = clause.domains

  def evalOrder = 0

  def explanation = ""

  def simpleClone(): NNFNode = new SmoothingNode(clause)

  def size = 1

  val smooth = this

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = if (clause.atom.isGround) {
      if (pos.contains(clause.atom) || neg.contains(clause.atom)) TrueNode
      else this
    } else this
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
  ): (String, String) = {
    // println("toDotNode: SmoothingNode")
    (
      ("  " + getName(
        nameSpace
      ) + """ [style="fill=blue!20",texlbl="""" + fontsize + """ """ + cnf
        .toLatex(true) + """"];""" + "\n"),
      ""
    )
  }

}
