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
import edu.ucla.cs.starai.forclift.nnf.visitors._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import scala.sys.process._
import System._
import collection.mutable.ListBuffer
import constraints._
import java.io._
import breeze.math._

class NNFNameSpace extends NameSpace[NNFNode, String] {

  var lastUsedVar = -1;

  protected def createName(node: NNFNode) = {
    lastUsedVar += 1
    "n" + lastUsedVar //("x"*lastUsedVar)
  }
}

object Cachestats {

  var hits = 0

  var miss = 0

  type Key = (NNFNode, DomainSizes)
  type WMCCache = collection.mutable.Map[Key, Double]

  def cacheTried(key: Key, cache: WMCCache) {
    if (cache.contains(key)) hits += 1
    else miss += 1
    if ((hits + miss) % 1000 == 0) {
      println(
        "NNF eval cache hit rate = " + (hits * 100.0 / (hits + miss)) + "%"
      )
    }
  }

}

object NNFNode {

  /** A (before -> after) cache used when cloning a circuit. */
  val cloningCache = new mutable.HashMap[NNFNode, NNFNode]

  /** A cache used to avoid infinite loops when running condition() on nodes. */
  val conditionCache =
    new mutable.HashMap[(NNFNode, Set[Atom], Set[Atom]), NNFNode]

  /** A cache used to avoid infinite loops when running smooth() on nodes. */
  val smoothingCache = new mutable.HashMap[NNFNode, NNFNode]

  def removeSubsumed(clauses: Set[PositiveUnitClause]) = {
    val clauses2 = clauses.toList.filter { clause1 =>
      !clauses.exists { clause2 =>
        (clause1 ne clause2) && clause2.subsumes(clause1)
      }
    }
    def removeDoubles(
        clauses: List[PositiveUnitClause]
    ): List[PositiveUnitClause] =
      clauses match {
        case clause :: rest =>
          clause :: removeDoubles(rest.filter { clause independent _ })
        case Nil => Nil
      }
    removeDoubles(clauses2).toSet
  }

}

/** A ParametrisedNode is either a CountingNode or a ConstraintRemovalNode,
  * i.e., a node that causes new domains to be introduced.
  */
abstract class ParametrisedNode() extends NNFNode {

  def mainIntroducedDomain: Domain

}

/** A circuit node.
  *
  * @param variablesForSmoothing we construct this using
  *                              SmoothingVariablesVisitor and use it in
  *                              smooth()
  */
abstract class NNFNode(
    var variablesForSmoothing: Set[PositiveUnitClause] =
      Set[PositiveUnitClause]()
) {

  // ========================= TO OVERRIDE/IMPLEMENT ==========================

  def cnf: CNF

  def condition(pos: Set[Atom], neg: Set[Atom]): NNFNode

  /** A list of direct successors of a node.
    *
    * None means that the formula generated for this outgoing edge is not
    * compiled yet. The length of this list depends on the node type but (so
    * far) is guaranteed to be in [0, 3].
    */
  def directSuccessors: List[Option[NNFNode]] = List[Option[NNFNode]]()

  def domains: Set[Domain]

  def evalOrder: Int

  def explanation: String

  /** Makes a copy of this node alone without any outgoing edges. */
  def simpleClone(): NNFNode

  def size: Int

  def smooth: NNFNode

  /** Set the outgoing edges of this node to point to the given list of nodes.
    *
    * The length of the given list must match the out-degree of the node. For
    * some node types, different outgoing edges have different interpretations
    * so the order of the elements in this list is important.
    */
  def update(children: List[Option[NNFNode]]): Unit = ()

  /** Set the first unassigned outgoing edge to point to the given node.
    *
    * @return true if successful (i.e., at least one of the direct successors is
    *         None) and false otherwise
    */
  def updateFirst(child: NNFNode): Boolean = false

  // ========================= OTHER ONE-LINERS ===============================

  // Keeping this as 'lazy var' should still work because by the time
  // orderedDomains is called, domains will have stabilized to the final value
  lazy val orderedDomains: IndexedSeq[Domain] = domains.toIndexedSeq

  override def toString = toString(new NNFNameSpace)

  def getName(nameSpace: NameSpace[NNFNode, String]) = nameSpace.getName(this)

  val fontsize = "" //"\tiny"

  // ========================= CLONING ========================================

  /** A wrapper function for myClone2 meant to be called once per circuit
    * (unlike myClone2 with its recursive calls).
    */
  def myClone(): NNFNode = {
    NNFNode.cloningCache.clear()
    myClone2()
  }

  /** Uses NNFNode.cloningCache to make a copy of this node and all nodes
    * reachable from this one.
    */
  private def myClone2(): NNFNode =
    if (NNFNode.cloningCache.contains(this)) {
      // println("myClone: found " + getClass.getSimpleName + " in the cache")
      NNFNode.cloningCache(this)
    } else {
      val newNode: NNFNode = simpleClone()
      NNFNode.cloningCache(this) = newNode

      // println("myClone: constructing a copy of " + getClass.getSimpleName +
      //           " " + hashCode + ": " + newNode.hashCode)
      // if (isInstanceOf[ConstraintRemovalNode]) {
      //   println("\n" + asInstanceOf[ConstraintRemovalNode] + "\n")
      // }
      // println("myClone: there are " + directSuccessors.size +
      //           " direct successors")

      newNode.update(directSuccessors.map(_.map(n => n.myClone2())))
      // println("myClone: finished the construction of " +
      //           getClass.getSimpleName + " " + hashCode)
      newNode
    }

  // ========================= SMOOTHING ======================================

  /** The main (outer) function for smoothing.
    *
    * Uses PostOrderVisitor and SmoothingVariablesVisitor to populate nodes
    * with variablesForSmoothing.
    */
  def smoothWithPredicates(
      predicates: Set[Predicate],
      excluded: Set[PositiveUnitClause] = Set.empty
  ): NNFNode = {
    // initialise variables for smoothing
    val postOrderVisitor = new PostOrderVisitor
    postOrderVisitor.visit(this)
    val smoothingVariablesVisitor = new SmoothingVariablesVisitor(
      postOrderVisitor.nodeOrder
    )
    smoothingVariablesVisitor.updateVariables

    val thisSmoothed = smooth
    val allVars = predicates.map { _.toAtom }
    val missing = allVars.flatMap {
      _.minus(variablesForSmoothing union excluded)
    }
    // println("smoothWithPredicates: allVars: " + allVars)
    // println("smoothWithPredicates: variablesForSmoothing: " + variablesForSmoothing)
    // println("smoothWithPredicates: missing: " + missing.toList)
    // println("smoothWithPredicates: after makeDisjoint: " +
    //           makeDisjoint(missing.toList))
    thisSmoothed.smoothWith(makeDisjoint(missing.toList).toSet)
  }

  // assumes atoms are disjoint
  def smoothWith(atoms: Set[PositiveUnitClause]): NNFNode =
    atoms.foldLeft(this) { (branch, clause) =>
      assume(atoms.forall { atom1 =>
        atoms.forall { atom2 => (atom1 eq atom2) || atom1.independent(atom2) }
      })
      // println("Adding a smoothing node for clause: " + clause)
      new And(
        branch.cnf,
        Some(new SmoothingNode(clause)),
        Some(branch),
        "Smoothing of $" + clause.toLatex() + "$."
      )
    }

  // ========================= MISCELLANEOUS ==================================

  /** Modify the circuit to insert newNode at the first empty spot.
    *
    * Ignoring Ref nodes/edges turns the circuit into a tree. For each node, we
    * try to insert newNode as the first direct successor, then anywhere in the
    * first subtree, then as the second direct successor, and so on. This order
    * is convenient to maintain while applying inference rules.
    *
    * @return true if successful and false otherwise
    */
  def addNode(newNode: NNFNode): Boolean =
    if (isInstanceOf[Ref]) {
      false
    } else {
      // println("addNode: trying to add " + newNode.getClass.getSimpleName +
      //           " below " + getClass.getSimpleName)
      directSuccessors.foreach {
        case None => {
          updateFirst(newNode)
          // println("addNode: added " + newNode.getClass.getSimpleName +
          //           " as a direct successor of " + getClass.getSimpleName)
          return true
        }
        case Some(node) => {
          if (node.addNode(newNode)) return true
        }
      }
      // println("addNode: giving up on " + getClass.getSimpleName)
      false
    }

  def makeDisjoint(catoms: List[PositiveUnitClause]): List[PositiveUnitClause] =
    catoms match {
      case Nil => Nil
      case catom :: rest =>
        catom :: makeDisjoint(rest.flatMap { _.minus(catom) })
    }

  // ========================= OUTPUT/VISUALISATION ===========================

  def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (getName(nameSpace) + ": \n" +
      cnf + "\n" +
      (if (explanation.nonEmpty) explanation + "\n" else ""))
  }

  /**
    * @todo    This is rather non-portable.
    */
  def showPDF(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      compact: Boolean = false,
      maxDepth: Int = Integer.MAX_VALUE,
      dir: String = "nnfs",
      file: String = "liftedinference.nnf",
      verbose: Boolean = false
  ) = {

    ExternalBinaries.checkPdfLatexAvailable
    ExternalBinaries.checkDotAvailable
    ExternalBinaries.checkDot2TexAvailable
    ExternalBinaries.checkDot2TexiAvailable

    if (!(new File(dir)).exists) (new File(dir)).mkdir
    val out = new FileWriter(dir + "/" + file + ".tex")
    out.write(toLatex(domainSizes, predicateWeights, compact, maxDepth))
    out.close

    val pdfFile = new java.io.File(dir + "/" + file + ".pdf")
    pdfFile.delete()

    println("# Running pdflatex on ./" + dir + "/" + file + ".tex")
    if (!verbose) println("# Switch --verbose on to see latex output")
    val pb1 = Process(
      ExternalBinaries.pdflatexCmd + " --shell-escape --interaction=nonstopmode " + file + ".tex",
      Some(new java.io.File(dir))
    );
    val output1 = new ListBuffer[String]()
    pb1 ! ExternalBinaries.stringLogger(output1, output1)
    output1.foreach(l => if (verbose) println(l))

    var pdfreaderbin = "evince";
    if (
      getenv("JAVA_ARCH") == "x86_64" ||
      System.getProperty("os.name").toLowerCase().startsWith("mac os x")
    ) {
      // It's a Mac
      pdfreaderbin = "open";
    }

    println("# Trying to open ./" + dir + "/" + file + ".pdf")
    if ((new File("./" + dir + "/" + file + ".pdf")).exists()) {
      val output2 = new ListBuffer[String]()
      val pb2 = Process(
        pdfreaderbin + " " + file + ".pdf",
        Some(new java.io.File(dir))
      );
      pb2 ! ExternalBinaries.stringLogger(output2, output2)
      output2.foreach(l => if (verbose) println(l))
    } else {
      println("# ./" + dir + "/" + file + ".pdf not found")
    }
  }

  def toLatex(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      compact: Boolean = false,
      maxDepth: Int = Integer.MAX_VALUE
  ) = ("""\documentclass{article}

\""" + """usepackage{dot2texi}
\""" + """usepackage{tikz}
\""" + """usetikzlibrary{shapes,arrows}
\""" + """usepackage{amsmath}
\""" + """usepackage[active,pdftex,tightpage]{preview}

\newcommand{\bigforall}[2]{{{\raisebox{1pt}{\mbox{\Large$\forall$}$#1$}}\atop{\scriptstyle #2}}}
\newcommand{\bigexists}[2]{{{\raisebox{1pt}{\mbox{\Large$\exists$}$#1$}}\atop{\scriptstyle #2}}}

\PreviewEnvironment[{[]}]{tikzpicture}

\begin{document}

\begin{dot2tex}[options=-t raw --autosize]
""" + toDot(domainSizes, predicateWeights, compact, maxDepth) + """
\end{dot2tex}

\end{document}""")

  def toDot(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      compact: Boolean = false,
      maxDepth: Int = Integer.MAX_VALUE
  ) = {
    val (nodes, edges) = toDotNode(
      domainSizes,
      predicateWeights,
      new NNFNameSpace,
      compact,
      0,
      maxDepth
    )
    ("""digraph G {
  node [shape=rectangle];
""" + nodes + edges + """
}""")
  }

  def edgeLabel(lbl: String) = {
    """label=" ", texlbl="""" + fontsize + """ """ + lbl + """""""
  }

  def cutoff(
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false
  ): (String, String) = {
    val nodes = if (compact) {
      "  " + getName(
        nameSpace
      ) + """ [texlbl="""" + fontsize + """ $\vdots$", shape=triangle];""" + "\n"
    } else {
      "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf
        .toLatex() + """"];""" + "\n" +
        "  " + "cutoff" + getName(
        nameSpace
      ) + """ [texlbl="""" + fontsize + """ $\vdots$", shape=triangle];""" + "\n"
    }
    val edges = if (compact) {
      ""
    } else {
      "  " + getName(nameSpace) + " -> " + "cutoff" + getName(
        nameSpace
      ) + """ [""" + edgeLabel(explanation) + """];""" + "\n"
    }
    (nodes, edges)
  }

  def toDotNode(
      domainSizes: DomainSizes,
      predicateWeights: PredicateWeights,
      nameSpace: NameSpace[NNFNode, String],
      compact: Boolean = false,
      depth: Int,
      maxDepth: Int = Integer.MAX_VALUE
  ): (String, String)

}
