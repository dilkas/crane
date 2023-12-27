package edu.ucla.cs.starai.forclift.nnf

import scala.collection._
import scala.collection.JavaConversions._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import com.google.common.collect.BiMap
import com.google.common.collect.HashBiMap

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.inference.WeightedCNF

/** Represents a function argument of the form x1-x2 or x1-10-x2 or x2-0.
  *
  * Should be constructed from a String using the companion object.
  *
  * @param terms
  *   a list of terms (implicitly combined by subtraction). Each term is
  *   represented as a pair, where the first element is a Boolean value
  *   indicating whether the term is positive, and the second element is the
  *   variable name or integer constant.
  */
class FunctionArgument(val terms: List[(Boolean, String)]) {
  lazy val isConstant: Boolean =
    terms.length == 1 && terms(0)._2.matches("[0-9]+")
  lazy val isSuitableForBaseCases: Boolean =
    terms.length == 2 && terms(1)._2.matches("[0-9]+")
  override def toString(): String = terms.map(_._2).mkString("-")
}

object FunctionArgument {

  def apply(argStr: String) = {
    var temp = removeParentheses(argStr).split('-')
    val termBuf: ListBuffer[(Boolean, String)] = ListBuffer()
    if (argStr.nonEmpty && argStr(0) == '-') {
      val plusLoc = argStr.indexOf('+')
      val plusTerm =
        ("x?[0-9]+".r).findFirstIn(argStr.substring(plusLoc)).getOrElse("")
      if (plusTerm == "")
        throw new IllegalStateException("Invalid argument: " + argStr)
      termBuf += ((true, plusTerm))
      for (t <- 0 to temp.length - 1)
        if (temp(t) != "")
          termBuf += ((false, temp(t).split('+')(0)))
    } else {
      termBuf += ((true, temp(0)))
      for (t <- 1 to temp.length - 1)
        termBuf += ((false, temp(t)))
    }
    new FunctionArgument(simplify(termBuf))
  }

  /** Removes unnecessary parentheses from the expression, simplifying + and -
    * signs in the process.
    *
    * Used by apply().
    */
  private[this] def removeParentheses(str: String): String = {
    val ans: StringBuilder = new StringBuilder()
    val signStack = Stack[Boolean]()
    signStack.push(true)
    for (i <- 0 to str.length() - 1) {
      if (str(i) == ')') {
        signStack.pop()
      } else if (str(i) != '(') {
        ans += str(i)
      } else if (str(i) == '+' || str(i) == '-') {
        if (i != 0 && str(i - 1) == '(' && ans.length != 0)
          ans.setLength(ans.length - 1)
        if (signStack.top) {
          ans += str(i)
        } else {
          ans += (if (str(i) == '-') '+' else '-')
        }
        if (str(i + 1) == '(')
          signStack.push(if (str(i) == '-') !signStack.top else signStack.top)
      }
    }
    if (ans.nonEmpty && ans(0) == '+') ans.substring(1) else ans.toString()
  }

  /** Reduces the number of integers in the expression to just one.
    *
    * Used by apply().
    */
  private[this] def simplify(
      termBuf: ListBuffer[(Boolean, String)]
  ): List[(Boolean, String)] = {
    var constTerm = 0
    val simplifiedTerms = ListBuffer[(Boolean, String)]()
    for ((b, s) <- termBuf) {
      if (s.matches("[0-9]+")) {
        b match {
          case true  => constTerm = constTerm + s.toInt
          case false => constTerm = constTerm - s.toInt
        }
      } else {
        simplifiedTerms += ((b, s))
      }
    }
    constTerm.signum match {
      case -1 => simplifiedTerms += ((false, (-constTerm).toString()))
      case 1  => simplifiedTerms += ((true, constTerm.toString()))
      case _ =>
        if (simplifiedTerms.size == 0)
          simplifiedTerms += ((true, "0"))
    }
    simplifiedTerms.toList
  }

}

/** Represents a function call of the form f1(x1, x2-3, ...)
  *
  * Should be constructed from a String using the companion object.
  */
case class FunctionCall(
    val name: String,
    val args: List[FunctionArgument]
) {

  lazy val constantArguments: List[String] = args
    .filter(_.isConstant)
    .map(_.terms(0)._2)

  lazy val firstConstant: String = {
    assert(constantArguments.length == 1)
    constantArguments.head
  }

  /** Replaces the ith argument to have value v */
  def replaceArgument(i: Int, v: Int): FunctionCall =
    new FunctionCall(name, args.updated(i, FunctionArgument(v.toString)))

  override def toString(): String =
    name + "[" + args.map(_.toString).mkString(",") + "]"
}

object FunctionCall {
  def apply(call: String) = {
    val call2 = call.replaceAll("\\s", "")
    val args = call2
      .substring(call2.indexOf('[') + 1, call2.length() - 1)
      .split(',')
      .map(str => FunctionArgument(str))
      .toList
    new FunctionCall(call2.substring(0, call2.indexOf('[')), args)
  }
}

/** A list of equations, each represented as a String. */
case class Equations(val equations: List[String] = List()) {

  // ========================== One-liners and values ==========================

  def +(other: String): Equations = Equations(equations :+ other)
  def ++(other: List[String]): Equations = Equations(equations ++ other)
  def ++(other: Equations): Equations = Equations(equations ++ other.equations)

  /** Returns a map from the function call on the left-hand side to the set of
    * function calls on the right-hand side.
    */
  private lazy val dependencies: Map[FunctionCall, Set[FunctionCall]] =
    equations
      .map(equation => {
        val dep = ("f[0-9]*\\[[x0-9,\\-\\+()]*\\]".r)
          .findAllIn(equation.split('=')(1).replaceAll("\\s", ""))
          .map(str => FunctionCall(str))
          .toSet
        (FunctionCall(equation.split('=')(0).replaceAll("\\s", "")) -> dep)
      })
      .toMap

  lazy val functionNames: Set[String] =
    equations.flatMap(("f[0-9]".r).findAllIn(_)).toSet

  def equationForFunction(name: String): String =
    equations.find(_.startsWith(name)) match {
      case Some(value: String) => value
      case _ =>
        throw new IllegalStateException(
          "No equation found for function " + name
        )
    }

  lazy val indexOfF0: Int = equations.indexWhere(_.startsWith("f0")) match {
    case -1 =>
      throw new IllegalStateException(
        "No equation found for function f0 among equations " + equations
      )
    case x => x
  }

  def map(f: String => String): Equations = Equations(equations.map(f))

  lazy val maxFuncNumber: Int = equations
    .map(("f[0-9]".r).findAllIn(_))
    .flatten
    .map(v => v.substring(1).toInt)
    .max

  def maxFuncNumber(other: Equations): Int =
    maxFuncNumber.max(other.maxFuncNumber)

  lazy val maxVarNumber: Int = equations
    .map(("x[0-9]+".r).findAllIn(_))
    .flatten
    .map(v => v.substring(1).toInt)
    .max

  def maxVarNumber(other: Equations): Int =
    maxVarNumber.max(other.maxVarNumber)

  def replace(oldName: String, newName: String): Equations =
    map(equation => equation.replace(oldName, newName))

  def updateF0(f: String => String): Equations = updateF0(
    f(equations(indexOfF0))
  )
  def updateF0(v: String): Equations = Equations(
    equations.updated(indexOfF0, v)
  )

  lazy val withoutSpaces = Equations(equations.map(_.replaceAll(" ", "")))

  lazy val withSumsExpanded: Equations = withoutSpaces.map(Equations.expandSums)

  // ============================= Everything else =============================

  // NOTE: The 'set' part is crucial here and has to work on strings rather
  // than FunctionCalls (at least for now)
  private[this] lazy val baseCases: Set[FunctionCall] = {
    var baseCases = Set[String]()
    for {
      dependency <- withSumsExpanded.dependencies
      rhsFunc <- dependency._2
      (arg, i) <- rhsFunc.args.zipWithIndex
      if arg.isSuitableForBaseCases
    } {
      for (l <- 0 to arg.terms(1)._2.toInt - 1)
        baseCases += dependency._1.replaceArgument(i, l).toString
    }
    baseCases.map(FunctionCall(_))
  }

  /** Make the occurrences of f0 consistent with the convention used on `name`
    * in the previous equations.
    *
    * @param lhsCall
    *   the function call in the original set of equations
    */
  def changeArguments(
      lhsCall: FunctionCall,
      variablesToDomains: BiMap[String, Domain],
      variablesToDomains2: Map[String, Domain],
      constDomain: Domain
  ): Equations = {
    val actualFuncArgs = Equations
      .findArgs(
        lhsCall.toString.substring(lhsCall.toString.indexOf('['))
      )
      .toArray
    val indexMap =
      createIndexMap(actualFuncArgs, variablesToDomains2, constDomain)
    map(
      "f0\\[[x0-9,\\-\\+]*\\]".r.replaceAllIn(
        _,
        call => {
          val args = Equations
            .findArgs(call.toString().substring(2))
            .toArray
            .filter(variablesToDomains(_) != constDomain)
          var transformedArgs = actualFuncArgs.clone()
          for (index <- (0 to args.length - 1).filter(i => indexMap.contains(i) && indexMap(i) != -1))
            transformedArgs(indexMap(index)) = args(index)
          "f0[" + transformedArgs.mkString(", ") + "]"
        }
      )
    )
  }

  /* Change the name f0 to name and change the other function names
  (f1, f2, ...) too to some non-overlapping names */
  def changeFunctionNames(
      multiplier: String,
      lhsCall: String,
      initialFuncNumber: Int
  ): Equations = {
    var funcNumber = initialFuncNumber
    var outputEquations = this
    for (name <- functionNames - "f0") {
      funcNumber += 1
      outputEquations = outputEquations.replace(name, "f" + funcNumber.toString)
    }
    outputEquations.updateF0(equation => {
      val indexOfEquals = equation.indexOf('=')
      lhsCall + "=" +
        (if (multiplier != "1") (multiplier + "(") else "") +
        equation.substring(indexOfEquals + 1) +
        (if (multiplier != "1") ")" else "")
    })
  }

  /** Maps each index of f0 parameters to the index of actualFuncArgs where that
    * parameter appears (except parameters associated with the domain that's
    * being eliminated (i.e., constDomain) (according to variablesToDomains2)).
    */
  def createIndexMap(
      actualFuncArgs: Array[String],
      variablesToDomains2: Map[String, Domain],
      constDomain: Domain
  ): Map[Int, Int] = {
    val parametersOfF0 = Equations
      .findArgs(
        equations(indexOfF0).substring(
          equations(indexOfF0).indexOf('['),
          equations(indexOfF0).indexOf('=')
        )
      )
      .toArray
    (0 to parametersOfF0.length - 1)
      .filter(index =>
        variablesToDomains2.keySet.contains(
          parametersOfF0(index)
        ) && variablesToDomains2(parametersOfF0(index)) != constDomain
      )
      .map(index => (index -> actualFuncArgs.indexOf(parametersOfF0(index))))
      .toMap
  }

  def findBaseCaseDefinitions(
      nameToFormula: Map[String, List[Clause]],
      variablesToDomains: Map[String, Domain],
      wcnf: WeightedCNF
  ): Equations = {
    var definitions = Equations()
    for (lhsCall <- baseCases) {
      println("\nTrying to find a base case for " + lhsCall + "\n")
      val funcSignatureStr = equationForFunction(lhsCall.name)
      val signature = FunctionCall(
        funcSignatureStr.substring(0, funcSignatureStr.indexOf('='))
      )
      val firstDifference =
        signature.args.zipWithIndex.zip(lhsCall.args).indexWhere {
          case ((a, i), b) => a.terms(0)._2 != b.terms(0)._2
        }
      val constDomain = variablesToDomains(
        signature.args(firstDifference).terms(0)._2
      )
      val newWcnf = wcnf.copy(cnf = new CNF(nameToFormula(lhsCall.name)))
      val (simplifiedWcnf, multiplier) = lhsCall.firstConstant.toInt match {
        case 0 => Equations.processZeroConstant(newWcnf, constDomain)
        case 1 => Equations.processOneConstant(newWcnf, constDomain)
        case _ =>
          throw new IllegalStateException(
            "The only constants allowed are 0 and 1"
          )
      }

      definitions ++= withSumsExpanded.propagate(
        lhsCall,
        constDomain,
        signature.name,
        simplifiedWcnf,
        multiplier,
        HashBiMap.create(variablesToDomains)
      )
    }
    definitions
  }

  private def propagate(
      lhsCall: FunctionCall,
      constDomain: Domain,
      name: String,
      simplifiedWcnf: WeightedCNF,
      multiplier: String,
      variablesToDomains: BiMap[String, Domain]
  ): Equations = if (multiplier == "0") {
    Equations(List(lhsCall.toString + " = 0"))
  } else {
    var newEquations = Equations()
    if (simplifiedWcnf.cnf.size == 0) {
      /* If there are no clauses after simplification, then there is
                 only one model, so no need to call Crane. This can
                 happen only if a domain is made empty. */
      val equation = equationForFunction(name)
      newEquations += equation
        .substring(0, equation.indexOf('='))
        .replaceAll(name, "f0") + "= 1"
    } else {
      // finding the base cases using Crane
      val (newEquations2, variablesToDomains2) = simplifiedWcnf.asEquations
      newEquations = newEquations2.withoutSpaces
      val newF0 = Equations.changeVariableNames(
        newEquations.equations(newEquations.indexOfF0),
        variablesToDomains,
        variablesToDomains2,
        maxVarNumber(newEquations)
      )
      newEquations = newEquations
        .updateF0(newF0)
        .changeArguments(
          lhsCall,
          variablesToDomains,
          variablesToDomains2,
          constDomain
        )
    }

    newEquations = newEquations.updateF0(
      _.replaceAll(
        variablesToDomains.inverse.get(constDomain),
        lhsCall.firstConstant
      )
    )
    newEquations.changeFunctionNames(
      multiplier,
      lhsCall.toString,
      maxFuncNumber(newEquations)
    )
  }

}

object Equations {

  private case class BaseCaseIndexedConstant(val i: Int) {
    override def toString = "c" + (if (i > 0) ("'" * i) else "")
  }

  /** Changes the variable names to the previous ones.
    *
    * Does this only for the free variables, i.e. those occuring as parameters
    * for the equation containing x0 on the left-hand side.
    */
  def changeVariableNames(
      equation: String,
      variablesToDomains: BiMap[String, Domain],
      variablesToDomains2: Map[String, Domain],
      initialVarNumber: Int
  ): String = {
    var varNumber = initialVarNumber
    var newValue = equation
    val indexOfEquals = newValue.indexOf('=')
    val freeVars = newValue
      .substring(3, indexOfEquals - 1)
      .split(',')
      .map(_.replaceAll(" ", ""))
      .toSet
    val f0BoundedVars = ("x[0-9]+".r)
      .findAllIn(newValue)
      .toSet
      .diff(freeVars)
    for (freeVar <- freeVars) {
      val toReplace =
        variablesToDomains.inverse.get(variablesToDomains2(freeVar))
      if (toReplace != freeVar) {
        // check if there is a collision and handle it
        if (f0BoundedVars.contains(toReplace)) {
          // get a new variable name
          varNumber += 1
          newValue.replaceAll(toReplace, "x" + varNumber.toString)
        }
      }
      newValue.replaceAll(freeVar, "y" + toReplace.substring(1))
    }
    newValue.replace('y', 'x')
    newValue
  }

  private[this] def findClosingBracket(str: String, i0: Int): Int = {
    var numOpenBrackets = 0
    for (i <- i0 to str.length() - 1) {
      if (str(i) == '[') {
        numOpenBrackets += 1
      } else if (str(i) == ']') {
        numOpenBrackets -= 1
        if (numOpenBrackets == 0)
          return i
      }
    }
    throw new IllegalStateException("No closing bracket found")
  }

  private[this] def splitAProduct(arg0: String): Array[String] = {
    var numOpenBrackets = 0
    var modifiedArg0: StringBuilder = new StringBuilder(arg0)
    for (index <- 0 to arg0.length() - 1) {
      modifiedArg0(index) match {
        case '{' => numOpenBrackets += 1
        case '}' => numOpenBrackets -= 1
        case '*' =>
          if (numOpenBrackets == 0)
            modifiedArg0.replace(index, index + 1, "_")
        case _ =>
      }
    }
    modifiedArg0.split('_')
  }

  private[this] def determineBounds(ineqArgs: List[String]): Range =
    (ineqArgs(1), ineqArgs(3)) match {
      case ("LessEqual", "Less") => ineqArgs(0).toInt to ineqArgs(4).toInt - 1
      case ("Less", "Less") => ineqArgs(0).toInt + 1 to ineqArgs(4).toInt - 1
      case ("LessEqual", "LessEqual") => ineqArgs(0).toInt to ineqArgs(4).toInt
      case ("Less", "LessEqual") => ineqArgs(0).toInt + 1 to ineqArgs(4).toInt
      case ("GreaterEqual", "Greater") =>
        ineqArgs(4).toInt + 1 to ineqArgs(0).toInt
      case ("Greater", "Greater") =>
        ineqArgs(4).toInt + 1 to ineqArgs(0).toInt - 1
      case ("GreaterEqual", "GreaterEqual") =>
        ineqArgs(4).toInt to ineqArgs(0).toInt
      case ("Greater", "GreaterEqual") =>
        ineqArgs(4).toInt to ineqArgs(0).toInt - 1
    }

  /** Expands sums in the equation */
  def expandSums(eqStr: String): String = {
    val eq = eqStr.replaceAll(" ", "")

    // find the outermost Sum
    val firstSumLoc = eq.indexOf("Sum")
    if (firstSumLoc == -1)
      return eq
    val sumClosingLoc = findClosingBracket(eq, firstSumLoc)

    // find the arguments of Sum
    val args =
      Equations.findArgs(eq.substring(firstSumLoc + 3, sumClosingLoc + 1))

    // finding the variable iteration for the sum
    val iterVar = args(1).substring(1, args(1).indexOf(','))

    // check if there is a piecewise term in the argument
    val prodTerms = splitAProduct(args(0))
    val pwIndex = prodTerms.indexWhere(
      _.matches("Piecewise\\[\\{\\{1,Inequality\\[[a-zA-Z0-9,]*\\]\\}\\},0\\]")
    )

    // check if the sum can be expanded
    if (pwIndex == -1) {
      eq
    } else {
      // find the inequality inside the piecewise function
      val ineqArgs = Equations.findArgs(
        ("Inequality\\[[a-zA-Z0-9,]*\\]".r)
          .findFirstIn(prodTerms(pwIndex))
          .getOrElse("")
          .replaceAll("Inequality", "")
      )
      val rest = (prodTerms.slice(0, pwIndex) ++ prodTerms.slice(
        pwIndex + 1,
        prodTerms.length
      )).mkString("*")
      if (ineqArgs.length != 5)
        return eq
      // find the inequality constraints on the summation variable
      if (ineqArgs(2) != iterVar)
        return eq
      val terms =
        determineBounds(ineqArgs).map(i => rest.replaceAll(iterVar, i.toString))
      expandSums(
        eq.substring(0, firstSumLoc) + "(" + terms.mkString("+") + ")" + eq
          .substring(sumClosingLoc + 1)
      )
    }
  }

  /** Splits the input string using comma as the separator, ignoring commas
    * inside of any kind of brackets.
    */
  private def findArgs(callStr: String): List[String] = {
    val str = callStr.replaceAll(" ", "")
    var args: List[String] = List()
    var numOpenBrackets: Int = 0
    var prevSep: Int = 0
    for (i <- 1 to str.length() - 2) {
      if (str(i) == '(' || str(i) == '{' || str(i) == '[') {
        numOpenBrackets += 1
      } else if (str(i) == ')' || str(i) == ']' || str(i) == '}') {
        numOpenBrackets -= 1
      } else if (str(i) == ',' && numOpenBrackets == 0) {
        args = args :+ str.substring(prevSep + 1, i)
        prevSep = i
      }
    }
    args :+ str.substring(prevSep + 1, str.length() - 1)
  }

  // TODO (Paulius): rename to propagate instead of process
  private def processZeroConstant(
      wcnf: WeightedCNF,
      constDomain: Domain
  ): (WeightedCNF, String) = {
    val simplifiedClauses: ListBuffer[Clause] = ListBuffer()
    var containsNullConst: Boolean = false
    for (clause: Clause <- wcnf.cnf.self if !containsNullConst) {
      /* check if the null domain is present in the domain constraints of
       * the clause */
      if (!clause.constrs.elemConstrs.domains.contains(constDomain)) {
        simplifiedClauses += clause
      } else {
        // check if there is a predicate none of whose arguments belong to the null domain
        val newPosList = ListBuffer[Atom]()
        val newNegList = ListBuffer[Atom]()
        for (atom <- clause.atoms if !containsNullConst) {
          var containsNullDom: Boolean = false
          for (arg <- atom.args if !containsNullConst) {
            arg match {
              case variable: Var =>
                if (clause.constrs.elemConstrs(variable) == constDomain)
                  containsNullDom = true
              case const: Constant =>
                if (const.domain == constDomain)
                  containsNullConst = true
              case _ =>
                throw new IllegalStateException(
                  "Invalid member having trait Term"
                )
            }
          }
          if (!containsNullDom && !containsNullConst) {
            newPosList += atom
            if (newPosList.size == 1)
              newNegList += atom
          }
        }
        if (newPosList.size != 0 || newNegList.size != 0)
          simplifiedClauses += Clause(
            newPosList.toList,
            newNegList.toList,
            Constraints(elemConstrs =
              clause.constrs.elemConstrs.filter(_._2 != constDomain)
            )
          )
      }
    }
    (
      wcnf.copy(cnf = new CNF(simplifiedClauses.toList)),
      if (containsNullConst) "0" else "1"
    )
  }

  private def processOneConstant(
      wcnf: WeightedCNF,
      constDomain: Domain
  ): (WeightedCNF, String) = {
    val constantsInUnitDomain =
      wcnf.cnf.constants.filter(_.domain == constDomain)
    if (constantsInUnitDomain.size > 1) {
      (wcnf, "0")
    } else {
      val existingIndices = wcnf.cnf.constants
        .filter {
          _.value.isInstanceOf[BaseCaseIndexedConstant]
        }
        .map { _.value.asInstanceOf[BaseCaseIndexedConstant].i }
        .toSet
      val newIndex = Stream.from(0).find(i => !existingIndices(i)).get
      val newConst = constantsInUnitDomain.size match {
        case 0 =>
          new Constant(BaseCaseIndexedConstant(newIndex)).setDomain(constDomain)
        case 1 => constantsInUnitDomain.toList(0)
      }

      val newClauses = wcnf.cnf.clauses.map { clause =>
        val vars = clause.literalVariables.filter {
          clause.constrs.domainFor(_).equals(constDomain)
        }
        var newClause = clause
        if (
          clause.constrs.ineqConstrs.filter(t => vars.contains(t._1)).nonEmpty
        )
          newClause = new Clause(
            clause.posLits ++ clause.negLits,
            clause.posLits ++ clause.negLits,
            new Constraints(
              elemConstrs =
                clause.constrs.elemConstrs.filter(_._2 != constDomain)
            )
          )
        newClause.substitute((v: Var) => if (vars.contains(v)) newConst else v)
      }
      (wcnf.copy(cnf = new CNF(newClauses)), "1")
    }
  }

}
