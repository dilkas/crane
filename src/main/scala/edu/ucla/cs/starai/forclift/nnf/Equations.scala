package edu.ucla.cs.starai.forclift.nnf

import scala.collection._
import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.control.Breaks._

import com.google.common.collect.BiMap
import com.google.common.collect.HashBiMap

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.inference.WeightedCNF

/** The boolean is true if the term is positive, else false.
  *
  * @param terms
  *   represents a function argument of the form x1-x2 or x1-10-x2 or x2-0
  */
class FunctionArgument(val terms: List[(Boolean, String)]) {
  override def toString(): String = terms.map(_._2).mkString("-")
}

object FunctionArgument {

  def apply(argStr: String) = {
    var temp = removeParentheses(argStr).split('-')
    val termBuf: ListBuffer[(Boolean, String)] = ListBuffer()
    if (argStr(0) == '-') {
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

  def removeParentheses(str: String): String = {
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
    if (ans(0) == '+') ans.substring(1) else ans.toString()
  }

  /** Simplifies in case multiple arguments are integers */
  private def simplify(
      termBuf: ListBuffer[(Boolean, String)]
  ): List[(Boolean, String)] = {
    var constTerm: Int = 0
    var simplifiedTerms: ListBuffer[(Boolean, String)] = ListBuffer()
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
        if (simplifiedTerms.size == 0) { simplifiedTerms += ((true, "0")) }
    }
    simplifiedTerms.toList
  }

}

/** Represents a function call of the form f1(x1, x2-3, ...) */
case class FunctionCall(
    val funcName: String,
    val args: List[FunctionArgument]
) {

  lazy val findConstant: Int = args.find(arg =>
    arg.terms.length == 1 && arg.terms(0)._2.matches("[0-9]+")
  ) match {
    case Some(value) => value.terms(0)._2.toInt
    case _           => throw new IllegalStateException("No constant found")
  }

  def replaceArgument(index: Int, value: String): FunctionCall =
    new FunctionCall(funcName, args.updated(index, FunctionArgument(value)))

  override def toString(): String =
    funcName + "[" + args.map(_.toString).mkString(",") + "]"
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

case class Equations(val equations: List[String] = List()) {

  def +(other: String): Equations = Equations(equations :+ other)
  def ++(other: List[String]): Equations = Equations(equations ++ other)
  def ++(other: Equations): Equations = Equations(equations ++ other.equations)

  /* Make the occurrences of f0 in the rest of the equations in
              `newEquations` consistent with the convention used on `functionName` in
              the previous equations */
  def changeArguments(
      indexOfF0: Int,
      lhsCall: FunctionCall,
      variablesToDomains: BiMap[String, Domain],
      variablesToDomains2: Map[String, Domain],
      constDomain: Domain
  ): Equations = {
    val ourFuncArgs: Array[String] = Equations
      .findArgs(
        equations(indexOfF0).substring(
          equations(indexOfF0).indexOf('['),
          equations(indexOfF0).indexOf('=')
        )
      )
      .toArray
    val actualFuncArgs: Array[String] = Equations
      .findArgs(
        lhsCall.toString.substring(lhsCall.toString.indexOf('['))
      )
      .toArray

    val indexMap: scala.collection.mutable.Map[Int, Int] = Map()
    for (index <- 0 to ourFuncArgs.length - 1)
      if (
        variablesToDomains2.keySet.contains(
          ourFuncArgs(index)
        ) && variablesToDomains2(
          ourFuncArgs(index)
        ) != constDomain
      )
        indexMap += (index -> actualFuncArgs.indexOf(ourFuncArgs(index)))

    Equations(
      equations.map(
        "f0\\[[x0-9,\\-\\+]*\\]".r.replaceAllIn(
          _,
          call => {
            val args: Array[String] = Equations
              .findArgs(
                call.toString().substring(2)
              )
              .toArray
              .filter(variablesToDomains(_) != constDomain)
            var transformedArgs: Array[String] =
              actualFuncArgs.clone()
            for (index <- 0 to args.length - 1)
              transformedArgs(indexMap(index)) = args(index)
            "f0[" + transformedArgs.mkString(", ") + "]"
          }
        )
      )
    )
  }

  /* Change the name f0 to functionName and change the other function names
  (f1, f2, ...) too to some non-overlapping names */
  def changeFunctionNames(
      multiplier: String,
      lhsCall: FunctionCall,
      initialMaxFuncNumber: Int
  ): (Equations, Int) = {
    var maxFuncNumber = initialMaxFuncNumber
    val outputEquations = equations.to[ListBuffer]
    for (
      funcName <- outputEquations
        .flatMap(("f[0-9]".r).findAllIn(_))
        .toSet - "f0"
    ) {
      maxFuncNumber += 1
      outputEquations.transform(
        _.replaceAll(funcName, "f" + maxFuncNumber.toString())
      )
    }
    val indexOfF0: Int = outputEquations.indexWhere(_.startsWith("f0"))
    val indexOfEquals: Int = outputEquations(indexOfF0).indexOf('=')
    outputEquations(indexOfF0) =
      lhsCall.toString + "=" + (if (multiplier != "1")
                                  (multiplier + "(")
                                else "") + outputEquations(indexOfF0)
        .substring(indexOfEquals + 1) + (if (multiplier != "1") ")"
                                         else "")
    (Equations(outputEquations.to[List]), indexOfF0)
  }

  /* Change the variable names to the previous ones. Do this only
              for the free variables, i.e. those occuring as parameters for the
              equation containing x0 on the LHS. If there is a collision,
              resolve it by changing the other variable to a new one. */
  def changeVariableNames(
      oldEquations: Equations,
      variablesToDomains: BiMap[String, Domain],
      variablesToDomains2: collection.mutable.Map[String, Domain]
  ): (Equations, Int) = {
    var maxVarNumber: Int = oldEquations.findMaxVarNumber.max(findMaxVarNumber)
    val indexOfF0 = equations.indexWhere(_.startsWith("f0"))
    var newValue = equations(indexOfF0)
    val indexOfEquals = newValue.indexOf('=')
    val freeVars: scala.collection.immutable.Set[String] =
      newValue
        .substring(3, indexOfEquals - 1)
        .split(',')
        .map(_.replaceAll(" ", ""))
        .toSet
    val f0BoundedVars: scala.collection.immutable.Set[String] =
      ("x[0-9]+".r)
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
          maxVarNumber += 1
          newValue.replaceAll(toReplace, "x" + maxVarNumber.toString)
        }
      }
      newValue.replaceAll(freeVar, "y" + toReplace.substring(1))
    }
    newValue.replace('y', 'x')
    (updated(indexOfF0, newValue), indexOfF0)
  }

  lazy val expanded: Equations = Equations(
    equations.map(eq => Equations.expandEquation(eq.replaceAll(" ", "")))
  )

  def findBaseCaseDefinitions(
      functionNameToFormula: collection.mutable.Map[String, List[Clause]],
      variablesToDomains: collection.mutable.Map[String, Domain],
      wcnf: WeightedCNF
  ): Equations = {
    var baseCases = Equations()
    for (
      baseCaseLhs <- Equations.findBaseCases(
        expanded.findFunctionDependency
      )
    ) {
      val lhsCall = FunctionCall(baseCaseLhs)
      val funcSignatureStr: String = startsWith(lhsCall.funcName)
      val signature = FunctionCall(
        funcSignatureStr.substring(0, funcSignatureStr.indexOf('='))
      )
      val diffIndex: Int =
        signature.args.zipWithIndex.zip(lhsCall.args).indexWhere {
          case ((a, i), b) => a.terms(0)._2 != b.terms(0)._2
        }
      val constDomain: Domain = variablesToDomains(
        signature.args(diffIndex).terms(0)._2
      )
      for (
        (simplifiedWcnf: WeightedCNF, multiplier: String) <- Equations
          .transformClauses(
            lhsCall,
            new WeightedCNF(
              new CNF(functionNameToFormula(lhsCall.funcName)),
              wcnf.domainSizes,
              wcnf.predicateWeights,
              wcnf.conditionedAtoms,
              wcnf.compilerBuilder
            ),
            constDomain
          )
      ) {
        baseCases ++= expanded.propagate(
          lhsCall,
          constDomain,
          signature.funcName,
          simplifiedWcnf,
          multiplier,
          HashBiMap.create(variablesToDomains)
        )
      }
    }
    baseCases
  }

  private def propagate(
      lhsCall: FunctionCall,
      constDomain: Domain,
      functionName: String,
      simplifiedWcnf: WeightedCNF,
      multiplier: String,
      variablesToDomains: BiMap[String, Domain]
  ): Equations = if (multiplier == "0") {
    Equations(List(lhsCall.toString + " = 0"))
  } else {
    var newEquations = Equations()
    if (simplifiedWcnf.cnf.size == 0) {
      /* If there are no clauses after simplification, then there is
                 only one satisfying model, so no need to call Crane. This can
                 happen only if a domain is made empty. */
      val indexOfFunc = equations.indexWhere(_.startsWith(functionName))
      val indexOfEquals = equations(indexOfFunc).indexOf('=')
      newEquations += equations(indexOfFunc)
        .substring(0, indexOfEquals)
        .replaceAll(functionName, "f0") + "= 1"
    } else {
      // finding the base cases using Crane
      val (newEquations2, variablesToDomains2) = simplifiedWcnf.asEquations

      val (newEquations3, indexOfF0) = newEquations2.removeSpaces
        .changeVariableNames(this, variablesToDomains, variablesToDomains2)
      newEquations = newEquations3.changeArguments(
        indexOfF0,
        lhsCall,
        variablesToDomains,
        variablesToDomains2,
        constDomain
      )
    }

    val (newEquations4, newIndexOfF0) = newEquations.changeFunctionNames(
      multiplier,
      lhsCall,
      maxFuncNumber.max(newEquations.maxFuncNumber)
    )

    newEquations4.updated(
      newIndexOfF0,
      _.replaceAll(
        variablesToDomains.inverse.get(constDomain),
        lhsCall.findConstant.toString
      )
    )
  }

  /** Finds the function call on the LHS and the function calls on the RHS,
    * i.e. those required to find the LHS.
    *
    * @return
    *   a map of the dependencies of each function
    */
  lazy val findFunctionDependency
      : Map[FunctionCall, scala.collection.immutable.Set[FunctionCall]] = {
    var dependencies
        : Map[FunctionCall, scala.collection.immutable.Set[FunctionCall]] =
      Map()
    for (equation: String <- equations) {
      var lhs = FunctionCall(equation.split('=')(0).replaceAll("\\s", ""))
      var dep: scala.collection.immutable.Set[FunctionCall] =
        ("f[0-9]*\\[[x0-9,\\-\\+()]*\\]".r)
          .findAllIn(equation.split('=')(1).replaceAll("\\s", ""))
          .map(str => FunctionCall(str))
          .toSet
      dependencies += (lhs -> dep)
    }
    dependencies
  }

  lazy val maxFuncNumber: Int = equations
    .map(("f[0-9]".r).findAllIn(_))
    .flatten
    .map(v => v.substring(1).toInt)
    .max

  lazy val findMaxVarNumber: Int = equations
    .map(("x[0-9]+".r).findAllIn(_))
    .flatten
    .map(v => v.substring(1).toInt)
    .max

  lazy val removeSpaces = Equations(equations.map(_.replaceAll(" ", "")))

  def startsWith(funcName: String): String =
    equations.find(_.startsWith(funcName)) match {
      case Some(value: String) => value
      case _ =>
        throw new IllegalStateException(
          "No equation found for function " + funcName
        )
    }

  def updated(index: Int, value: String): Equations = Equations(
    equations.updated(index, value)
  )

  def updated(index: Int, f: String => String): Equations =
    updated(index, f(equations(index)))

}

object Equations {

  private case class BaseCaseIndexedConstant(val i: Int) {
    override def toString = "c" + (if (i > 0) ("'" * i) else "")
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

  // @TODO: expand the piecewise and sums in the equation
  def expandEquation(eqStr: String): String = {
    var eq = eqStr.replaceAll(" ", "")

    // find the outermost Sum
    var firstSumLoc: Int = eq.indexOf("Sum")
    if (firstSumLoc == -1)
      return eq
    val sumClosingLoc = findClosingBracket(eq, firstSumLoc)

    // finding the arguments of Sum
    var args: List[String] = Equations.findArgs(
      eq.substring(firstSumLoc + 3, sumClosingLoc + 1)
    )

    // finding the variable iteration for the sum
    var iterVar: String = args(1).substring(1, args(1).indexOf(','))

    // check if there is a piecewise term in the argument
    var prodTerms: Array[String] = splitAProduct(args(0))
    val pwIndex: Int = prodTerms.indexWhere(
      _.matches("Piecewise\\[\\{\\{1,Inequality\\[[a-zA-Z0-9,]*\\]\\}\\},0\\]")
    )

    // check if the sum can be expanded
    if (pwIndex != -1) {
      // find the inequality inside the piecewise function
      val piecewise: String = prodTerms(pwIndex)
      var ineqArgs: List[String] = Equations.findArgs(
        ("Inequality\\[[a-zA-Z0-9,]*\\]".r)
          .findFirstIn(piecewise)
          .getOrElse("")
          .replaceAll("Inequality", "")
      )
      var rest: String = (prodTerms.slice(0, pwIndex) ++ prodTerms.slice(
        pwIndex + 1,
        prodTerms.length
      )).mkString("*")
      if (ineqArgs.length == 5) {
        // find the inequality constraints on the summation variable
        if (ineqArgs(2) != iterVar)
          return eq
        var lowerBound: Int = 0
        var upperBound: Int = 0
        (ineqArgs(1), ineqArgs(3)) match {
          case ("LessEqual", "Less") =>
            lowerBound = ineqArgs(0).toInt;
            upperBound = ineqArgs(4).toInt - 1
          case ("Less", "Less") =>
            lowerBound = ineqArgs(0).toInt + 1;
            upperBound = ineqArgs(4).toInt - 1
          case ("LessEqual", "LessEqual") =>
            lowerBound = ineqArgs(0).toInt; upperBound = ineqArgs(4).toInt
          case ("Less", "LessEqual") =>
            lowerBound = ineqArgs(0).toInt + 1;
            upperBound = ineqArgs(4).toInt

          case ("GreaterEqual", "Greater") =>
            lowerBound = ineqArgs(4).toInt + 1;
            upperBound = ineqArgs(0).toInt
          case ("Greater", "Greater") =>
            lowerBound = ineqArgs(4).toInt + 1;
            upperBound = ineqArgs(0).toInt - 1
          case ("GreaterEqual", "GreaterEqual") =>
            lowerBound = ineqArgs(4).toInt; upperBound = ineqArgs(0).toInt
          case ("Greater", "GreaterEqual") =>
            lowerBound = ineqArgs(4).toInt;
            upperBound = ineqArgs(0).toInt - 1
        }
        var terms: List[String] = List()
        for (i <- lowerBound to upperBound) {
          terms = terms :+ rest.replaceAll(iterVar, i.toString())
        }
        var prefix = firstSumLoc match {
          case 0 => ""
          case _ => eq.substring(0, firstSumLoc)
        }
        var suffix = ""
        if (sumClosingLoc != eq.length() - 1) {
          suffix = eq.substring(sumClosingLoc + 1)
        }

        return expandEquation(prefix + "(" + terms.mkString("+") + ")" + suffix)
      } else {
        return eq
      }
    }
    eq
  }

  private def findArgs(callStr: String, sep: Char = ','): List[String] = {
    val str = callStr.replaceAll(" ", "")
    var args: List[String] = List()
    var numOpenBrackets: Int = 0
    var prevSep: Int = 0
    for (i <- 1 to str.length() - 2) {
      if (str(i) == '(' || str(i) == '{' || str(i) == '[') {
        numOpenBrackets += 1
      } else if (str(i) == ')' || str(i) == ']' || str(i) == '}') {
        numOpenBrackets -= 1
      } else if (str(i) == sep && numOpenBrackets == 0) {
        args = args :+ str.substring(prevSep + 1, i)
        prevSep = i
      }
    }
    args :+ str.substring(prevSep + 1, str.length() - 1)
  }

  private def findBaseCases(
      dependencies: Map[
        FunctionCall,
        scala.collection.immutable.Set[FunctionCall]
      ]
  ): Set[String] = {
    var baseCases = Set[String]()
    for {
      dependency <- dependencies
      rhsFunc <- dependency._2
      (arg, i) <- rhsFunc.args.zipWithIndex
    } {
      if (arg.terms.length > 2) {
        throw new IllegalStateException(
          "This type of term not supported : " + arg.toString
        )
      } else if (arg.terms.length == 2) {
        val lim: Int = arg.terms(1)._2.toInt - 1
        for (l <- 0 to lim) {
          baseCases += dependency._1.replaceArgument(i, l.toString).toString
          if (rhsFunc.funcName != dependency._1.funcName)
            baseCases += (rhsFunc.funcName + "[" + rhsFunc.args
              .map(_.terms(0)._2)
              .mkString(",") + "]").replace(arg.terms(0)._2, l.toString)
        }
      }
    }
    baseCases
  }

  private def transformClauses(
      functionCall: FunctionCall,
      wcnf: WeightedCNF,
      constDomain: Domain
  ): ListBuffer[(WeightedCNF, String)] = {
    val transformedClauses: ListBuffer[(WeightedCNF, String)] = ListBuffer()
    var const: Int = -1

    // finding the constant
    for (arg <- functionCall.args) {
      if (arg.terms.length == 1 && arg.terms(0)._2.matches("[0-9]+")) {
        if (const != -1)
          return ListBuffer()
        const = arg.terms(0)._2.toInt
      }
    }

    if (const == -1)
      throw new IllegalStateException(
        "invalid arguments to function transformClauses"
      )
    const match {
      case 0 => transformedClauses += processZeroConstant(wcnf, constDomain)
      case 1 => transformedClauses += processOneConstant(wcnf, constDomain)
      case _ =>
        throw new IllegalStateException(
          "No support for base cases with more than 2 elements in a domain"
        )
    }
    transformedClauses
  }

  private def processZeroConstant(
      wcnf: WeightedCNF,
      constDomain: Domain
  ): (WeightedCNF, String) = {
    val simplifiedClauses: ListBuffer[Clause] = ListBuffer()
    var containsNullConst: Boolean = false
    for (clause: Clause <- wcnf.cnf.self if !containsNullConst) {
      /* check if the null domain is present in the domain constraints of
       * the clause */
      if (clause.constrs.elemConstrs.domains.contains(constDomain)) {
        // check if there is a predicate none of  whose arguments belong to the null domain
        val newPosList: ListBuffer[Atom] = ListBuffer()
        val newNegList: ListBuffer[Atom] = ListBuffer()
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
            Constraints(elemConstrs = clause.constrs.elemConstrs)
          )
      } else {
        simplifiedClauses += clause
      }
    }
    val newWcnf = new WeightedCNF(
      new CNF(simplifiedClauses.toList),
      wcnf.domainSizes,
      wcnf.predicateWeights,
      wcnf.conditionedAtoms,
      wcnf.compilerBuilder
    )
    val multiplier = if (containsNullConst) "0" else "1"
    (newWcnf, multiplier)
  }

  private def processOneConstant(
      wcnf: WeightedCNF,
      constDomain: Domain
  ): (WeightedCNF, String) = {
    val constantsInUnitDomain: immutable.Set[Constant] =
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
      val newIndex =
        Stream.from(0).find { index => !existingIndices(index) }.get
      val c = new Constant(BaseCaseIndexedConstant(newIndex))
      val newConst: Constant = constantsInUnitDomain.size match {
        case 0 => c.setDomain(constDomain)
        case 1 => constantsInUnitDomain.toList(0)
      }
      val newClauses = wcnf.cnf.clauses.flatMap { clause =>
        val vars = clause.literalVariables.filter {
          clause.constrs.domainFor(_).equals(constDomain)
        }
        val contradictingIneq = clause.constrs.ineqConstrs.filter((t) => {
          vars.contains(t._1)
        })
        var newClause = clause
        if (contradictingIneq.size != 0) {
          newClause = new Clause(
            clause.posLits ++ clause.negLits,
            clause.posLits ++ clause.negLits,
            new Constraints(
              elemConstrs = clause.constrs.elemConstrs.filter(t => {
                val ret: Boolean = (t._2 != constDomain)
                ret
              })
            )
          )
        }
        List(
          newClause.substitute((variable: Var) =>
            if (vars.contains(variable)) newConst else variable
          )
        )
      }
      val newWcnf = new WeightedCNF(
        new CNF(newClauses),
        wcnf.domainSizes,
        wcnf.predicateWeights,
        wcnf.conditionedAtoms,
        wcnf.compilerBuilder
      )
      (newWcnf, "1")
    }
  }

}
