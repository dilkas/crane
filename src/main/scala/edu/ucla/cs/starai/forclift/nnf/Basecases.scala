package edu.ucla.cs.starai.forclift.nnf

// TODO (Paulius): sort them
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.Clause
import edu.ucla.cs.starai.forclift.Domain
import edu.ucla.cs.starai.forclift.compiler.rulesets.MyCompiler
import scala.collection._
import scala.util.matching.Regex
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.util.control.Breaks._
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import scala.collection.mutable.ListBuffer
import edu.ucla.cs.starai.forclift.constraints.Constraints
import com.google.common.collect.BiMap
import com.google.common.collect.HashBiMap
import scala.collection.mutable.Stack
import scala.collection.JavaConversions._

/** The boolean is true if the term is positive, else false.
  *
  * @param terms
  *   represents a function argument of the form x1-x2 or x1-10-x2 or x2-0
  */
class FuncArgument(var terms: List[(Boolean, String)]) {

  def removeParentheses(exp: String): String = {
    var ans: StringBuilder = new StringBuilder()
    var num_minus: Int = 0
    val sign_stack: Stack[Boolean] = Stack()
    sign_stack.push(true)
    for (index <- 0 to exp.length() - 1) {
      if (exp(index) == ')') {
        sign_stack.pop()
      } else if (exp(index) == '+' || exp(index) == '-') {
        if (index != 0 && exp(index - 1) == '(' && ans.length != 0)
          ans.setLength(ans.length - 1)
        if (sign_stack.top) {
          ans += exp(index)
        } else {
          if (exp(index) == '-') ans += '+'
          else ans += '-'
        }
        if (exp(index + 1) == '(') {
          if (exp(index) == '-')
            sign_stack.push(!sign_stack.top)
          else
            sign_stack.push(sign_stack.top)
        }
      } else if (exp(index) == '(') {} else {
        ans += exp(index)
      }
    }
    if (ans(0) == '+')
      return ans.substring(1)
    return ans.toString()
  }

  def parseString(arg_str: String): Unit = {
    var temp = removeParentheses(arg_str).split('-')
    val term_buf: ListBuffer[(Boolean, String)] = ListBuffer()
    if (arg_str(0) == '-') {
      val plus_loc = arg_str.indexOf('+')
      val plus_term =
        ("x?[0-9]+".r).findFirstIn(arg_str.substring(plus_loc)).getOrElse("")
      if (plus_term == "") {
        throw new IllegalStateException("Invalid argument: " + arg_str)
      }
      term_buf += ((true, plus_term))
      for (t <- 0 to temp.length - 1)
        if (temp(t) != "")
          term_buf += ((false, temp(t).split('+')(0)))
    } else {
      term_buf += ((true, temp(0)))
      for (t <- 1 to temp.length - 1)
        term_buf += ((false, temp(t)))
    }

    // simplifying in case multiple arguments are integers
    var const_term: Int = 0
    var simplified_terms: ListBuffer[(Boolean, String)] = ListBuffer()
    for ((b, s) <- term_buf) {
      if (s.matches("[0-9]+")) {
        b match {
          case true  => const_term = const_term + s.toInt
          case false => const_term = const_term - s.toInt
        }
      } else {
        simplified_terms += ((b, s))
      }
    }
    const_term.signum match {
      case -1 => simplified_terms += ((false, (-const_term).toString()))
      case 1  => simplified_terms += ((true, const_term.toString()))
      case _ =>
        if (simplified_terms.size == 0) { simplified_terms += ((true, "0")) }
    }
    terms = simplified_terms.toList
  }

  def this(arg: String) = {
    this(List())
    parseString(arg)
  }

  override def toString(): String = {
    return terms.map(_._2).mkString("-")
  }
}

// represent a function call of the form f1(x1, x2-3, ..)
class FuncCall(var funcName: String, var args: List[FuncArgument]) {

  def parseString(call: String): Unit = {
    funcName = call.substring(0, call.indexOf('['))
    args = call
      .substring(call.indexOf('[') + 1, call.length() - 1)
      .split(',')
      .map(str => new FuncArgument(str.replaceAll("\\s", "")))
      .toList
  }

  def this(call: String) = {
    this("", List())
    parseString(call)
  }

  override def toString(): String = {
    return funcName + "[" + args.map(_.toString()).mkString(",") + "]"
  }
}

object Basecases {

  /** Given a list of recursive equations, it finds the function call on the LHS
    * and the function calls on the rhs, i.e. those required to find the lhs.
    *
    * @param equations
    *   list of equations.
    * @return
    *   a map of the dependencies of each function
    */
  def findFunctionDependency(
      equations: List[String]
  ): Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = {
    var dependencies: Map[FuncCall, scala.collection.immutable.Set[FuncCall]] =
      Map()
    for (equation: String <- equations) {
      var lhs: FuncCall = new FuncCall(
        equation.split('=')(0).replaceAll("\\s", "")
      )
      var dep: scala.collection.immutable.Set[FuncCall] =
        ("f[0-9]*\\[[x0-9,\\-\\+()]*\\]".r)
          .findAllIn(equation.split('=')(1).replaceAll("\\s", ""))
          .map(str => new FuncCall(str))
          .toSet
      dependencies += (lhs -> dep)
    }
    return dependencies
  }

  def findArgs(call_str: String, sep: Char = ','): List[String] = {
    val str = call_str.replaceAll(" ", "")
    var args: List[String] = List()
    var num_open_brackets: Int = 0
    var prev_sep: Int = 0
    for (i <- 1 to str.length() - 2) {
      if (str(i) == '(' || str(i) == '{' || str(i) == '[') {
        num_open_brackets += 1
      } else if (str(i) == ')' || str(i) == ']' || str(i) == '}') {
        num_open_brackets -= 1
      } else if (str(i) == sep && num_open_brackets == 0) {
        args = args :+ str.substring(prev_sep + 1, i)
        prev_sep = i
      }
    }
    args = args :+ str.substring(prev_sep + 1, str.length() - 1)
    return args
  }

  // @TODO: expand the piecewise and sums in the equation
  def expand_equation(eq_str: String): String = {
    var eq = eq_str.replaceAll(" ", "")

    // find the outermost Sum
    var firstSumLoc: Int = eq.indexOf("Sum")
    if (firstSumLoc == -1) {
      return eq
    }

    // finding the closing bracket
    var sumClosingLoc: Int = 0
    var numOpenBrackets: Int = 0
    breakable {
      for (i <- firstSumLoc to eq.length() - 1) {
        if (eq(i) == '[') {
          numOpenBrackets += 1
        } else if (eq(i) == ']') {
          numOpenBrackets -= 1;
          if (numOpenBrackets == 0) {
            sumClosingLoc = i
            break
          }
        }
      }
    }

    // finding the arguments of Sum
    var args: List[String] = findArgs(
      eq.substring(firstSumLoc + 3, sumClosingLoc + 1)
    )

    // finding the variable iteration for the sum
    var iter_var: String = args(1).substring(1, args(1).indexOf(','))

    // check if there is a piecewise term in the argument
    var num_open_brackets: Int = 0
    var modified_arg0: StringBuilder = new StringBuilder(args(0))
    for (index <- 0 to args(0).length() - 1) {
      modified_arg0(index) match {
        case '{' => num_open_brackets += 1
        case '}' => num_open_brackets -= 1
        case '*' =>
          if (num_open_brackets == 0) {
            modified_arg0.replace(index, index + 1, "_")
          }
        case _ =>
      }
    }
    var prod_terms: Array[String] = modified_arg0.split('_')
    val pw_index: Int = prod_terms.indexWhere(
      _.matches("Piecewise\\[\\{\\{1,Inequality\\[[a-zA-Z0-9,]*\\]\\}\\},0\\]")
    )

    // check if the sum can be expanded
    if (pw_index != -1) {
      // find the inequality inside the piecewise function
      val piecewise: String = prod_terms(pw_index)
      var ineq_args: List[String] = findArgs(
        ("Inequality\\[[a-zA-Z0-9,]*\\]".r)
          .findFirstIn(piecewise)
          .getOrElse("")
          .replaceAll("Inequality", "")
      )
      var rest: String = (prod_terms.slice(0, pw_index) ++ prod_terms.slice(
        pw_index + 1,
        prod_terms.length
      )).mkString("*")
      if (ineq_args.length == 5) {
        // find the inequality constraints on the summation variable
        if (ineq_args(2) != iter_var)
          return eq
        var lower_bound: Int = 0
        var upper_bound: Int = 0
        (ineq_args(1), ineq_args(3)) match {
          case ("LessEqual", "Less") =>
            lower_bound = ineq_args(0).toInt;
            upper_bound = ineq_args(4).toInt - 1
          case ("Less", "Less") =>
            lower_bound = ineq_args(0).toInt + 1;
            upper_bound = ineq_args(4).toInt - 1
          case ("LessEqual", "LessEqual") =>
            lower_bound = ineq_args(0).toInt; upper_bound = ineq_args(4).toInt
          case ("Less", "LessEqual") =>
            lower_bound = ineq_args(0).toInt + 1;
            upper_bound = ineq_args(4).toInt

          case ("GreaterEqual", "Greater") =>
            lower_bound = ineq_args(4).toInt + 1;
            upper_bound = ineq_args(0).toInt
          case ("Greater", "Greater") =>
            lower_bound = ineq_args(4).toInt + 1;
            upper_bound = ineq_args(0).toInt - 1
          case ("GreaterEqual", "GreaterEqual") =>
            lower_bound = ineq_args(4).toInt; upper_bound = ineq_args(0).toInt
          case ("Greater", "GreaterEqual") =>
            lower_bound = ineq_args(4).toInt;
            upper_bound = ineq_args(0).toInt - 1
        }
        var terms: List[String] = List()
        for (i <- lower_bound to upper_bound) {
          terms = terms :+ rest.replaceAll(iter_var, i.toString())
        }
        var prefix = firstSumLoc match {
          case 0 => ""
          case _ => eq.substring(0, firstSumLoc)
        }
        var suffix = ""
        if (sumClosingLoc != eq.length() - 1) {
          suffix = eq.substring(sumClosingLoc + 1)
        }

        return expand_equation(
          prefix + "(" + terms.mkString("+") + ")" + suffix
        )
      } else {
        return eq
      }
    }
    return eq
  }

  def getSufficientBaseCaseSet(
      dependencies: Map[FuncCall, scala.collection.immutable.Set[FuncCall]]
  ): Set[String] = {
    var baseCases: mutable.Set[String] = mutable.Set()
    for (dependency <- dependencies) {
      for (rhs_func <- dependency._2) {
        if (rhs_func.funcName == dependency._1.funcName) {
          for (arg <- rhs_func.args) {
            if (arg.terms.length > 2)
              throw new IllegalStateException(
                "This type of term not supported : " + arg.toString()
              )
            else if (arg.terms.length == 2) {
              val lim: Int = arg.terms(1)._2.toInt - 1
              for (l <- 0 to lim) {
                baseCases += dependency._1
                  .toString()
                  .replace(arg.terms(0)._2, l.toString())
              }
            }
          }
        } else {
          for (arg <- rhs_func.args) {
            if (arg.terms.length > 2)
              throw new IllegalStateException(
                "This type of term not supported : " + arg.toString()
              )
            else if (arg.terms.length == 2) {
              val lim: Int = arg.terms(1)._2.toInt - 1
              for (l <- 0 to lim) {
                baseCases += dependency._1
                  .toString()
                  .replace(arg.terms(0)._2, l.toString())
                baseCases += (rhs_func.funcName + "[" + rhs_func.args
                  .map(_.terms(0)._2)
                  .mkString(",") + "]").replace(arg.terms(0)._2, l.toString())
              }
            }
          }
        }
      }
    }
    return baseCases
  }

  case class BaseCaseIndexedConstant(val i: Int) {
    override def toString = "c" + (if (i > 0) ("'" * i) else "")
  }

  def transformClauses(
      func_call: FuncCall,
      wcnf: WeightedCNF,
      constDomain: Domain,
      varDomainMap: collection.mutable.Map[String, Domain]
  ): ListBuffer[(WeightedCNF, String)] = {
    var transformed_clauses: ListBuffer[(WeightedCNF, String)] = ListBuffer()
    var const: Int = -1

    // finding the constant
    for (arg <- func_call.args) {
      if (arg.terms.length == 1 && arg.terms(0)._2.matches("[0-9]+")) {
        if (const != -1) {
          return ListBuffer()
        }
        const = arg.terms(0)._2.toInt
      }
    }
    if (const == -1) {
      throw new IllegalStateException(
        "invalid arguments to function transformClauses"
      )
    }
    const match {
      case 0 => {
        val domainVarMap: collection.mutable.Map[Domain, String] =
          varDomainMap.flatMap { case (key, value) => Seq(value -> key) }
        var simplified_clauses: ListBuffer[Clause] = ListBuffer()
        var removed_predicates: Set[Predicate] = Set()
        var retained_predicates: Set[Predicate] = Set()
        var contains_null_const: Boolean = false
        for (clause: Clause <- wcnf.cnf.self if !contains_null_const) {
          // check if null_dom is present in the domain constraints of the clause
          if (clause.constrs.elemConstrs.domains.contains(constDomain)) {
            // check if there is a predicate none of  whose arguments belong to the null domain
            val new_posList: ListBuffer[Atom] = ListBuffer()
            val new_negList: ListBuffer[Atom] = ListBuffer()
            for (atom <- clause.atoms if !contains_null_const) {
              var contains_null_dom: Boolean = false
              for (arg <- atom.args if !contains_null_const) {
                arg match {
                  case variable: Var => {
                    if (clause.constrs.elemConstrs(variable) == constDomain) {
                      contains_null_dom = true
                    }
                  }
                  case const: Constant => {
                    if (const.domain == constDomain) {
                      contains_null_const = true
                    }
                  }
                  case _ => {
                    throw new IllegalStateException(
                      "Invalid member having trait Term"
                    )
                  }
                }
              }
              if (!contains_null_dom && !contains_null_const) {
                new_posList += atom
                if (new_posList.size == 1) {
                  new_negList += atom
                }
              }
            }
            val new_cosntr =
              Constraints(elemConstrs = clause.constrs.elemConstrs)
            val new_clause: Clause =
              Clause(new_posList.toList, new_negList.toList, new_cosntr)
            if (new_posList.size != 0 || new_negList.size != 0) {
              simplified_clauses += new_clause
            }
          } else {
            simplified_clauses += clause
          }
        }
        var multiplier: String = (if (contains_null_const) "0" else "1")
        transformed_clauses += ((
          new WeightedCNF(
            new CNF(simplified_clauses.toList),
            wcnf.domainSizes,
            wcnf.predicateWeights,
            wcnf.conditionedAtoms,
            wcnf.compilerBuilder
          ),
          multiplier
        ))
      }
      case 1 => {
        val constants_in_unit_domain: immutable.Set[Constant] =
          wcnf.cnf.constants.filter(_.domain == constDomain)
        if (constants_in_unit_domain.size <= 1) {
          val existingIndices = wcnf.cnf.constants
            .filter {
              _.value.isInstanceOf[BaseCaseIndexedConstant]
            }
            .map { _.value.asInstanceOf[BaseCaseIndexedConstant].i }
            .toSet
          val newIndex =
            Stream.from(0).find { index => !existingIndices(index) }.get
          val c = new Constant(new BaseCaseIndexedConstant(newIndex))
          val new_const: Constant = constants_in_unit_domain.size match {
            case 0 => c.setDomain(constDomain)
            case 1 => constants_in_unit_domain.toList(0)
          }
          val new_clauses = wcnf.cnf.clauses.flatMap { clause =>
            val vars = clause.literalVariables.filter {
              clause.constrs.domainFor(_).equals(constDomain)
            }
            val contradicting_ineq = clause.constrs.ineqConstrs.filter((t) => {
              vars.contains(t._1)
            })
            var new_clause = clause
            if (contradicting_ineq.size != 0) {
              new_clause = new Clause(
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
            new_clause = new_clause.substitute((variable: Var) =>
              if (vars.contains(variable)) {
                new_const
              } else {
                variable
              }
            )
            List(new_clause)
          }
          val multiplier: String = "1"
          transformed_clauses += ((
            new WeightedCNF(
              new CNF(new_clauses),
              wcnf.domainSizes,
              wcnf.predicateWeights,
              wcnf.conditionedAtoms,
              wcnf.compilerBuilder
            ),
            multiplier
          ))
        } else {
          transformed_clauses += ((wcnf, "0"))
        }
      }
      case _ => {
        throw new IllegalStateException(
          "No support for base cases with more than 2 elements in a domain"
        )
      }
    }
    transformed_clauses
  }

  def findBaseCases(
      equations: List[String],
      clauseFuncMap: collection.mutable.Map[String, List[Clause]],
      varDomainMap: collection.mutable.Map[String, Domain],
      wcnf: WeightedCNF
  ): List[String] = {
    println("Equations: " + equations.toString())
    var expandedEquations: List[String] =
      equations.map(eq => expand_equation(eq.replaceAll(" ", "")))
    val baseCases: ListBuffer[String] = ListBuffer()

    for (
      baseCaseLhs <- getSufficientBaseCaseSet(
        findFunctionDependency(expandedEquations)
      )
    ) {
      val lhsCall: FuncCall = new FuncCall(baseCaseLhs)
      val funcSignatureStr: String =
        expandedEquations.find(_.startsWith(lhsCall.funcName)) match {
          case Some(value: String) => value
          case _ =>
            throw new IllegalStateException(
              "No equation found for function " + lhsCall.funcName
            )
        }
      val signature: FuncCall = new FuncCall(
        funcSignatureStr.substring(0, funcSignatureStr.indexOf('='))
      )
      println(
        "signature: " + signature
          .toString() + ", base case: " + lhsCall.toString()
      )
      val diffIndex: Int =
        signature.args.zipWithIndex.zip(lhsCall.args).indexWhere {
          case ((a, i), b) => a.terms(0)._2 != b.terms(0)._2
        }
      val constDomain: Domain = varDomainMap(
        signature.args(diffIndex).terms(0)._2
      )
      val transformedWcnf: ListBuffer[(WeightedCNF, String)] =
        transformClauses(
          lhsCall,
          new WeightedCNF(
            new CNF(clauseFuncMap(lhsCall.funcName)),
            wcnf.domainSizes,
            wcnf.predicateWeights,
            wcnf.conditionedAtoms,
            wcnf.compilerBuilder
          ),
          constDomain,
          varDomainMap
        )

      for (
        (simplifiedWcnf: WeightedCNF, multiplier: String) <- transformedWcnf
      ) {
        baseCases ++= findBaseCases2(
          expandedEquations,
          lhsCall,
          constDomain,
          signature.funcName,
          simplifiedWcnf,
          multiplier,
          HashBiMap.create(varDomainMap)
        )
      }
    }
    baseCases.toList
  }

  /* TODO (Paulius):
  0) extract more functions,
  1) better name,
  2) shorter list of arguments (at most 4)
   */
  def findBaseCases2(
      expandedEquations: List[String],
      lhsCall: FuncCall,
      constDomain: Domain,
      functionName: String,
      simplifiedWcnf: WeightedCNF,
      multiplier: String,
      varDomainMap: BiMap[String, Domain]
  ): ListBuffer[String] = if (multiplier == "0") {
    ListBuffer(lhsCall.toString + " = 0")
  } else {
    var newEquations: ListBuffer[String] = ListBuffer()
    if (simplifiedWcnf.cnf.size == 0) {
      /* If there are no clauses after simplification, then there is
                 only one satisfying model, so no need to call Crane. This can
                 happen only if a domain is made empty. */
      val indexOfFunc = expandedEquations.indexWhere(_.startsWith(functionName))
      val indexOfEquals = expandedEquations(indexOfFunc).indexOf('=')
      newEquations += expandedEquations(indexOfFunc)
        .substring(0, indexOfEquals)
        .replaceAll(functionName, "f0") + "= 1"
    } else {
      // finding the base cases using Crane
      newEquations ++= simplifiedWcnf.SimplifyInWolfram
      newEquations.transform(_.replaceAll(" ", ""))
      /* Change the variable names to the previous ones. Do this only
              for the free variables, i.e. those occuring as parameters for the
              equation containing x0 on the LHS. If there is a collision,
              resolve it by changing the other variable to a new one. */
      var maxVarNumber = (expandedEquations ++ newEquations)
        .map(("x[0-9]+".r).findAllIn(_))
        .flatten
        .map(v => v.substring(1).toInt)
        .max
      val indexOfF0 = newEquations.indexWhere(_.startsWith("f0"))
      val indexOfEquals = newEquations(indexOfF0).indexOf('=')
      val freeVars: scala.collection.immutable.Set[String] =
        newEquations(indexOfF0)
          .substring(3, indexOfEquals - 1)
          .split(',')
          .map(_.replaceAll(" ", ""))
          .toSet
      val f0BoundedVars: scala.collection.immutable.Set[String] =
        ("x[0-9]+".r)
          .findAllIn(newEquations(indexOfF0))
          .toSet
          .diff(freeVars)
      for (freeVar <- freeVars) {
        val toReplace: String =
          varDomainMap.inverse.get(simplifiedWcnf.varDomainMap(freeVar))
        if (toReplace != freeVar) {
          // check if there is a collision and handle it
          if (f0BoundedVars.contains(toReplace)) {
            // get a new variable name
            maxVarNumber += 1
            newEquations(indexOfF0).replaceAll(
              toReplace,
              "x" + maxVarNumber.toString()
            )
          }
        }
        newEquations(indexOfF0).replaceAll(
          freeVar,
          "y" + toReplace.substring(1)
        )
      }
      newEquations(indexOfF0).replace('y', 'x')

      /* Make the occurrences of f0 in the rest of the equations in
              `newEquations` consistent with the convention used on `functionName` in
              the previous equations */
      val ourFuncArgs: Array[String] = findArgs(
        newEquations(indexOfF0).substring(
          newEquations(indexOfF0).indexOf('['),
          newEquations(indexOfF0).indexOf('=')
        )
      ).toArray
      val actualFuncArgs: Array[String] = findArgs(
        lhsCall.toString.substring(lhsCall.toString.indexOf('['))
      ).toArray

      val indexMap: scala.collection.mutable.Map[Int, Int] = Map()
      for (index <- 0 to ourFuncArgs.length - 1)
        if (simplifiedWcnf.varDomainMap.keySet.contains(ourFuncArgs(index)))
          if (simplifiedWcnf.varDomainMap(ourFuncArgs(index)) != constDomain)
            indexMap += (index -> actualFuncArgs.indexOf(ourFuncArgs(index)))

      newEquations.transform(
        "f0\\[[x0-9,\\-\\+]*\\]".r.replaceAllIn(
          _,
          call => {
            val args: Array[String] = findArgs(
              call.toString().substring(2)
            ).toArray.filter(varDomainMap(_) != constDomain)
            var transformedArgs: Array[String] =
              actualFuncArgs.clone()
            for (index <- 0 to args.length - 1) {
              transformedArgs(indexMap(index)) = args(index)
            }
            "f0[" + transformedArgs.mkString(", ") + "]"
          }
        )
      )
    }

    val (newNewEquations, indexOfF0) =
      changeFunctionNames(expandedEquations, newEquations, multiplier, lhsCall)
    newEquations = newNewEquations

    // find the constant
    val const = lhsCall.args.find(arg =>
      arg.terms.length == 1 && arg.terms(0)._2.matches("[0-9]+")
    ) match {
      case Some(value) => value.terms(0)._2.toInt
      case _           => throw new IllegalStateException("No constant found")
    }
    newEquations(indexOfF0) = newEquations(indexOfF0).replaceAll(
      varDomainMap.inverse.get(constDomain),
      const.toString()
    )

    newEquations
  }

  /* Change the name f0 to functionName and change the other function names
  (f1, f2, ...) too to some non-overlapping names */
  def changeFunctionNames(
      expandedEquations: List[String],
      newEquations: ListBuffer[String],
      multiplier: String,
      lhsCall: FuncCall
  ): (ListBuffer[String], Int) = {
    val outputEquations = newEquations.clone()
    var maxFuncNumber: Int = (expandedEquations ++ outputEquations)
      .map(("f[0-9]+".r).findAllIn(_))
      .flatten
      .map(v => v.substring(1).toInt)
      .max
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
    (outputEquations, indexOfF0)
  }

}

// class Equations(val equations: ListBuffer[String]) {
// }
