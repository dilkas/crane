package edu.ucla.cs.starai.forclift.nnf

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.Clause
import edu.ucla.cs.starai.forclift.Domain
import scala.collection._
import scala.util.matching.Regex
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.util.control.Breaks._
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import scala.collection.mutable.ListBuffer
import java.security.DomainCombiner

/**	
  * The boolean is true if the term is positive, else false.
  *
  * @param terms
  */
class FuncArgument(var terms : List[(Boolean, String)]){ //represents a function argument of the form x1-x2 or x1-10-x2 or x2-0
	def parseString(arg_str : String): Unit = {
		var temp = arg_str.split('-')
		if (arg_str(0) == '-'){
			val plus_loc = arg_str.indexOf('+')
			val plus_term = ("x?[0-9]+".r).findFirstIn(arg_str.substring(plus_loc)).getOrElse("")
			if (plus_term == ""){
				throw new IllegalStateException("Invalid argument: " + arg_str)
			}
			terms = List((true, plus_term))
			for(t <- 0 to temp.length - 1)
				if (temp(t) != "")
					terms = terms :+ (false, temp(t).split('+')(0))
		}
		else{
			terms = List((true, temp(0)))
			for(t <- 1 to temp.length - 1)
				terms = terms :+  (false, temp(t))
		}
	}

	def this(arg : String) = {
		this(List())
		parseString(arg)
	}
	
	override def toString() : String = {
		return terms.map(_._2).mkString("-")
	}
}

class FuncCall(var func_name: String, var args : List[FuncArgument]){  //represent a function call of the form f1(x1, x2-3, ..)
	def parseString(call : String): Unit =  {
		func_name = call.substring(0, call.indexOf('['))
		args = call.substring(call.indexOf('[')+1, call.length()-1).split(',').map(str => new FuncArgument(str.replaceAll("\\s", ""))).toList
	}
	def this(call : String) = {
		this("", List())
		parseString(call)
	}
	override def toString(): String = {
		return func_name + "(" + args.map(_.toString()).mkString(", ") + ")"
	}
}

object Basecases {

	/**
	 * Simplifies the clauses assuming that one of the domains is empty.
	 *
	 * @param clauses List of clauses that need to be simplified.
	 * @param null_dom Domain that is to be made empty.
	 * @return List of simplified clauses and those predicates that got removed during simplification, which would still contribute to the model count.
	 */
	def SetDomainToZero(clauses : List[Clause], null_dom : Domain) : (List[Clause], List[Predicate]) = {
		var simplified_clauses : List[Clause] = List()
		var removed_predicates : Set[Predicate] = Set()
		var retained_predicates : Set[Predicate] = Set()
		for (clause : Clause <- clauses){
			//check if null_dom is present in the domain constraints of the clause
			if (clause.constrs.elemConstrs.domains.contains(null_dom)){
				removed_predicates ++= clause.predicates
			}
			else{
				simplified_clauses = simplified_clauses :+ clause
				retained_predicates ++= clause.predicates
			}
		}
		removed_predicates --= retained_predicates
		return (simplified_clauses, removed_predicates.toList)
	}

	/**
	  * Given a list of recursive equations, it finds the function call on the LHS and the function calls on the rhs, i.e.  those required to find the lhs.
	  *
	  * @param equations list of equations.
	  * @return a map of the dependencies of each function 
	  */
	def find_function_dependency(equations : List[String]): Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = {
		equations.foreach(println(_))
		var dependencies : Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = Map()
		for(equation : String <- equations){
			var lhs : FuncCall = new FuncCall(equation.split('=')(0).replaceAll("\\s", ""))
			var dep : scala.collection.immutable.Set[FuncCall] = ("f[0-9]*\\[[x0-9,\\-\\+]*\\]".r).findAllIn(equation.split('=')(1).replaceAll("\\s", "")).map(str => new FuncCall(str)).toSet
			dependencies += (lhs -> dep)
		}
		return dependencies
	}

	def find_null_domain(dependencies : Map[FuncCall, scala.collection.immutable.Set[FuncCall]]): String = {
		//finding the set of functions that appear on the rhs
		val rhs_func_set : scala.collection.immutable.Set[String] = dependencies.values.toList.flatMap( arg => arg.map(_.func_name)).toSet
		//finding the set of equations whose lhs is appears on the rhs of some equation
		val rhs_func_dep : Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = dependencies.filter(p => rhs_func_set.contains(p._1.func_name))
		//finding the domain which has only x-1 on the rhs of rhs_func_dep
		val rhs_arg_list : List[FuncArgument] = rhs_func_dep.values.flatMap(arg => arg.map(_.args)).toList.flatten
		var rhs_var_map : Map[String, Boolean] = Map()
		var ret_val : String = ""
		for(arg <- rhs_arg_list){
			rhs_var_map(arg.terms(0)._2) = true
		}
		for(arg <- rhs_arg_list if (arg.terms.length != 2 || arg.terms(1)._2 != "1")){
			rhs_var_map(arg.terms(0)._2) = false
		}
		println(rhs_var_map.toString())
		for ((key, value) <- rhs_var_map if value && ret_val == ""){
			ret_val = key
		}
		if(ret_val == "" && rhs_var_map.size != 0)
			throw new IllegalStateException("No Null Domain Found")
		return ret_val
	}

	def find_args(call_str: String, sep: Char = ','): List[String] = {
		val str = call_str.replaceAll(" ", "")
		var args: List[String] = List()
		var num_open_brackets: Int = 0
		var prev_sep: Int = 0
		for(i <- 1 to str.length()-2){
			if (str(i) == '(' || str(i) == '{' || str(i) == '['){
				num_open_brackets += 1
			}
			else if (str(i) == ')' || str(i) == ']' || str(i) == '}'){
				num_open_brackets -= 1
			}
			else if (str(i) == sep && num_open_brackets == 0){
				args = args :+ str.substring(prev_sep+1, i)
				prev_sep = i
			}
		}
		args = args :+ str.substring(prev_sep+1, str.length()-1)
		return args
	}

	//@TODO: expand the piecewise and sums in the equation
	def expand_equation(eq_str: String) : String = {
		var eq = eq_str.replaceAll(" ", "")
		//find the outermost Sum
		var firstSumLoc: Int = eq.indexOf("Sum")
		if (firstSumLoc == -1){
			return eq
		}
		//finding the closing bracket
		var sumClosingLoc : Int = 0
		var numOpenBrackets : Int = 0
		breakable{
			for (i <- firstSumLoc to eq.length()-1){
				if (eq(i) == '['){
					numOpenBrackets += 1
				}
				else if (eq(i) == ']'){
					numOpenBrackets -= 1;
					if (numOpenBrackets == 0){
						sumClosingLoc = i
						break
					}
				}
			}
		}
		//finding the arguments of Sum
		var args : List[String] = find_args(eq.substring(firstSumLoc + 3, sumClosingLoc + 1))
		//finding the variable iteration for the sum
		var iter_var : String = args(1).substring(1, args(1).indexOf(','))
		// println("iter_var: " + iter_var)
		// println("first_char: " + args(1)(0).toInt + "|")
		// println(eq)
		// println(eq_str)
		//check if the sum can be expanded
		if (args(0).split('*')((args(0).split('*')).length-1).matches("Piecewise\\[\\{\\{1,Inequality\\[[a-zA-Z0-9,]*\\]\\}\\},0\\]")){
			//find the inequality inside the piecewise function
			val piecewise : String = args(0).split('*')((args(0).split('*')).length-1)
			var ineq_args : List[String] = find_args(("Inequality\\[[a-zA-Z0-9,]*\\]".r).findFirstIn(piecewise).getOrElse("").replaceAll("Inequality", ""))
			var rest : String = args(0).split('*').dropRight(1).mkString("*")
			if (ineq_args.length == 5){
				//find the inequality constraints on the summation variable
				if(ineq_args(2) != iter_var)
					return eq
				var lower_bound: Int = 0
				var upper_bound : Int = 0
				(ineq_args(1), ineq_args(3)) match {
					case ("LessEqual", "Less") => lower_bound = ineq_args(0).toInt; upper_bound = ineq_args(4).toInt-1
					case ("Less", "Less") => lower_bound = ineq_args(0).toInt+1; upper_bound = ineq_args(4).toInt-1
					case ("LessEqual", "LessEqual") => lower_bound = ineq_args(0).toInt; upper_bound = ineq_args(4).toInt
					case ("Less", "LessEqual") => lower_bound = ineq_args(0).toInt+1; upper_bound = ineq_args(4).toInt
					
					case ("GreaterEqual", "Greater") => lower_bound = ineq_args(4).toInt+1; upper_bound = ineq_args(0).toInt
					case ("Greater", "Greater") => lower_bound = ineq_args(4).toInt+1; upper_bound = ineq_args(0).toInt-1
					case ("GreaterEqual", "GreaterEqual") => lower_bound = ineq_args(4).toInt; upper_bound = ineq_args(0).toInt
					case ("Greater", "GreaterEqual") => lower_bound = ineq_args(4).toInt; upper_bound = ineq_args(0).toInt-1
				}
				var terms : List[String] = List()
				for (i <- lower_bound to upper_bound){
					terms = terms :+ rest.replaceAll(iter_var, i.toString())
				}
				var prefix = firstSumLoc match {
					case 0 => "" 
					case _ => eq.substring(0, firstSumLoc)
				} 
				var suffix = ""
				if (sumClosingLoc != eq.length()-1){
					suffix = eq.substring(sumClosingLoc+1)
				}

				return expand_equation(prefix + "(" + terms.mkString("+") + ")" + suffix)
			}
			else{
				return eq
			}
		}
		return eq
	}

	def find_base_cases(equations : List[String], clause_func_map : collection.mutable.Map[String, List[Clause]], var_domain_map : collection.mutable.Map[String, Domain], wcnf : WeightedCNF): List[String] = {
		var expanded_equations : List[String] = equations.map(expand_equation(_))
		println("\n==============================\n")
		expanded_equations.foreach(println(_))
		println("\n==============================\n")
		var dependencies = find_function_dependency(expanded_equations)
		val domain_var_map : collection.mutable.Map[Domain, String] = var_domain_map.flatMap{case (key, value) => Seq(value->key)}
		println("\n==============================\nDependencies\n")
		dependencies.foreach(println(_))
		println("\n==============================\n")
		val null_dom_var : String = find_null_domain(dependencies)
		if (null_dom_var == ""){
			return List[String]()
		}
		// println("\n==============================\nvar_domain_map\n")
		// println(var_domain_map.toString())
		// println("\n==============================\n")
		val null_dom : Domain = var_domain_map(null_dom_var)
		//find the basecases for each function with this domain set to zero by first simplifying the clauses and then calling crane
		//find basecases for only those functions which appear on the rhs of some equations
		val rhs_func_set : scala.collection.immutable.Set[String] = dependencies.values.toList.flatMap( arg => arg.map(_.func_name)).toSet
		val base_cases : ListBuffer[String] = ListBuffer()
		for(func <- rhs_func_set){
			//simplifying the corresponding clauses
			val (simplified_clauses, removed_predicates) : (List[Clause], List[Predicate]) = SetDomainToZero(clause_func_map(func), null_dom)
			val new_equations : ListBuffer[String] = ListBuffer()
			var base_case_var_domain_map : scala.collection.mutable.Map[String, Domain] = Map()
			if (simplified_clauses.size == 0){
				//If there are no clauses after simplification, then there is only one satisfuing model, so no need to call crane
				val index_of_func : Int = equations.indexWhere(_.startsWith(func))
				val index_of_equals : Int = equations(index_of_func).indexOf('=')
				val trivial_eqn : String = equations(index_of_func).substring(0, index_of_equals).replaceAll(func, "f0") + "= 1"
				new_equations += trivial_eqn
			}
			else{
				//finding the base cases using crane
				val simplified_cnf : CNF = new CNF(simplified_clauses)
				val simplified_wcnf : WeightedCNF = new WeightedCNF(simplified_cnf, wcnf.domainSizes, wcnf.predicateWeights, wcnf.conditionedAtoms, wcnf.compilerBuilder)
				new_equations ++= simplified_wcnf.SimplifyInWolfram
				base_case_var_domain_map = wcnf.varDomainMap
			}

			//change the variable names to the previous ones
			//do this only for the free variables, i.e. those occuring as parameters for the equation containing x0 on the lhs
			//if there is a collision, resolve it by changing the other variable to a new one
			var maxVarNumber : Int = (equations ++ new_equations).map(("x[0-9]+".r).findAllIn(_)).flatten.map(v => v.substring(1).toInt).max
			val index_of_f0 : Int = new_equations.indexWhere(_.startsWith("f0"))
			val index_of_equals : Int = new_equations(index_of_f0).indexOf('=')
			val free_vars : scala.collection.immutable.Set[String] = new_equations(index_of_f0).substring(0, index_of_equals + 1).split(',').map(_.replaceAll(" ", "")).toSet
			val f0_bounded_vars : scala.collection.immutable.Set[String] = ("x[0-9]".r).findAllIn(new_equations(index_of_f0)).toSet.diff(free_vars)
			for (free_var <- free_vars){
				val to_replace : String = domain_var_map(base_case_var_domain_map(free_var))
				if (to_replace != free_var){
					//check if there is a collision and handle it
					if (f0_bounded_vars.contains(to_replace)){
						//get a new variable name
						maxVarNumber += 1
						val newVarName = "x" + maxVarNumber.toString()
						new_equations(index_of_f0).replaceAll(to_replace, newVarName)
					}
				}
				new_equations(index_of_f0).replaceAll(free_var, "y" + to_replace.substring(1))
			}
			new_equations(index_of_f0).replace('y', 'x')
			//make the occurrences of f0 in the rest of the equations in `new_equations` consistent with the convention used on `func` in the previous equations
			val func_equation : String = equations(equations.indexWhere(_.startsWith(func)))
			val our_func_args : Array[String] = find_args(new_equations(index_of_f0).substring(new_equations(index_of_f0).indexOf('['), new_equations(index_of_f0).indexOf('='))).toArray
			val actual_func_args : Array[String] = find_args(func_equation.substring(func_equation.indexOf('['), func_equation.indexOf('=') + 1).replace(null_dom_var, "0")).toArray
			val index_map : scala.collection.mutable.Map[Int, Int] = Map()
			for(index <- 0 to our_func_args.length){
				index_map += (index -> actual_func_args.indexOf(our_func_args(index)))
			}
			new_equations.transform("f0\\[[x0-9,\\-\\+]*\\]".r.replaceAllIn(_, call => {
				val args : Array[String] = find_args(call.toString().substring(2)).toArray
				var transformed_args : Array[String] = actual_func_args.clone()
				for(index <- 0 to args.length){
					transformed_args(index_map(index)) = args(index)
				}
				"f0[" + transformed_args.mkString(", ") + "]"
			}))

			//multiply 2^(product of domain sizes of arguments of removed predicates) on the rhs of the function containing the model count of f0
			var multiplier : String = removed_predicates.map(pred => pred.domains.map(domain => {
				if (domain == null_dom) "0"
				else domain_var_map(domain)
			}).mkString("*")).mkString("+")
			multiplier = "2^(" + multiplier + ")"
			
			//change the name f0 to func and change the other function names(f1, f2, ...) too to some non-overlapping names
			var maxFuncNumber : Int = (equations ++ new_equations).map(("f[0-9]+".r).findAllIn(_)).flatten.map(v => v.substring(1).toInt).max
			val func_names : scala.collection.immutable.Set[String] =  new_equations.flatMap(("f[0-9]".r).findAllIn(_)).toSet - "f0"
			for(func_name <- func_names){
				maxFuncNumber += 1
				val new_func_name : String = "f" + maxFuncNumber.toString()
				new_equations.transform(_.replaceAll(func_name, new_func_name))
			}
			new_equations(index_of_f0) = func_equation.substring(0, func_equation.indexOf('=') + 1).replace(null_dom_var, "0") + multiplier + "(" + new_equations(index_of_f0).substring(index_of_equals+1) + ")"
			
			//append these basecases to base_cases
			base_cases ++= new_equations
		}

		return base_cases.toList
	}
}