package edu.ucla.cs.starai.forclift.nnf

import scala.collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import scala.util.matching.Regex
import edu.ucla.cs.starai.forclift.Clause
import edu.ucla.cs.starai.forclift.Domain
import scala.collection.mutable.Set
import scala.collection.mutable.Map

/**
  * The boolen is true if the term is +ve, else false.
  *
  * @param terms
  */
class FuncArgument(var terms : List[(Boolean, String)]){ //represents a function argument of the form x1-x2 or x1-10-x2 or x2-0
	def parseString(arg_str : String){
		var temp = arg_str.split('-')
		if (arg_str(0) == '-'){
			val plus_loc = arg_str.indexOf('+')
			val plus_term = ("x?[0-9]+"r).findFirstIn(arg_str.substring(plus_loc)).getOrElse("")
			if (plus_term == ""){
				throw new IllegalStateException("Invalid argument: " + arg_str)
			}
			terms = List((true, plus_term))
		}
		else{
			terms = List((true, temp(0)))
		}
		for(t <- 1 to temp.length - 1)
			terms :+ (false, temp(t).split('+')(0))
	}
	def this(arg : String) {
		this(List())
		parseString(arg)
	}
}

class FuncCall(var func_name: String, var args : List[FuncArgument]){  //represent a function call of the form f1(x1, x2-3, ..)
	def parseString(call : String){
		func_name = call.substring(0, call.indexOf('('))
		args = call.substring(call.indexOf('(')+1, call.length()-2).split(',').map(str => new FuncArgument(str.replaceAll("\\s", ""))).toList
	}
	def this(call : String) {
		this("", List())
		parseString(call)
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
			var pos : Set[Atom] = Set()
			var neg : Set[Atom] = Set()
			//check if null_dom is present in the domain constraints of the clause
			if (clause.constrs.elemConstrs.domains.contains(null_dom)){
				removed_predicates ++= clause.predicates
			}
			else{
				simplified_clauses = (simplified_clauses :+ clause)
				retained_predicates ++= clause.predicates
			}
		}
		removed_predicates --= retained_predicates
		return (simplified_clauses, removed_predicates.toList)
	}
	/**
	  * Given a list of recursive equations, it finds the function call on the LHS and the function calls on the rhs, ie those required to find the lhs.
	  *
	  * @param equations list of equations.
	  * @return a map of the dependencies of each function 
	  */
	def find_function_dependency(equations : List[String]): Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = {
		var dependencies : Map[FuncCall, scala.collection.immutable.Set[FuncCall]] = Map()
		for(equation : String <- equations){
			var lhs : FuncCall = new FuncCall(equation.split('=')(0).replaceAll("\\s", ""))
			var dep : scala.collection.immutable.Set[FuncCall] = ("""f[0-9]*\\[[x0-9,-]*\\]"""r).findAllIn(equation.split('=')(1).replaceAll("\\s", "")).map(str => new FuncCall(str)).toSet
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
		for(arg <- rhs_arg_list){
			rhs_var_map(arg.terms(0)._2) = true
		}
		for(arg <- rhs_arg_list if (arg.terms.length != 2 || arg.terms(1)._2 != "1")){
			rhs_var_map(arg.terms(0)._2) = false
		}
		for ((key, value) <- rhs_var_map if value){
			return key
		}
		throw new IllegalStateException("No Null Domain Found")
		return ""
	}

	def find_args(call_str: String, sep: Char = ','): List[String] = {
		val str = call_str.replaceAll(" ", "")
		var args: List[String] = List()
		var num_open_brackets: Int = 0
		var prev_sep: Int = 1
		for(i <- 1 to str.length()-2){
			if (str(i) == '(' || str(i) == '{' || str(i) == '['){
				num_open_brackets += 1
			}
			else if (str(i) == ')' || str(i) == ']' || str(i) == '}'){
				num_open_brackets -= 1
			}
			else if (str(i) == sep && num_open_brackets == 0){
				args :+ str.substring(prev_sep+1, i-1)
				prev_sep = i+1
			}
		}
		args :+ str.substring(prev_sep+1, str.length()-2)
		return args
	}

	//@TODO: expand the piecewise and sums in the equation
	def expand_equation(eq: String) : String = {
		return eq
	}
}