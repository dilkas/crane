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

package edu.ucla.cs.starai.forclift.constraints

import collection._
import scala.language.implicitConversions

import edu.ucla.cs.starai.forclift._
import util._
import util.extracollection._

final class IneqConstr(final val self: MultiMap[Var, Term] = MultiMap.empty)
    extends MapProxy[Var, Set[Term]] {

  if (!self.values.forall { _.nonEmpty }) {
    println("remove me")
  }

  // this property is assumed by other classes iterating over this map
  assume(
    self.values.forall { _.nonEmpty },
    "The map does not contain empty inequality constraints."
  )

  import MultiMap._

  def +(v: Var, a: Term): IneqConstr = {
    if (!this(v).contains(a)) {
      var clone = self
      clone += (v, a)
      if (a.isInstanceOf[Var]) {
        clone += (a.asInstanceOf[Var], v)
      }
      new IneqConstr(clone)
    } else this
  }

  def -(v: Var, t: Term): IneqConstr = {
    if (this(v).contains(t)) {
      var clone = self
      clone -= (v, t)
      if (clone(v).isEmpty) clone -= v
      if (t.isInstanceOf[Var]) {
        val av = t.asInstanceOf[Var]
        clone -= (av, v)
        if (clone(av).isEmpty) clone -= av
      }
      new IneqConstr(clone)
    } else this
  }

  def --(other: IneqConstr): IneqConstr = {
    var clone = self
    for ((v, s) <- other.iterator; t <- s) {
      clone -= (v, t)
      if (clone(v).isEmpty) clone -= v
    }
    new IneqConstr(clone)
  }

  def variablesNotEqualTo(constant: Constant): Set[Var] = {
    self.collect {
      case (v, terms) if terms.contains(constant) => v
    }.toSet
  }

  def conflictsWith(eqClasses: List[EquivalenceClass]): Boolean = {
    // OLD IMPLEMENTATION - sound incomplete fast way to detect conflict
    for (eqClass <- eqClasses) {
      val differentElems = differentFromTerms(eqClass)
      if (eqClass.exists { differentElems.contains(_) }) return true
    }
    // NEW IMPLEMENTATION - sound incomplete slow way to detect conflict -
    // complete combined with previous
    // fixes bug where {X,a}, {Y,a} with X != Y does not conflict
    // we here assume that X != Y cannot appear between variables with
    // different domains
    // 9/2014: optimized below for speed: critical
    val classesWithConsts = eqClasses.filter(_.hasConstant)
    for (eqClass1 <- classesWithConsts) {
      val merged = classesWithConsts
        .filter { eqClass2 =>
          eqClass2.hasConstant && eqClass1.exists { eqClass2.contains(_) }
        }
        .flatten
        .toSet
      val differentElems = differentFromTerms(merged)
      if (merged.exists { differentElems.contains(_) }) return true
    }
    return false;
  }

  lazy val constants: Set[Constant] = flatMap { case (_, args) =>
    args.flatMap {
      case c: Constant => List(c)
      case _           => List()
    }
  }.toSet

  def differentFromTerms(elements: Set[Term]): Set[Term] = {
    elements.flatMap {
      _ match {
        case v: Var => self(v)
        case _      => Set[Term]()
      }
    }
  }

  def differentFromVars(elements: Set[Var]): Set[Term] = {
    elements.flatMap { self(_) }
  }

  override def empty = IneqConstr.empty

  override val hashCode = super.hashCode

  def inconsistent = {
    this.exists { tuple: (Var, Set[Term]) => tuple._2.contains(tuple._1) }
  }

  def join(other: IneqConstr) = {
    if (this eq other) this
    else {
      val joinedMaps = this ++ other
      new IneqConstr(map2MultiMap(joinedMaps))
    }
  }

  def joinOverlapped(ineqs: List[(Var, Term)]) = {
    ineqs.foldLeft(this)((ineqConstrs, ineq) =>
      ineqConstrs + (ineq._1, ineq._2)
    )
  }

  def inverseSubstitution(c: Constant, v: Var): IneqConstr = {
    var vIneqs: List[Var] = Nil
    var tuples = for ((variable, argSet) <- this) yield {
      val substSet = argSet.map {
        _ match {
          case `c` => {
            vIneqs = variable :: vIneqs
            v
          }
          case t => t
        }
      }
      (variable, substSet)
    }
    if (vIneqs.nonEmpty) tuples += (v -> vIneqs.toSet)
    new IneqConstr(map2MultiMap(tuples))
  }

  def project(variables: Set[Var]): IneqConstr = {
    val projectedKeys = self.filterKeys { variables(_) }
    val projectedMap = projectedKeys.mapValues { set =>
      set.filter { a =>
        a.isInstanceOf[Constant] || variables(a.asInstanceOf[Var])
      }
    }
    val nonEmptyProjection = projectedMap.filter { case (_, v) => v.nonEmpty }
    new IneqConstr(map2MultiMap(nonEmptyProjection))
  }

  def removeRedundant(elemConstr: ElemConstr): IneqConstr = {
    val removedValues = self.map { case (keyVar, terms) =>
      val keyVarDomain = elemConstr(keyVar)
      val nonRedundantTerms = terms.filter {
        _ match {
          case v: Var => !keyVarDomain.disjoint(elemConstr(v))
          case _      => true
        }
      }
      (keyVar, nonRedundantTerms)
    }
    val nonEmptyResult = removedValues.filter { case (_, v) => v.nonEmpty }
    new IneqConstr(map2MultiMap(nonEmptyResult))
  }

  def substitute(substitution: Var.Substitution): IneqConstr = {
    // substitute values
    // beware for modifying the collection you loop over! iterators are not
    // like in Java
    val tuples1 = for ((variable, argSet) <- this) yield {
      val substSet = argSet.map {
        _ match {
          case v: Var      => v.substitute(substitution)
          case c: Constant => c
        }
      }
      (variable, substSet)
    }
    var clone: MultiMap[Var, Term] = new MultiMap(tuples1)
    // substitute keys
    for (variable <- this.keySet) {
      val subst = variable.substitute(substitution)
      if (subst != variable) {
        subst match {
          case subsVar: Var => {
            val joinedSets = clone(subsVar) ++ clone(variable)
            val tuple = (subsVar, joinedSets)
            clone += tuple
          }

          // We don't want to end up with a 'constant != constant' constraint.
          // In case of such a constraint, the entire substitution is invalid.
          case subsConstant: Constant =>
            require(clone(variable).forall(!_.equals(subsConstant)))
        }
        clone -= variable
      }
    }
    new IneqConstr(map2MultiMap(clone))
  }

  override def toString = toString(ToStringNameSpace)

  def toString(nameSpace: NameSpace[Var, String], ineqSymbol: String = "≠") = {
    flatMap { case (v, args) =>
      val vName = nameSpace.getName(v)
      // order to remove X!=Y and Y!=X constraints
      val orderedArgs = args.filter { a =>
        !a.isInstanceOf[Var] || a.hashCode > v.hashCode
      }
      orderedArgs.map { arg =>
        val argName = arg match {
          case variable: Var      => nameSpace.getName(variable)
          case constant: Constant => constant.toString
        }
        vName + ineqSymbol + argName
      }
    }.toArray.sorted.mkString(", ")
  }

  def toFastWfomc(namespace: NameSpace[Var, String], ineqSymbol: String = "≠") =
    toString(namespace, ineqSymbol).toLowerCase

  def variables = keySet

}

object IneqConstr {

  val empty: IneqConstr = new IneqConstr

  def apply(ineqs: (Var, Term)*): IneqConstr = {
    var map = MultiMap.empty[Var, Term]
    for ((v, a) <- ineqs) {
      map += (v, a)
      if (a.isInstanceOf[Var]) {
        map += (a.asInstanceOf[Var], v)
      }
    }
    map
  }

  implicit def multiMap2IneqConstr(map: MultiMap[Var, Term]): IneqConstr = {
    new IneqConstr(map)
  }

}
