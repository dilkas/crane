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

package edu.ucla.cs.starai.forclift.examples.models

import edu.ucla.cs.starai.forclift._

class FriendsSmokerModel(
  nbPeople: Int,
  knownPeople: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends FactorGraphModel {

  def theoryString = (
    "domain Person " + nbPeople + knownPeople.mkString(" {", ",", "}") + "\n" +
    """
predicate smokes(Person)
predicate cancer(Person)
predicate friends(Person,Person)

!smokes(X) v cancer(X) 2 1
!friends(X,Y) v smokes(X) v !smokes(Y) 1.2 1
!friends(X,Y) v !smokes(X) v smokes(Y) 1.2 1
"""
    + evidence.mkString("\n"))

}
