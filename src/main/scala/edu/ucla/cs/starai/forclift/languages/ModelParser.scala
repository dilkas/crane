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

package edu.ucla.cs.starai.forclift.languages

import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.Domain
import edu.ucla.cs.starai.forclift.PositiveUnitClause

/**
 * A parser supporting one of the input file types.
 */
trait ModelParser {

  /**
   * The domains of the instance in the order in which they were defined
   */
  var domains: List[Domain] = List()

  /**
   * Parse a string into a supported model.
   */
  def parseModel(theoryString: String): StatRelModel
  
  /**
   * Parse an atom using the same vocabulary as parseModel
   */
  def parseAtom(atomString: String): Atom
  
}
