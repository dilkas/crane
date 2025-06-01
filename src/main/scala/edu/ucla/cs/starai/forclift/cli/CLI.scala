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

package edu.ucla.cs.starai.forclift.cli

import java.io._
import java.lang.System._
import scala.collection.JavaConverters._
import scala.io._
import org.clapper.argot._
import upickle.default._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.compiler.rulesets._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.languages._
import edu.ucla.cs.starai.forclift.learning.structure.StructureLearner
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.learning.Likelihood
import edu.ucla.cs.starai.forclift.languages.focnf._

object CLI extends App {

  case class Instance(
      formula: String,
      // Int can be replaced with anything else here: it's always empty
      cardinalities: List[Int],
      weights: Map[String, List[Double]]
  )

  assertFalse()

  val argumentParser = new ArgotParser(
    "Crane",
    false,
    80,
    Some("Version 2.0"),
    Some("""
EXAMPLE

java -jar target/scala-2.11/crane-assembly-1.0.jar -n --format-in mln ./models/friendsmoker.mln
"""),
    true
  )

  val debugCLI = new DebugCLI(argumentParser)
  val inputCLI = new InputCLI(argumentParser, debugCLI)
  val inferenceCLI = new InferenceCLI(argumentParser, debugCLI, inputCLI)
  val learningCLI = new LearningCLI(argumentParser, debugCLI, inputCLI)
  val outputCLI = new OutputCLI(argumentParser, debugCLI, inputCLI)

  /* PARSE FLAGS AND HANDLE LOGIC */

  try {
    argumentParser.parse(args)

    debugCLI.runDebugging(inputCLI)
    inferenceCLI.runInference()
    learningCLI.runLearning()
    outputCLI.runOutput()

  } catch {
    case e: ArgotUsageException =>
      println(e.message)
      System.exit(1)
    case e: SkolemizationFinishedException =>
      implicit val ownerRw: ReadWriter[Instance] = macroRW[Instance]
      val p = new java.io.PrintWriter(e.filename)
      try {
        p.write(
          write(
            Instance(
              e.formula,
              List(),
              inputCLI.wcnfModel.predicateWeights.toFastWfomc
            ),
            indent = 4
          )
        )
      } finally {
        p.close()
      }
      System.exit(0)
  }

  def assertFalse() =
    assert(false, "Assertions are enabled in CLI: check compiler flags")

}
