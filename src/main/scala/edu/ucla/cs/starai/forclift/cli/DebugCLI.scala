/*
 * Copyright 2023 Paulius Dilkas (National University of Singapore)
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

import com.typesafe.scalalogging.LazyLogging
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.ArgotParser
import org.clapper.argot.FlagOption
import org.clapper.argot.SingleValueOption
import org.clapper.argot.SingleValueParameter
import org.slf4j.LoggerFactory
import edu.ucla.cs.starai.forclift.propositional.C2DError
import edu.ucla.cs.starai.forclift.languages.StatRelModel
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.languages.ModelConverters._
import edu.ucla.cs.starai.forclift.propositional.DimacsCNF
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.nnf.visitors.MainOutputVisitor
import edu.ucla.cs.starai.forclift.nnf.Equations
import edu.ucla.cs.starai.forclift.nnf.NumericalEvaluation

/** Handle all debugging logic for CLI
  */
class DebugCLI(argumentParser: ArgotParser) extends LazyLogging {

  val TraceConfig = "src/main/resources/logback/trace.xml"
  val DebugConfig = "src/main/resources/logback/debug.xml"
  val InfoConfig = "src/main/resources/logback/info.xml"

  /* DEBUGGING FLAGS */

  val helpFlag = argumentParser.flag[Any](List("h", "help"), "This help.") {
    (s, opt) => argumentParser.usage("")
  }

  val verbosityFlag = argumentParser.option[Int](
    List("v", "verbosity"),
    "integer",
    "Verbosity level (0, 1, or 2; default 0)"
  )
  def verbosity = verbosityFlag.value.getOrElse(0)
  def verbose = verbosity > 0

  val verifyWmcFlag = argumentParser.flag[Boolean](
    List("verify"),
    "Verify the result of wfomc using the UCLA c2d compiler. The c2d compiler command can be set with environment variable C2DCMD (default: ./c2d_linux)."
  )
  def verifyWmc = verifyWmcFlag.value.getOrElse(false)

  val showNNFFlag = argumentParser.flag[Boolean](
    List("pdf"),
    "Create a pdf visualizing the NNF circuit. Requires pdflatex and graphviz dot to be in your path and the dot2texi package installed."
  )
  def showNNF = showNNFFlag.value.getOrElse(false)

  val showGroundingFlag = argumentParser
    .flag[Boolean](List("ground"), "Show a ground CNF for the model.")
  def showGrounding = showGroundingFlag.value.getOrElse(false)

  val numericalFlag = argumentParser.flag[Boolean](
    List("n", "numerical"),
    "Compute the model count numerically by running the C++ program with the domain sizes provided as part of the instance."
  )
  def numerical = numericalFlag.value.getOrElse(false)

  def runDebugging(inputCLI: InputCLI) {
    // manage verbosity levels
    val context = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext]
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(context)
      context.reset()
      if (verbosity == 0) {
        configurator.doConfigure(InfoConfig)
      } else if (verbosity == 1) {
        configurator.doConfigure(DebugConfig)
      } else {
        configurator.doConfigure(TraceConfig)
      }
    } catch {
      case _: JoranException =>
    }
    StatusPrinter.printInCaseOfErrorsOrWarnings(context)

    if (showGrounding) {
      logger.info("Ground model:")
      logger.info(inputCLI.model.ground + "\n")
    }

    if (verifyWmc) {
      inputCLI.wcnfModel.verifyLogWmc
      if (inputCLI.hasQuery) {
        val wcnfQuery = inputCLI.wcnfModel.addConstraint(inputCLI.query)
        wcnfQuery.verifyLogWmc
      }
    }

    if (showNNF) {
      // set some default parameter that have no flags
      val compact = true;
      val maxDepth = Integer.MAX_VALUE
      inputCLI.wcnfModel.showNnfPdf(
        compact,
        maxDepth,
        "theory.nnf",
        verbose = verbose
      )
      inputCLI.wcnfModel.showSmoothNnfPdf(
        compact,
        maxDepth,
        "theory.smooth.nnf",
        verbose = verbose
      )
      if (inputCLI.hasQuery) {
        val wcnfQuery = inputCLI.wcnfModel.addConstraint(inputCLI.query)
        wcnfQuery.showNnfPdf(compact, maxDepth, "query.nnf", verbose = verbose)
        wcnfQuery.showSmoothNnfPdf(
          compact,
          maxDepth,
          "query.smooth.nnf",
          verbose = verbose
        )
      }
    }

    // Print the final set of equations (assuming a sufficient verbosity level)
    val expandedEquations = inputCLI.wcnfModel.asEquations.map(eqn =>
      Equations.expandEquation(eqn.replaceAll(" ", ""))
    )
    logger.debug("")
    expandedEquations.foreach { logger.debug(_) }

    NumericalEvaluation.generateCppCode(
      inputCLI.wcnfModel,
      expandedEquations.toArray,
      inputCLI.parser.domains.reverse
    )

    // Run the C++ program and get the output
    if (numerical) {
      logger.info("Compilation finished. Running the C++ program...")
      val ans: BigInt =
        NumericalEvaluation.getNumericalAnswer(inputCLI.wcnfModel.domainSizes)
      // Format the BigInt with commas
      val formattedNumber =
        ans.toString().reverse.grouped(3).mkString(",").reverse
      logger.info("Model count: " + formattedNumber)
    }
  }
}
