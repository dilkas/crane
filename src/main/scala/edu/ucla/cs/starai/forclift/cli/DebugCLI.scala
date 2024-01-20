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
import edu.ucla.cs.starai.forclift.nnf.NumericalEvaluator
import edu.ucla.cs.starai.forclift.util.RunWithTimeout

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

  val timeoutFlag = argumentParser.option[Int](
    List("t", "timeout"),
    "integer",
    "Timeout (in seconds) for compilation and for running the C++ program (separately, default: unlimited)"
  )
  def timeout = timeoutFlag.value.getOrElse(-1)

  val maxDomainSizeFlag = argumentParser.option[Int](
    List("d", "domain-size"),
    "integer",
    "-d <n> : The compiled C++ program will run on domain sizes 2^0, 2^1, 2^2,..., 2^n (setting all domain sizes to be equal to the same value)."
  )
  def maxDomainSize = maxDomainSizeFlag.value.getOrElse(-1)

  val maxDomainSizeFlag2 = argumentParser.option[Int](
    List("e"),
    "integer",
    "-e <n> : The compiled C++ program will run on domain sizes 1, 2, 3, ..., n (setting all domain sizes to be equal to the same value)."
  )
  def maxDomainSize2 = maxDomainSizeFlag2.value.getOrElse(-1)

  def runDebugging(inputCLI: InputCLI): Unit = {
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

    def run(): (NumericalEvaluator, String) = {
      val expandedEquations =
        inputCLI.wcnfModel.asEquations._1.withSumsExpanded.equations
      logger.debug("")
      expandedEquations.foreach { logger.debug(_) }
      val evaluator = new NumericalEvaluator(inputCLI.parser.domains.reverse)
      val execFilename = evaluator.generateCppCode(expandedEquations)
      (evaluator, execFilename)
    }

    val startTime = System.nanoTime
    RunWithTimeout(timeout)(run()) match {
      case None => logger.info("Compilation timed out")
      case Some((evaluator, execFilename)) => {
        logger.info(
          "Compilation time: " + (System.nanoTime - startTime) / 1000000 + " ms"
        )

        if (numerical)
          logger.info(
            "Model count: " + evaluator.getNumericalAnswer(
              execFilename,
              timeout,
              inputCLI.wcnfModel.domainSizes
            )
          )

        if (maxDomainSize > 0) {
          for (i <- 0 to maxDomainSize) {
            val n = scala.math.pow(2, i).toInt
            val count = evaluator.getNumericalAnswer(execFilename, timeout, n)
            logger.info(
              "The model count for domain(s) of size " + n + ": " + count
            )
            if (count == "TIMEOUT")
              return
          }
        }

        if (maxDomainSize2 > 0) {
          for (n <- 1 to maxDomainSize2) {
            val count = evaluator.getNumericalAnswer(execFilename, timeout, n)
            logger.info(
              "The model count for domain(s) of size " + n + ": " + count
            )
            if (count == "TIMEOUT")
              return
          }
        }
      }
    }
  }

}
