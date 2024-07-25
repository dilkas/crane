package edu.ucla.cs.starai.forclift.nnf

import java.io._
import java.util.concurrent._
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.math.pow
import ExecutionContext.Implicits.global
import scala.io._
import scala.sys.process._

import com.typesafe.scalalogging.LazyLogging

import edu.ucla.cs.starai.forclift.Domain
import edu.ucla.cs.starai.forclift.inference.DomainSizes

/** The class responsible for compiling equations into C++ programs as well as
  * compiling and (optionally) running those programs.
  */
class NumericalEvaluator(val domains: List[Domain]) extends LazyLogging {

  // Constants
  private[this] val CppCodeGenerator = "bin/shunting_yard"
  private[this] val CppCompiler = "g++"
  private[this] val CompileFlags = Seq("-std=c++17")
  private[this] val LinkFlags = Seq("-lgmpxx", "-lgmp")

  /** Uses the C++ code generator to generate C++ code for the given equations,
    * compiles it, and returns the filename of the executable.
    */
  def generateCppCode(equations: List[String]): String = {
    // Preprocessing input data
    val newEquations = equations.map(
      _.replaceAll(" ", "")
        .replaceAll("\\{", "")
        .replaceAll("\\}", "")
        .replaceAll("\\.[0-9]*", "")
    )
    val domainInfo =
      (domains.size.toString :: domains.map(_.toString)).mkString(" ")
    val equationInfo =
      newEquations.size.toString + "\n" + newEquations.mkString(
        "\n"
      ) + "\n" + domainInfo + "\n"

    val equationsFilename = writeToTempFile("equations-", ".in", equationInfo)
    val out =
      run("C++ code generation", Seq(CppCodeGenerator, equationsFilename))
    logger.debug("\n********** C++ CODE **********\n")
    out.foreach(logger.debug(_))
    logger.debug("\n********** END OF C++ CODE **********\n")
    val cppFilename = writeToTempFile("crane-", ".cpp", out.mkString("\n"))
    val execFilename = cppFilename.take(cppFilename.lastIndexOf('.'))
    run(
      CppCompiler,
      Seq(CppCompiler) ++ CompileFlags ++ Seq(
        cppFilename,
        "-o",
        execFilename
      ) ++ LinkFlags
    )
    new File(execFilename).deleteOnExit()
    execFilename
  }

  // TODO (Paulius): mention that timeout is in seconds and a negative timeout
  // means no timeout. Even better, make it an Option[Int]. Also, note that
  // timeout is measured in two places: here and in the C++ code.
  /** Runs the compiled C++ code and extracts the numerical answer. */
  private[this] def getNumericalAnswer(
      execFilename: String,
      timeout: Int,
      arguments: Seq[String]
  ): String = {
    val startTime = System.nanoTime
    val out = run("C++ code", execFilename +: timeout.toString +: arguments)(0)
    val endTime = System.nanoTime
    val outOfTime = timeout >= 0 && endTime - startTime > timeout * pow(10, 9)
    if (out == "TIMEOUT" || outOfTime) {
      "TIMEOUT"
    } else {
      out + " (in " + (System.nanoTime - startTime) / 1000000 + " ms)"
    }
  }

  def getNumericalAnswer(
      execFilename: String,
      timeout: Int,
      domainSizes: DomainSizes
  ): String = getNumericalAnswer(
    execFilename,
    timeout,
    domains.map(domain => domainSizes(domain).size.toString)
  )
  def getNumericalAnswer(
      execFilename: String,
      timeout: Int,
      domainSize: Int
  ): String =
    getNumericalAnswer(execFilename, timeout, Seq(domainSize.toString))

  /** Runs the given command and returns its output.
    *
    * @param name
    *   The descriptive name of the process being executed (used for error
    *   messages)
    */
  private[this] def run(
      name: String,
      command: Seq[String]
  ): ListBuffer[String] = {
    logger.debug(s"Running $name: ${command.mkString(" ")}")
    val stdout = ListBuffer[String]()
    val stderr = ListBuffer[String]()
    val process =
      command.run(ProcessLogger(line => stdout += line, line => stderr += line))

    // Wait for the process to finish
    val exitCode = process.exitValue()

    if (exitCode != 0 || stderr.nonEmpty)
      throw new RuntimeException(name + " failed:\n" + stderr.mkString("\n"))
    stdout
  }

  /** Writes the provided content String to a temporary file with the given
    * prefix and suffix and returns the filename of the temporary file (as a
    * String).
    */
  private[this] def writeToTempFile(
      prefix: String,
      suffix: String,
      content: String
  ): String = {
    val file = File.createTempFile(prefix, suffix)
    logger.debug("Writing to " + file + "...")
    file.deleteOnExit()
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(content)
    writer.close()
    file.toString
  }

}
