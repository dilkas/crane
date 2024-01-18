package edu.ucla.cs.starai.forclift.nnf

import scala.sys.process._
import System._
import java.io._
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.LazyLogging

import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.util.RunWithTimeout
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.Domain

/** The class responsible for compiling equations into C++ programs as well as
  * compiling and (optionally) running those programs.
  */
class NumericalEvaluator(val domains: List[Domain]) extends LazyLogging {

  // Constants
  private[this] val CompileFlags = Seq("-std=c++17")
  private[this] val CppCompiler = "g++"
  private[this] val LinkFlags = Seq("-lgmpxx", "-lgmp")
  private[this] val ParserBinPath = "bin/shunting_yard"

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
    val out = run("Parser execution", Seq(ParserBinPath, equationsFilename))
    logger.debug("\n********** C++ CODE **********\n")
    out.foreach(logger.debug(_))
    logger.debug("\n********** END OF C++ CODE **********\n")
    val cppFilename = writeToTempFile("crane-", ".cpp", out.mkString("\n"))
    val execFilename = cppFilename.take(cppFilename.lastIndexOf('.'))
    new File(execFilename).deleteOnExit()
    run(
      "Code compilation",
      Seq(CppCompiler) ++ CompileFlags ++ Seq(
        cppFilename,
        "-o",
        execFilename
      ) ++ LinkFlags
    )
    execFilename
  }

  /** Runs the compiled C++ code and extracts the numerical answer. */
  private[this] def getNumericalAnswer(
      execFilename: String,
      timeout: Int,
      arguments: Seq[String]
  ): String = {
    val startTime = System.nanoTime
    def call() = run("Numerical evaluation", execFilename +: arguments)
    RunWithTimeout(timeout)(call()) match {
      case Some(out) => {
        val count =
          BigInt(out(0)).toString.reverse.grouped(3).mkString(",").reverse
        count + " (in " + (System.nanoTime - startTime) / 1000000 + " ms)"
      }
      case None => "TIMEOUT"
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
    val out: ListBuffer[String] = ListBuffer()
    val err: ListBuffer[String] = ListBuffer()
    Process(command, Some(new File("."))) ! ExternalBinaries.stringLogger(
      out,
      err
    )
    if (err.size != 0)
      throw new Exception(name + " failed: \n" + err.mkString("\n"))
    out
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
    file.deleteOnExit()
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(content)
    writer.close()
    file.toString
  }

}
