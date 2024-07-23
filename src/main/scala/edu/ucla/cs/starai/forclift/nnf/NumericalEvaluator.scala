package edu.ucla.cs.starai.forclift.nnf

import java.io._
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.io._
import scala.sys.process._

import com.typesafe.scalalogging.LazyLogging

import edu.ucla.cs.starai.forclift.Domain
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.util.RunWithTimeout

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

  // TODO (Paulius): mention that timeout is in seconds and a negative timeout
  // means no timeout. Even better, make it an Option[Int].
  /** Runs the compiled C++ code and extracts the numerical answer. */
  private[this] def getNumericalAnswer(
      execFilename: String,
      timeout: Int,
      arguments: Seq[String]
  ): String = {
    val startTime = System.nanoTime
    try {
      val out = run("Numerical evaluation", execFilename +: arguments, timeout)
      val count =
        BigInt(out(0)).toString.reverse.grouped(3).mkString(",").reverse
      count + " (in " + (System.nanoTime - startTime) / 1000000 + " ms)"
    } catch {
      case e: TimeoutException => "TIMEOUT"
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

  // TODO (Paulius): describe the timeout feature
  /** Runs the given command and returns its output.
    *
    * @param name
    *   The descriptive name of the process being executed (used for error
    *   messages)
    */
  private[this] def run(
      name: String,
      command: Seq[String],
      timeout: Int = -1
  ): ListBuffer[String] = {
    val stdout = ListBuffer[String]()
    val stderr = ListBuffer[String]()
    val process =
      command.run(ProcessLogger(line => stdout += line, line => stderr += line))
    val thread = new Thread {
      override def run() {
        process.destroy()
      }
    }
    Runtime.getRuntime.addShutdownHook(thread)

    // Timeout handling
    if (timeout >= 0) {
      val future = Future(blocking(process.exitValue))
      try {
        Await.result(future, duration.Duration(timeout, "sec"))
      } catch {
        case _: TimeoutException =>
          process.destroy
          throw new TimeoutException(s"$name timed out after $timeout seconds")
      }
    } else process.exitValue

    Runtime.getRuntime.removeShutdownHook(thread)
    if (stderr.size != 0)
      throw new Exception(name + " failed: \n" + stderr.mkString("\n"))
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
    file.deleteOnExit()
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(content)
    writer.close()
    file.toString
  }

}
