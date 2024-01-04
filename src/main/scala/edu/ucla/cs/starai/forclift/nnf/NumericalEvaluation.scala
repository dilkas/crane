package edu.ucla.cs.starai.forclift.nnf

import scala.sys.process._
import System._
import java.io._
import java.nio.file.{Paths, Files}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import com.typesafe.scalalogging.LazyLogging

import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.Domain

// TODO (Paulius): use the makefile or something similar
// TODO (Paulius): is the file name still accurate?
object NumericalEvaluation extends LazyLogging {
  // TODO (Paulius): either get rid of these or make them private and adjust the names
  val CPP_COMPILER: String = "g++"
  val COMPILE_FLAGS: Array[String] = Array("-w", "-g", "-Wall", "-std=c++17")
  val LINK_FLAGS: Array[String] = Array("-lgmpxx", "-lgmp")
  val OBJ_FILE_PATH: String = "bin/test.exe"
  val CODE_FILE_PATH: String = "bin/test.cpp"
  val PARSER_BIN_PATH: String = "bin/shunting_yard"
  val IN_FILE_PATH: String = "bin/equations.in"
  var domains: List[Domain] = List()

  // TODO (Paulius): why aren't we closing files?
  private def generateCppCode(
      equations: Array[String],
      domainInfo: String
  ): Unit = {
    // creating the input file
    val writer = new BufferedWriter(new FileWriter(new File(IN_FILE_PATH)))
    writer.write(
      equations.size.toString() + "\n" + equations.mkString(
        "\n"
      ) + "\n" + domainInfo + "\n"
    )
    writer.flush()

    // running the executable and storing its output
    val out: ListBuffer[String] = ListBuffer()
    val err: ListBuffer[String] = ListBuffer()
    Process(
      Seq(PARSER_BIN_PATH, IN_FILE_PATH),
      Some(new java.io.File("."))
    ) ! ExternalBinaries.stringLogger(out, err)
    if (err.size != 0)
      throw new Exception(
        "Parser execution failed - \n" + err.mkString("\n")
      )
    logger.debug("\n********** C++ CODE **********\n")
    out.foreach(logger.debug(_))
    logger.debug("\n********** END OF C++ CODE **********\n")
    val writer2 = new BufferedWriter(new FileWriter(new File(CODE_FILE_PATH)))
    writer2.write(out.mkString("\n"))
    writer2.flush()
  }

  def generateCppCode(
      wcnf: WeightedCNF,
      equations: Array[String],
      domains: List[Domain]
  ): Unit = {
    this.domains = domains
    // TODO (Paulius): there must be a better way to do this without the
    // asignment above and without rewriting the newEquations variable
    var newEquations: Array[String] = new Array[String](equations.length)
    equations.copyToArray(newEquations)
    newEquations = newEquations.map(eqn =>
      eqn
        .replaceAll(" ", "")
        .replaceAll("\\{", "")
        .replaceAll("\\}", "")
        .replaceAll("\\.[0-9]*", "")
    )
    generateCppCode(
      newEquations,
      (domains.size.toString :: domains.map(_.toString)).mkString(" ")
    )
  }

  // TODO (Paulius): extract a method for running processes
  def getNumericalAnswer(domainSizes: DomainSizes): BigInt = {
    val compileCmd: Seq[String] =
      Seq(CPP_COMPILER) ++ COMPILE_FLAGS.toSeq ++ Seq(
        CODE_FILE_PATH,
        "-o",
        OBJ_FILE_PATH
      ) ++ LINK_FLAGS.toSeq
    val compileProc: ProcessBuilder =
      Process(compileCmd, Some(new java.io.File(".")))
    val out: ListBuffer[String] = ListBuffer()
    val err: ListBuffer[String] = ListBuffer()
    compileProc ! ExternalBinaries.stringLogger(out, err)
    if (err.size != 0)
      throw new Exception("Code compile failed - \n" + err.mkString("\n"))

    val execCmd =
      OBJ_FILE_PATH :: domains.map(domain => domainSizes(domain).size.toString)
    val execProc: ProcessBuilder =
      Process(execCmd, Some(new java.io.File(".")))
    val execOut: ListBuffer[String] = ListBuffer()
    val execErr: ListBuffer[String] = ListBuffer()
    execProc ! ExternalBinaries.stringLogger(execOut, execErr)
    if (execErr.size != 0 || execOut.size != 1)
      throw new Exception(
        "Numerical evaluation failed - \n" + execErr.mkString("\n")
      )
    BigInt(execOut(0))
  }

}
