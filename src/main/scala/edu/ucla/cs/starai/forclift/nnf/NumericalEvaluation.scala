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
// TODO (Paulius): instead of underscores, use camel case
object NumericalEvaluation extends LazyLogging {
  val CPP_COMPILER: String = "g++"
  val COMPILE_FLAGS: Array[String] = Array("-w", "-g", "-Wall", "-std=c++17")
  val LINK_FLAGS: Array[String] = Array("-lgmpxx", "-lgmp")
  val OBJ_FILE_PATH: String = "bin/test.exe"
  val CODE_FILE_PATH: String = "bin/test.cpp"
  val PARSER_BIN_PATH: String = "bin/shunting_yard"
  val IN_FILE_PATH: String = "bin/equations.in"
  var domains: List[Domain] = List()

  def generate_cpp_code(equations: Array[String], domainInfo: String): Unit = {
    // creating the input file
    val in_file = new File(IN_FILE_PATH)
    val in_file_writer = new FileWriter(in_file)
    val buffered_in_file_writer = new BufferedWriter(in_file_writer)
    buffered_in_file_writer.write(
      equations.size.toString() + "\n" + equations.mkString(
        "\n"
      ) + "\n" + domainInfo + "\n"
    )
    buffered_in_file_writer.flush()

    // running the executable and storing its output
    val exec_cmd: Seq[String] = Seq(PARSER_BIN_PATH, IN_FILE_PATH)
    val exec_proc: ProcessBuilder =
      Process(exec_cmd, Some(new java.io.File(".")))
    val exec_out: ListBuffer[String] = ListBuffer()
    val exec_err: ListBuffer[String] = ListBuffer()
    exec_proc ! ExternalBinaries.stringLogger(exec_out, exec_err)
    if (exec_err.size != 0) {
      throw new Exception(
        "Parser execution failed - \n" + exec_err.mkString("\n")
      )
    }
    logger.debug("\n********** CPP CODE **********\n")
    exec_out.foreach(logger.debug(_))
    logger.debug("\n********** END OF CPP CODE **********\n")
    val code_file = new File(CODE_FILE_PATH)
    val code_file_writer = new FileWriter(code_file)
    val buffered_code_file_writer = new BufferedWriter(code_file_writer)
    buffered_code_file_writer.write(exec_out.mkString("\n"))
    buffered_code_file_writer.flush()
  }

  def generate_cpp_code(
      wcnf: WeightedCNF,
      var_domain_map: Map[String, Domain],
      equations: Array[String],
      domains: List[Domain]
  ): Unit = {
    this.domains = domains
    var new_equations: Array[String] = new Array[String](equations.length)
    equations.copyToArray(new_equations)
    new_equations = new_equations.map(eqn => {
      var new_eqn: String = eqn
      new_eqn = new_eqn.replaceAll(" ", "")
      new_eqn = new_eqn.replaceAll("\\{", "")
      new_eqn = new_eqn.replaceAll("\\}", "")
      new_eqn = new_eqn.replaceAll("\\.[0-9]*", "")
      new_eqn
    })
    return generate_cpp_code(
      new_equations,
      (domains.size.toString :: domains.map(_.toString)).mkString(" ")
    )
  }

  def get_numerical_answer(domainSizes: DomainSizes): BigInt = {
    val compile_cmd: Seq[String] =
      Seq(CPP_COMPILER) ++ COMPILE_FLAGS.toSeq ++ Seq(
        CODE_FILE_PATH,
        "-o",
        OBJ_FILE_PATH
      ) ++ LINK_FLAGS.toSeq
    val compile_proc: ProcessBuilder =
      Process(compile_cmd, Some(new java.io.File(".")))
    val compile_out: ListBuffer[String] = ListBuffer()
    val compile_err: ListBuffer[String] = ListBuffer()
    compile_proc ! ExternalBinaries.stringLogger(compile_out, compile_err)
    if (compile_err.size != 0) {
      throw new Exception(
        "Code compile failed - \n" + compile_err.mkString("\n")
      )
    }

    val exec_cmd =
      OBJ_FILE_PATH :: domains.map(domain => domainSizes(domain).size.toString)
    val exec_proc: ProcessBuilder =
      Process(exec_cmd, Some(new java.io.File(".")))
    val exec_out: ListBuffer[String] = ListBuffer()
    val exec_err: ListBuffer[String] = ListBuffer()
    exec_proc ! ExternalBinaries.stringLogger(exec_out, exec_err)
    if (exec_err.size != 0 || exec_out.size != 1) {
      throw new Exception(
        "Numerical evaluation failed - \n" + exec_err.mkString("\n")
      )
    }
    return BigInt(exec_out(0))
  }
}
