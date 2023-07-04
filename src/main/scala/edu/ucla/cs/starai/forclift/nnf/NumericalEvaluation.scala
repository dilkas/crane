package edu.ucla.cs.starai.forclift.nnf

import scala.sys.process._
import System._
import java.io._
import java.nio.file.{Paths, Files}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.Domain

object NumericalEvaluation {
	val CPP_COMPILER : String = "g++"
	val COMPILE_FLAGS : String = "-O3"
	val OBJ_FILE_PATH : String = "bin/test.exe"
	val CODE_FILE_PATH : String = "src/main/cpp/test.cpp"
	val PARSER_BIN_PATH : String = "bin/shunting_yard.exe"
	val PARSER_CODE_PATH : String = "src/main/cpp/shunting_yard.cpp"
	val IN_FILE_PATH : String = "bin/equations.in"

	def generate_cpp_code(equations : Array[String], target : String) : Unit = {
		//creating the input file
		val in_file = new File(IN_FILE_PATH)
		val in_file_writer = new FileWriter(in_file)
		val buffered_in_file_writer = new BufferedWriter(in_file_writer)
		buffered_in_file_writer.write(equations.size.toString() + "\n" + equations.mkString("\n") + "\n" + target)
		buffered_in_file_writer.flush()
		//compiling and running the parser
		val parser_bin = new File(PARSER_BIN_PATH)
		if (!parser_bin.exists() || !parser_bin.isFile){
			println("Compiling the parser ....")
			val compile_cmd : Seq[String] = Seq(CPP_COMPILER, COMPILE_FLAGS, PARSER_CODE_PATH, "-o", PARSER_BIN_PATH)
			val compile_proc : ProcessBuilder = Process(compile_cmd, Some(new java.io.File(".")))
			val compile_out : ListBuffer[String] = ListBuffer()
			val compile_err : ListBuffer[String] = ListBuffer()
			compile_proc ! ExternalBinaries.stringLogger(compile_out, compile_err)
			if (compile_err.size != 0){
				throw new Exception("Parser compile failed - \n" + compile_err.mkString("\n"))
			}
		}
		//running the executable and storing its output
		val exec_cmd : Seq[String] = Seq(PARSER_BIN_PATH, IN_FILE_PATH)
		val exec_proc : ProcessBuilder = Process(exec_cmd, Some(new java.io.File(".")))
		val exec_out : ListBuffer[String] = ListBuffer()
		val exec_err : ListBuffer[String] = ListBuffer()
		exec_proc ! ExternalBinaries.stringLogger(exec_out, exec_err)
		if (exec_err.size != 0){
			throw new Exception("Parser execution failed - \n" + exec_err.mkString("\n"))
		}
        exec_out.foreach(println(_))
		val code_file = new File(CODE_FILE_PATH)
		val code_file_writer = new FileWriter(code_file)
		val buffered_code_file_writer = new BufferedWriter(code_file_writer)
		buffered_code_file_writer.write(exec_out.mkString("\n"))
		buffered_code_file_writer.flush()
	}

    def generate_cpp_code(wcnf : WeightedCNF, var_domain_map : Map[String, Domain], equations : Array[String], target : Option[String] = None) : Unit = {
		var new_equations : Array[String] = new Array[String](equations.length)
        equations.copyToArray(new_equations)
        new_equations = new_equations.map(eqn => {
            var new_eqn : String = eqn
            new_eqn = new_eqn.replaceAll(" ", "")
            new_eqn = new_eqn.replaceAll("\\{", "")
            new_eqn = new_eqn.replaceAll("\\}", "")
            new_eqn = new_eqn.replaceAll("\\.", "")
            new_eqn
        })
		var target_str : String = target match{
			case Some(x) => "f0(" + x + ")"
			case None => {
				var target_str = "f0("
				for (eqn <- new_equations){
					val index_of_equals : Int = eqn.indexOf('=')
					val lhs : FuncCall = new FuncCall(eqn.substring(0, index_of_equals).replaceAll(" ", ""))
					if (lhs.func_name == "f0"){
						var is_base_call : Boolean = true
						for (arg <- lhs.args){
							if (arg.terms.size != 1 || !arg.terms(0)._1 || !arg.terms(0)._2.matches("x[0-9]+"))
								is_base_call = false
						}
						if (is_base_call){
							for(arg <- lhs.args){
								val dom_size : Int = wcnf.domainSizes(var_domain_map(arg.terms(0)._2)).size
								target_str += dom_size.toString() + ","
							}
							target_str = target_str.substring(0, target_str.size - 1) + ")"
						}
					}
				}
				target_str
			}
		}
        return generate_cpp_code(new_equations, target_str)
    }

    def get_numerical_answer() : Int = {
        val compile_cmd : Seq[String] = Seq(CPP_COMPILER, COMPILE_FLAGS, CODE_FILE_PATH, "-o", OBJ_FILE_PATH)
        val compile_proc : ProcessBuilder = Process(compile_cmd, Some(new java.io.File(".")))
        val compile_out : ListBuffer[String] = ListBuffer()
        val compile_err : ListBuffer[String] = ListBuffer()
        compile_proc ! ExternalBinaries.stringLogger(compile_out, compile_err)
        if (compile_err.size != 0){
            throw new Exception("Code compile failed - \n" + compile_err.mkString("\n"))
        }
        val exec_cmd : Seq[String] = Seq(OBJ_FILE_PATH)
		val exec_proc : ProcessBuilder = Process(exec_cmd, Some(new java.io.File(".")))
		val exec_out : ListBuffer[String] = ListBuffer()
		val exec_err : ListBuffer[String] = ListBuffer()
		exec_proc ! ExternalBinaries.stringLogger(exec_out, exec_err)
		if (exec_err.size != 0 || exec_out.size != 1){
			throw new Exception("Numerical evaluation failed - \n" + exec_err.mkString("\n"))
		}
        return exec_out(0).toInt
    }
}