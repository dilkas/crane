/*
    TODO :
    (1) Add support for functions where a function appears inside another
   function, specifically power. (2) Handle the sum function of wolfram
   (suggestion : define a separate function for it like power and Binomial) For
   (2), need to add support for the {...} list/tuple syntax of wolfram, or
   replace it beforehand  by something else.
*/

// TODO (Paulius): remove unnecessary includes. Make sure that all the other
// files have appropriate includes.
#include <algorithm>
#include <cctype>
#include <fstream>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <utility>
#include <vector>

#include "expression.h"
#include "function_call.h"
#include "token.h"

// TODO (Paulius): two classes and a bunch of functions. Can they be
// reorganised in a better way?

// TODO (Paulius): fix the function name formatting

// generates c++ definitions for the equations
std::string generate_function_def(std::string eqn) {
  std::stringstream code;
  size_t loc = eqn.find('=');
  Token lhs = Expression(eqn.substr(0, loc)).Front();
  std::unique_ptr<Expression> rhs =
      Expression(eqn.substr(loc + 1, eqn.size() - loc - 1)).HandlePower();
  std::string signature =
      lhs.GetFunctionSignature("mpz_class", "unsigned long", "");
  code << signature << "{\n";

  // check if the element is present in the cache
  code << "\tmpz_class& stored_val = ";
  std::stringstream stored_val_loc_stream;
  for (unsigned i{0}; i < lhs.func().func_args.size(); i++) {
    stored_val_loc_stream << "get_elem(";
  }
  stored_val_loc_stream << lhs.func().func_name << "_cache";
  for (unsigned i{0}; i < lhs.func().func_args.size(); i++) {
    stored_val_loc_stream << ", " << lhs.func().func_args.at(i)->ToString()
                          << ")";
  }
  stored_val_loc_stream << ".n";
  std::string stored_val_loc = stored_val_loc_stream.str();
  code << stored_val_loc;
  code << ";\n\tif (stored_val != -1)\n\t\treturn stored_val;\n";

  // find the maximum subtractor among the arguments in the rhs
  std::map<std::string, int> max_sub;
  for (auto const &arg : lhs.func().func_args)
    if (arg->Front().type == TokenType::kVariable)
      max_sub.insert({arg->Front().var(), 0});
  auto max_sub_vec = rhs->MaxDecrementPerVariable(max_sub);

  // handling the case where all args are +ve for the call with all variable
  // arguments
  std::vector<std::string> free_vars;
  for (auto const &exp : lhs.func().func_args) {
    if (exp->Front().type == TokenType::kVariable)
      free_vars.push_back(exp->Front().var());
  }
  if (max_sub_vec.size()) {
    code << "\tif (";
    for (unsigned i{0}; i < max_sub_vec.size(); i++) {
      if (i != 0)
        code << " && ";
      code << max_sub_vec.at(i).first << " >= " << max_sub_vec.at(i).second;
    }
    code << "){\n\t\tmpz_class ret_val = "
         << rhs->ToString([free_vars](Token &e) {
              if (e.func().func_name == "Binomial" ||
                  e.func().func_name == "power" || e.func().func_name == "Sum")
                return e.func().toCppString(free_vars);
              return e.func().toString(true);
            })
         << ";\n\t\t" << stored_val_loc
         << " = ret_val;\n\t\treturn ret_val;\n\t}\n";
  } else {
    code << "\tmpz_class ret_val = " << rhs->ToString([free_vars](Token &e) {
      if (e.func().func_name == "Binomial" || e.func().func_name == "power" ||
          e.func().func_name == "Sum")
        return e.func().toCppString(free_vars);
      return e.func().toString(true);
    }) << ";\n\t"
         << stored_val_loc << " = ret_val;\n\treturn ret_val;\n";
  }

  // handling the rest of the cases
  for (unsigned i = 0; i < max_sub_vec.size(); i++) {
    for (int sub = 0; sub < max_sub_vec.at(i).second; sub++) {
      code << "\telse if (" << max_sub_vec.at(i).first << " == " << sub << "){";
      code << "\n\t\treturn "
           << lhs.toString([max_sub_vec, i, sub](const Token &e) {
                Token transformed_e = e;
                for (unsigned j = 0; j < transformed_e.func().func_args.size();
                     j++) {
                  if (transformed_e.func().func_args.at(j)->Front().type ==
                          TokenType::kVariable &&
                      transformed_e.func().func_args.at(j)->Front().var() ==
                          max_sub_vec.at(i).first) {
                    transformed_e.func().func_args.at(j)->SetFront(sub);
                    break;
                  }
                }
                return transformed_e.GetFunctionSignature("", "", "");
              })
           << ";\n\t}\n";
    }
  }
  code << "\treturn -1;\n}";
  return code.str();
}

std::string generate_cpp_code(std::vector<std::string> equations) {
  std::stringstream code;
  for (auto &eqn : equations) {
    code << Expression(eqn.substr(0, eqn.find('=')))
                .Front()
                .GetFunctionSignature("mpz_class", "unsigned long")
         << "\n";
  }
  code << "\n";
  for (auto &eqn : equations) {
    code << generate_function_def(eqn) << "\n";
  }
  return code.str();
}

std::set<std::pair<std::string, int>>
get_functions(std::vector<std::string> equations) {
  std::set<std::pair<std::string, int>> functions;
  for (std::string eqn : equations) {
    size_t loc = eqn.find('=');
    Token lhs = Expression(eqn.substr(0, loc)).Front();
    functions.insert({lhs.func().func_name, lhs.func().func_args.size()});
  }
  return functions;
}

std::string
generate_cpp_code_with_main(std::vector<std::string> equations,
                            const std::vector<std::string> &domains) {
  std::stringstream code;
  code << "#include <array>" << std::endl
       << "#include <iostream>" << std::endl
       << "#include <string>" << std::endl
       << "#include <vector>" << std::endl
       << "#include <cmath>" << std::endl
       << "#include <gmpxx.h>" << std::endl
       << std::endl;

  // helper code
  code
      << "class cache_elem{\npublic : \n\tmpz_class n;\n\tcache_elem(mpz_class "
         "x) : n{x} {}\n\tcache_elem() : n{-1} {}\n};\n\n";
  code << "template <class T> T& get_elem(std::vector<T>& a, size_t n){\n\tif "
          "(n >= a.size()){\n\t\ta.resize(n+1);\n\t}\n\treturn a.at(n);\n}\n\n";
  code << "mpz_class Binomial(unsigned long n, unsigned long r){\n\tmpz_t "
          "ans;\n\tmpz_init(ans);\n\tmpz_bin_uiui(ans, n, r);\n\treturn "
          "mpz_class{ans};}\n\n";
  code << "mpz_class power(mpz_class x, unsigned long y){\n\tmpz_t "
          "ans;\n\tmpz_init(ans);\n\tmpz_pow_ui(ans, x.get_mpz_t(), "
          "y);\n\treturn mpz_class{ans};\n}\n\n";

  // make the caches
  std::set<std::pair<std::string, int>> functions = get_functions(equations);
  for (const auto &func : functions) {
    for (int i{0}; i < func.second; i++) {
      code << "std::vector<";
    }
    code << "cache_elem";
    for (int i{0}; i < func.second; i++) {
      code << ">";
    }
    code << " " << func.first << "_cache"
         << ";\n";
  }
  code << "\n";
  code << generate_cpp_code(equations) << std::endl;

  code << "int main(int argc, char *argv[]) {" << std::endl
       << "\tif (argc != " << domains.size() << " + 1) {" << std::endl
       << "\t\tstd::cerr << \"Please provide " << domains.size()
       << " arguments for the following domains (in this order): ";
  for (int i = 0; i < domains.size(); i++) {
    if (i != 0)
      code << ", ";
    code << domains.at(i);
  }
  code << "\" << std::endl;" << std::endl
       << "\t\texit(1);" << std::endl
       << "\t}" << std::endl
       << "\tstd::array<unsigned long, " << domains.size() << "> arguments;"
       << std::endl
       << "\tfor (int i = 1; i < argc; i++)" << std::endl
       << "\t\targuments[i-1] = std::stoul(argv[i]);" << std::endl
       << "\tstd::cout << std::apply(f0, arguments) << std::endl;" << std::endl
       << "}" << std::endl;

  return code.str();
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Specify the input filename as the first argument."
              << std::endl;
    exit(1);
  }
  std::ifstream in_file{argv[1]};
  int num_equations{0};
  in_file >> num_equations;
  std::vector<std::string> equations;
  equations.resize(num_equations);
  for (unsigned i{0}; i < num_equations; i++) {
    in_file >> equations.at(i);
  }

  int num_domains;
  in_file >> num_domains;
  std::vector<std::string> domains;
  for (int i = 0; i < num_domains; i++) {
    std::string domain;
    in_file >> domain;
    domains.push_back(domain);
  }

  std::cout << generate_cpp_code_with_main(equations, domains) << std::endl;
}
