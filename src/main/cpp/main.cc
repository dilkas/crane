// clang-format off
/* TODO:
 * (1) Add support for functions where a function appears inside another
 *     function, specifically power.
 * (2) Handle the sum function of wolfram (suggestion: define a separate
 *     function for it like power and Binomial). For (2), need to add support
 *     for the {...} list/tuple syntax of wolfram, or replace it beforehand by
 *     something else.
 */
// clang-format on

#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#include "expression.h"
#include "function_call.h"
#include "token.h"

// TODO (Paulius): fix the function name formatting
// TODO (Paulius): fix memory leaks
// TODO (Paulius): smaller functions

/** Generates C++ definitions for the equations */
std::string GenerateFunctionDefinition(std::string equation) {
  std::stringstream code;
  size_t equalSign = equation.find('=');
  std::unique_ptr<FunctionCall> lhs = std::unique_ptr<FunctionCall>(
      FunctionCall::Create(equation.substr(0, equalSign)));
  std::unique_ptr<Expression> rhs =
      Expression(
          equation.substr(equalSign + 1, equation.size() - equalSign - 1))
          .HandlePower();
  std::string signature =
      lhs->GetFunctionSignature("mpz_class", "unsigned long", "");
  code << signature << " {" << std::endl;

  // check if the element is present in the cache
  code << "  mpz_class& stored_val = ";
  std::stringstream stored_val_loc_stream;
  for (unsigned i = 0; i < lhs->func_args.size(); i++)
    stored_val_loc_stream << "get_elem(";
  stored_val_loc_stream << lhs->func_name << "_cache";
  for (unsigned i = 0; i < lhs->func_args.size(); i++)
    stored_val_loc_stream << ", " << lhs->func_args.at(i)->ToString() << ")";
  stored_val_loc_stream << ".n";
  std::string stored_val_loc = stored_val_loc_stream.str();
  code << stored_val_loc << ";" << std::endl
       << "  if (stored_val != -1)" << std::endl
       << "    return stored_val;" << std::endl;

  // find the maximum subtractor among the arguments in the rhs
  std::map<std::string, int> max_sub;
  for (auto const &arg : lhs->func_args)
    if (arg->Front()->GetVariable() != "")
      max_sub.insert({arg->Front()->GetVariable(), 0});
  auto max_sub_vec = rhs->MaxDecrementPerVariable(max_sub, lhs->func_name);

  std::vector<std::string> free_vars;
  for (auto const &exp : lhs->func_args)
    if (exp->Front()->GetVariable() != "")
      free_vars.push_back(exp->Front()->GetVariable());
  auto retriever = [free_vars](const Token &e) {
    return e.ToCppString(free_vars);
  };

  // handling the case where all args are +ve for the call with all variable
  // arguments
  if (!max_sub_vec.empty()) {
    code << "  if (";
    for (unsigned i = 0; i < max_sub_vec.size(); i++) {
      if (i != 0)
        code << " && ";
      code << max_sub_vec.at(i).first << " >= " << max_sub_vec.at(i).second;
    }
    code << ") {" << std::endl
         << "    mpz_class ret_val = " << rhs->ToString(retriever) << ";"
         << std::endl
         << "    " << stored_val_loc << " = ret_val;" << std::endl
         << "    return ret_val;" << std::endl
         << "  }" << std::endl;
  } else {
    code << "  mpz_class ret_val = " << rhs->ToString(retriever) << ";"
         << std::endl
         << "  " << stored_val_loc << " = ret_val;" << std::endl
         << "  return ret_val;" << std::endl;
  }

  // handling the rest of the cases
  for (unsigned i = 0; i < max_sub_vec.size(); i++) {
    for (int sub = 0; sub < max_sub_vec.at(i).second; sub++) {
      auto transformed = lhs->CloneFunctionCall();
      for (unsigned j = 0; j < transformed->func_args.size(); j++) {
        if (transformed->func_args.at(j)->Front()->GetVariable() ==
            max_sub_vec.at(i).first) {
          transformed->func_args.at(j)->SetFront(sub);
          break;
        }
      }
      code << "  else if (" << max_sub_vec.at(i).first << " == " << sub << ") {"
           << std::endl
           << "    return " << transformed->GetFunctionSignature("", "", "")
           << ";" << std::endl
           << "  }" << std::endl;
    }
  }
  code << "  exit(1);" << std::endl << "}";
  return code.str();
}

std::string GenerateCppCode(const std::vector<std::string> &equations) {
  std::stringstream code;
  for (auto &eqn : equations)
    code << Expression(eqn.substr(0, eqn.find('=')))
                .Front()
                ->GetFunctionSignature("mpz_class", "unsigned long")
         << std::endl;
  code << std::endl;
  for (auto &eqn : equations)
    code << GenerateFunctionDefinition(eqn) << std::endl;
  return code.str();
}

std::set<std::pair<std::string, int>>
GetFunctions(const std::vector<std::string> &equations) {
  std::set<std::pair<std::string, int>> functions;
  for (const auto &eqn : equations)
    functions.insert(
        Expression(eqn.substr(0, eqn.find('='))).Front()->GetFunctionPair());
  return functions;
}

std::string GenerateCppCodeWithMain(std::vector<std::string> equations,
                                    const std::vector<std::string> &domains) {
  std::stringstream code;
  code << "#include <array>" << std::endl
       << "#include <chrono>" << std::endl
       << "#include <future>" << std::endl
       << "#include <iostream>" << std::endl
       << "#include <string>" << std::endl
       << "#include <vector>" << std::endl
       << "#include <cmath>" << std::endl
       << "#include <gmpxx.h>" << std::endl
       << std::endl;

  // helper code
  code << "class cache_elem {" << std::endl
       << "public:" << std::endl
       << "  mpz_class n;" << std::endl
       << "  cache_elem(mpz_class x) : n{x} {}" << std::endl
       << "  cache_elem() : n{-1} {}" << std::endl
       << "};" << std::endl
       << std::endl
       << "template <class T> T& get_elem(std::vector<T>& a, size_t n) {"
       << std::endl
       << "  if (n >= a.size()) {" << std::endl
       << "    a.resize(n+1);" << std::endl
       << "  }" << std::endl
       << "  return a.at(n);" << std::endl
       << "}" << std::endl
       << std::endl
       << "mpz_class Binomial(unsigned long n, unsigned long r) {" << std::endl
       << "  mpz_t ans;" << std::endl
       << "  mpz_init(ans);" << std::endl
       << "  mpz_bin_uiui(ans, n, r);" << std::endl
       << "  return mpz_class{ans};" << std::endl
       << "}" << std::endl
       << std::endl
       << "mpz_class power(mpz_class x, unsigned long y) {" << std::endl
       << "  mpz_t ans;" << std::endl
       << "  mpz_init(ans);" << std::endl
       << "  mpz_pow_ui(ans, x.get_mpz_t(), y);" << std::endl
       << "  return mpz_class{ans};" << std::endl
       << "}" << std::endl
       << std::endl;

  // make the caches
  std::set<std::pair<std::string, int>> functions = GetFunctions(equations);
  for (const auto &func : functions) {
    for (int i = 0; i < func.second; i++)
      code << "std::vector<";
    code << "cache_elem";
    for (int i = 0; i < func.second; i++)
      code << ">";
    code << " " << func.first << "_cache;" << std::endl;
  }

  code << std::endl
       << GenerateCppCode(equations) << std::endl
       << "int main(int argc, char *argv[]) {" << std::endl
       << "  if (argc != 3 && argc != " << (domains.size() + 2) << ") {"
       << std::endl
       << "    std::cerr << \"Please provide a timeout value (-1 for no "
          "timeout) and either one domain size to be used for all domains or "
       << domains.size()
       << " domain size(s) for the following domains (in this order): ";

  for (size_t i = 0; i < domains.size(); i++) {
    if (i != 0)
      code << ", ";
    code << domains.at(i);
  }

  code << "\" << std::endl;" << std::endl
       << "    exit(1);" << std::endl
       << "  }" << std::endl
       << "  std::array<unsigned long, " << domains.size() << "> arguments;"
       << std::endl
       << "  for (int i = 2; i <= " << domains.size() + 1 << "; i++)"
       << std::endl
       << "    arguments[i-2] = std::stoul((argc == 3) ? argv[2] : argv[i]);"
       << std::endl
       << "  if (argv[1][0] == '-') {" << std::endl
       << "    std::cout << std::apply(f0, arguments) << std::endl;"
       << std::endl
       << "    return 0;" << std::endl
       << "  }" << std::endl
       << "  auto timeout = std::chrono::seconds(std::stoul(argv[1]));"
       << std::endl
       << "  auto run = [arguments]() {" << std::endl
       << "    return std::apply(f0, arguments);" << std::endl
       << "  };" << std::endl
       << "  std::packaged_task<mpz_class()> task(run);" << std::endl
       << "  auto future = task.get_future();" << std::endl
       << "  std::thread thr(std::move(task));" << std::endl
       << "  if (future.wait_for(timeout) != std::future_status::timeout) {"
       << std::endl
       << "    thr.join();" << std::endl
       << "    std::cout << future.get() << std::endl;" << std::endl
       << "  } else {" << std::endl
       << "    thr.detach();" << std::endl
       << "    std::cout << \"TIMEOUT\" << std::endl;" << std::endl
       << "    return 0;" << std::endl
       << "  }" << std::endl
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
  int num_equations = 0;
  in_file >> num_equations;
  std::vector<std::string> equations;
  equations.resize(num_equations);
  for (int i = 0; i < num_equations; i++)
    in_file >> equations.at(i);

  int num_domains;
  in_file >> num_domains;
  std::vector<std::string> domains;
  for (int i = 0; i < num_domains; i++) {
    std::string domain;
    in_file >> domain;
    domains.push_back(domain);
  }

  std::cout << GenerateCppCodeWithMain(equations, domains) << std::endl;
}
