#include "function_call.h"

#include <sstream>

#include "expression.h"

FunctionCall::FunctionCall(const std::string &func_name,
                           const std::vector<Expression *> &func_args)
    : func_name(func_name) {
  for (auto const &arg : func_args)
    this->func_args.push_back(std::unique_ptr<Expression>(arg));
}

FunctionCall *FunctionCall::Create(std::string call_str) {
  int num_open_brack = -1;
  int arg_start = 0;
  std::string func_name;
  std::vector<Expression *> func_args;
  for (unsigned i = 0; i < call_str.size(); i++) {
    if (call_str.at(i) == '[') {
      if (num_open_brack == -1) {
        func_name = call_str.substr(0, i);
        arg_start = i + 1;
      }
      num_open_brack += 1;
    } else if (call_str.at(i) == ',' || call_str.at(i) == ']') {
      if (num_open_brack == 0) {
        std::string arg = call_str.substr(arg_start, i - arg_start);
        if (arg != "")
          func_args.push_back(new Expression(arg));
        arg_start = i + 1;
      }
      if (call_str.at(i) == ']')
        num_open_brack -= 1;
    }
  }

  if (func_name == "Binomial" || func_name == "power")
    return new OtherFunctionCall(func_name, func_args);
  if (func_name == "Sum")
    return new SumFunctionCall(func_name, func_args);
  return new RealFunctionCall(func_name, func_args);
}

std::string FunctionCall::ToString(bool use_paren /*= false*/) {
  std::string arg_str = "";
  for (auto &arg : func_args)
    arg_str += arg->ToString() + ",";
  if (use_paren)
    return func_name + "(" + arg_str.substr(0, arg_str.size() - 1) + ")";
  return func_name + "[" + arg_str.substr(0, arg_str.size() - 1) + "]";
}

// TODO (Paulius): remove duplicates
FunctionCall *OtherFunctionCall::Clone() {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new OtherFunctionCall(func_name, new_func_args);
}

std::string OtherFunctionCall::ToCppString(std::vector<std::string> free_vars) {
  std::stringstream cpp_exp;
  cpp_exp << func_name << '(';
  for (unsigned i = 0; i < func_args.size(); i++) {
    if (i != 0)
      cpp_exp << ',';
    cpp_exp << func_args.at(i)->ToString(
        [free_vars](Token &e) { return e.func().ToCppString(free_vars); });
  }
  cpp_exp << ')';
  return cpp_exp.str();
}

FunctionCall *RealFunctionCall::Clone() {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new RealFunctionCall(func_name, new_func_args);
}

FunctionCall *SumFunctionCall::Clone() {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new SumFunctionCall(func_name, new_func_args);
}

std::string SumFunctionCall::ToCppString(std::vector<std::string> free_vars) {
  std::stringstream lambda;
  lambda << "([";
  for (unsigned i{0}; i < free_vars.size(); i++) {
    if (i != 0)
      lambda << ',';
    lambda << free_vars.at(i);
  }
  std::string iter_var = func_args.at(1)->Front().var();
  free_vars.push_back(iter_var);
  lambda << "](){mpz_class sum{0}; for (unsigned " << iter_var << " = "
         << func_args.at(2)->Front().ToString() << "; " << iter_var
         << " <= " << func_args.at(3)->Front().ToString() << "; " << iter_var
         << "++){ sum += (" << func_args.at(0)->ToString([free_vars](Token &e) {
              return e.func().ToCppString(free_vars);
            })
         << ");} return sum;})()";
  return lambda.str();
}

std::ostream &operator<<(std::ostream &s, const FunctionCall &f) {
  std::string arg_str = "";
  for (auto &arg : f.func_args)
    arg_str += arg->ToString() + ",";
  s << f.func_name << "[" << arg_str.substr(0, arg_str.size() - 1) << "]";
  return s;
}
