#include "function_call.h"

#include <sstream>

#include "expression.h"

FunctionCall::FunctionCall(const std::string &fname,
                           std::unique_ptr<Expression> arg1,
                           std::unique_ptr<Expression> arg2)
    : func_name{fname} {
  func_args.push_back(move(arg1));
  func_args.push_back(move(arg2));
}

FunctionCall::FunctionCall(FunctionCall *other) : func_name(other->func_name) {
  func_args.reserve(other->func_args.size());
  for (auto const &arg : other->func_args)
    func_args.emplace_back(
        std::unique_ptr<Expression>(new Expression(arg.get())));
}

FunctionCall::FunctionCall(std::string call_str) {
  int num_open_brack = -1;
  int arg_start = 0;
  for (unsigned i{0}; i < call_str.size(); i++) {
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
          func_args.push_back(std::unique_ptr<Expression>(new Expression(arg)));
        arg_start = i + 1;
      }
      if (call_str.at(i) == ']')
        num_open_brack -= 1;
    }
  }
}

std::string FunctionCall::toCppString(std::vector<std::string> free_vars) {
  if (func_name == "Sum") {
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
           << func_args.at(2)->Front().toString() << "; " << iter_var
           << " <= " << func_args.at(3)->Front().toString() << "; " << iter_var
           << "++){ sum += ("
           << func_args.at(0)->ToString([free_vars](Token &e) {
                if (e.func().func_name == "Binomial" ||
                    e.func().func_name == "power" ||
                    e.func().func_name == "Sum")
                  return e.func().toCppString(free_vars);
                return e.func().toString(true);
              })
           << ");} return sum;})()";
    return lambda.str();
  } else if (func_name == "power" || func_name == "Binomial") {
    std::stringstream cpp_exp;
    cpp_exp << func_name << '(';
    for (int i{0}; i < func_args.size(); i++) {
      if (i != 0)
        cpp_exp << ',';
      cpp_exp << func_args.at(i)->ToString([free_vars](Token &e) {
        if (e.func().func_name == "Binomial" || e.func().func_name == "power" ||
            e.func().func_name == "Sum")
          return e.func().toCppString(free_vars);
        return e.func().toString(true);
      });
    }
    cpp_exp << ')';
    return cpp_exp.str();
  } else {
    return toString(true);
  }
}

std::string FunctionCall::toString(bool use_paren /*= false*/) {
  std::string arg_str = "";
  for (auto &arg : func_args)
    arg_str += arg->ToString() + ",";
  if (use_paren)
    return func_name + "(" + arg_str.substr(0, arg_str.size() - 1) + ")";
  return func_name + "[" + arg_str.substr(0, arg_str.size() - 1) + "]";
}

std::ostream &operator<<(std::ostream &s, const FunctionCall &f) {
  std::string arg_str = "";
  for (auto &arg : f.func_args)
    arg_str += arg->ToString() + ",";
  s << f.func_name << "[" << arg_str.substr(0, arg_str.size() - 1) << "]";
  return s;
}
