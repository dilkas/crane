/*
 * Copyright 2025 Paulius Dilkas (University of Toronto), Ananth K. Kidambi (IIT
 * Bombay), Guramrit Singh (IIT Bombay)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "function_call.h"

#include <cassert>
#include <sstream>

#include "expression.h"

FunctionCall::FunctionCall(const std::string &func_name,
                           const std::vector<Expression *> &func_args)
    : func_name(func_name) {
  for (auto const &arg : func_args)
    this->func_args.push_back(std::unique_ptr<Expression>(arg));
}

FunctionCall *FunctionCall::Create(const std::string &call_str) {
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
  if (func_name == "Inequality")
    return new InequalityFunctionCall(func_name, func_args);
  if (func_name == "Piecewise")
    return new PiecewiseFunctionCall(func_name, func_args);
  if (func_name == "Sum")
    return new SumFunctionCall(func_name, func_args);
  return new RealFunctionCall(func_name, func_args);
}

std::string
FunctionCall::GetFunctionSignature(std::string func_ret_pref /*= "int"*/,
                                   std::string var_pref /*= "int"*/,
                                   std::string end /*= ";"*/) const {
  std::stringstream signature;
  // get the argument list for the cpp code.
  std::string arg_list = "";
  for (auto const &arg : func_args)
    if (arg->Front()->GetVariable() != "")
      arg_list += var_pref + " " + arg->Front()->GetVariable() + ", ";
  if (arg_list.size() != 0)
    arg_list = arg_list.substr(0, arg_list.size() - 2);
  signature << func_ret_pref << " " << GetFunctionName() << "(" << arg_list
            << ")" << end;
  return signature.str();
}

std::string FunctionCall::GetFunctionName() const {
  std::stringstream name;
  bool base_func = true;
  name << func_name << "_";
  for (auto const &arg : func_args) {
    std::string function_name = arg->Front()->ToFunctionName();
    name << function_name;
    if (function_name != "x")
      base_func = false;
  }
  if (base_func)
    return func_name;
  return name.str();
}

void FunctionCall::HandlePower(
    std::stack<std::list<std::unique_ptr<Token>>> &exp_stack,
    bool recursive) const {
  FunctionCall *new_unit = CloneFunctionCall();
  if (recursive)
    for (auto &arg : new_unit->func_args)
      arg = arg->HandlePower();
  std::list<std::unique_ptr<Token>> list;
  list.push_back(std::unique_ptr<Token>(new_unit));
  exp_stack.push(std::move(list));
}

std::string FunctionCall::ToString() const {
  std::stringstream ss;
  ss << func_name << "(";
  for (size_t i = 0; i < func_args.size(); i++) {
    if (i != 0)
      ss << ", ";
    ss << func_args.at(i)->ToString();
  }
  ss << ")";
  return ss.str();
}

FunctionCall *InequalityFunctionCall::CloneFunctionCall() const {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new InequalityFunctionCall(func_name, new_func_args);
}

std::string
InequalityFunctionCall::ToCppString(std::vector<std::string> free_vars) const {
  auto get_func_call = [free_vars](const Token &e) {
    return e.ToCppString(free_vars);
  };

  std::vector<std::string> as_strings;
  for (auto const &arg : func_args)
    as_strings.push_back(arg->ToString(get_func_call));

  std::stringstream cpp_exp;
  bool first = true;
  for (unsigned i = 1; i < as_strings.size(); i++) {
    if (as_strings.at(i) == "<=") {
      if (!first)
        cpp_exp << " && ";
      first = false;
      cpp_exp << as_strings.at(i - 1) << " " << as_strings.at(i) << " "
              << as_strings.at(i + 1);
    }
  }
  return cpp_exp.str();
}

void FunctionCall::MaxDecrementPerVariable(
    std::stack<Expression const *> &arg_stack,
    std::map<std::string, int> &max_sub, std::string function_name) const {
  for (auto const &arg : func_args)
    arg_stack.push(arg.get());
}

void FunctionCall::ShuntingYard(
    std::stack<std::unique_ptr<Token>> &operator_stack, Expression *postfix_exp,
    bool recursive) const {
  FunctionCall *new_unit = CloneFunctionCall();
  if (recursive)
    for (size_t i = 0; i < new_unit->func_args.size(); i++)
      new_unit->func_args[i] = new_unit->func_args[i]->ShuntingYard();
  postfix_exp->AddToken(std::unique_ptr<Token>(new_unit));
}

FunctionCall *OtherFunctionCall::CloneFunctionCall() const {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new OtherFunctionCall(func_name, new_func_args);
}

std::string
OtherFunctionCall::ToCppString(std::vector<std::string> free_vars) const {
  std::stringstream cpp_exp;
  cpp_exp << func_name << '(';
  for (unsigned i = 0; i < func_args.size(); i++) {
    if (i != 0)
      cpp_exp << ',';
    cpp_exp << func_args.at(i)->ToString(
        [free_vars](const Token &e) { return e.ToCppString(free_vars); });
  }
  cpp_exp << ')';
  return cpp_exp.str();
}

FunctionCall *PiecewiseFunctionCall::CloneFunctionCall() const {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new PiecewiseFunctionCall(func_name, new_func_args);
}

std::string
PiecewiseFunctionCall::ToCppString(std::vector<std::string> free_vars) const {
  assert(func_args.size() == 3);
  std::stringstream cpp_exp;
  auto get_func_call = [free_vars](const Token &e) {
    return e.ToCppString(free_vars);
  };
  cpp_exp << "((" << func_args.at(1)->ToString(get_func_call) << ")"
          << " ? (" << func_args.at(0)->ToString(get_func_call) << ") : ("
          << func_args.at(2)->ToString(get_func_call) << "))";
  return cpp_exp.str();
}

FunctionCall *RealFunctionCall::CloneFunctionCall() const {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new RealFunctionCall(func_name, new_func_args);
}

void RealFunctionCall::MaxDecrementPerVariable(
    std::stack<Expression const *> &arg_stack,
    std::map<std::string, int> &max_sub, std::string function_name) const {
  if (func_name != function_name)
    return;
  for (auto const &arg : func_args) {
    std::string var_name = arg->FirstVariable();
    if (max_sub.find(var_name) == max_sub.end())
      return;
    max_sub[var_name] =
        std::max(max_sub.at(var_name), -arg->ShuntingYard()->Evaluate());
  }
}

std::string RealFunctionCall::ToCppString(std::vector<std::string>) const {
  std::string arg_str = "";
  for (auto &arg : func_args)
    arg_str += arg->ToString() + ",";
  return func_name + "(" + arg_str.substr(0, arg_str.size() - 1) + ")";
}

FunctionCall *SumFunctionCall::CloneFunctionCall() const {
  std::vector<Expression *> new_func_args;
  for (auto const &arg : func_args)
    new_func_args.push_back(new Expression(arg.get()));
  return new SumFunctionCall(func_name, new_func_args);
}

std::string
SumFunctionCall::ToCppString(std::vector<std::string> free_vars) const {
  std::stringstream lambda;
  lambda << "([";
  for (unsigned i = 0; i < free_vars.size(); i++) {
    if (i != 0)
      lambda << ',';
    lambda << free_vars.at(i);
  }
  std::string iter_var = func_args.at(1)->Front()->GetVariable();
  free_vars.push_back(iter_var);
  lambda << "](){mpz_class sum{0}; for (unsigned " << iter_var << " = "
         << func_args.at(2)->Front()->ToString() << "; " << iter_var
         << " <= " << func_args.at(3)->Front()->ToString() << "; " << iter_var
         << "++){ sum += ("
         << func_args.at(0)->ToString([free_vars](const Token &e) {
              return e.ToCppString(free_vars);
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
