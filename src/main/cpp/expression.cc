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

#include "expression.h"

#include <cmath>
#include <sstream>
#include <stack>

Expression::Expression(Expression *other) {
  for (const auto &token : other->tokens_)
    tokens_.push_back(token->Clone());
}

Expression::Expression(const std::string &exp_str) {
  tokens_.reserve(exp_str.size());
  for (unsigned i = 0; i < exp_str.size(); i++) {
    if (isspace(exp_str.at(i)))
      continue;
    if (isdigit(exp_str.at(i))) {
      // integer
      unsigned j = i;
      for (; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      tokens_.emplace_back(Token::Create(stoi(exp_str.substr(i, j - i))));
      i = j - 1;
    } else if (isalpha(exp_str.at(i))) {
      // variable or function call
      int num_open_brack = 0;
      unsigned j = i;
      bool isVar = true;
      for (;
           j < exp_str.size() && !(num_open_brack == 1 && exp_str.at(j) == ']');
           j++) {
        if (num_open_brack == 0 && !isalnum(exp_str.at(j)) &&
            exp_str.at(j) != '[')
          break;
        if (exp_str.at(j) == '[') {
          num_open_brack++;
          isVar = false;
        } else if (exp_str.at(j) == ']') {
          num_open_brack--;
        }
      }
      if (isVar) {
        tokens_.emplace_back(Token::Create(exp_str.substr(i, j - i)));
        i = j - 1;
      } else {
        tokens_.emplace_back(Token::Create(exp_str.substr(i, j - i + 1)));
        i = j;
      }
    } else if (exp_str.at(i) == '-' &&
               (tokens_.size() == 0 ||
                (tokens_.back()->GetOperator() == '('))) {
      // negative integer
      unsigned j = i + 1;
      for (; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      tokens_.emplace_back(Token::Create(stoi(exp_str.substr(i, j - i))));
      i = j - 1;
    } else {
      // (a single-character) operator
      tokens_.emplace_back(Token::Create(exp_str.at(i)));
    }
  }
}

Expression::Expression(std::list<std::unique_ptr<Token>> tokens) {
  tokens_.reserve(tokens.size());
  for (auto &e : tokens)
    tokens_.emplace_back(std::move(e));
};

int Expression::Evaluate() {
  std::stack<std::unique_ptr<Token>> eval_stack;
  for (auto const &e : tokens_)
    e->Evaluate(eval_stack);
  return eval_stack.top()->GetInteger();
}

std::string Expression::FirstVariable() const {
  for (auto const &e : tokens_)
    if (e->GetVariable() != "")
      return e->GetVariable();
  return "";
}

std::unique_ptr<Expression> Expression::HandlePower(bool recursive /*= true*/) {
  auto postfix_exp = ShuntingYard(false);
  std::stack<std::list<std::unique_ptr<Token>>> exp_stack;
  for (auto const &e : postfix_exp->tokens_)
    e->HandlePower(exp_stack, recursive);
  return std::unique_ptr<Expression>(
      new Expression(std::move(exp_stack.top())));
}

std::vector<std::pair<std::string, int>>
Expression::MaxDecrementPerVariable(std::map<std::string, int> &max_sub,
                                    std::string function_name) const {
  std::stack<Expression const *> arg_stack;
  arg_stack.push(this);
  while (!arg_stack.empty()) {
    Expression const *arg_exp = arg_stack.top();
    arg_stack.pop();
    for (auto &e : arg_exp->tokens_)
      e->MaxDecrementPerVariable(arg_stack, max_sub, function_name);
  }
  std::vector<std::pair<std::string, int>> max_sub_vec;
  std::copy(max_sub.begin(), max_sub.end(), std::back_inserter(max_sub_vec));
  return max_sub_vec;
}

std::unique_ptr<Expression>
Expression::ShuntingYard(bool recursive /*= true*/) const {
  std::stack<std::unique_ptr<Token>> operator_stack;
  auto postfix_exp = std::unique_ptr<Expression>(new Expression());
  postfix_exp->tokens_.reserve(tokens_.size());
  for (auto const &x : tokens_)
    x->ShuntingYard(operator_stack, postfix_exp.get(), recursive);
  while (!operator_stack.empty()) {
    postfix_exp->tokens_.push_back(std::move(operator_stack.top()));
    operator_stack.pop();
  }
  return postfix_exp;
}

std::string Expression::ToString(
    std::function<std::string(const Token &)> get_func_call) const {
  std::stringstream s;
  for (const auto &e : tokens_)
    s << e->ToString(get_func_call);
  return s.str();
}
