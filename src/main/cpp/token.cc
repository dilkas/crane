#include "token.h"

#include <cmath>
#include <sstream>

#include "expression.h"
#include "function_call.h"

std::unique_ptr<Token> Token::Create(char value) {
  return std::unique_ptr<Token>(new OperatorToken(value));
}

std::unique_ptr<Token> Token::Create(int value) {
  return std::unique_ptr<Token>(new IntegerToken(value));
}

std::unique_ptr<Token> Token::Create(const std::string &value) {
  if (std::regex_match(value, std::regex("[a-zA-Z][a-zA-Z0-9]*")))
    return std::unique_ptr<Token>(new VariableToken(value));
  return std::unique_ptr<Token>(FunctionCall::Create(value));
}

void Token::HandlePower(
    std::stack<std::list<std::unique_ptr<Token>>> &exp_stack, bool) const {
  std::list<std::unique_ptr<Token>> list;
  list.push_back(std::move(Clone()));
  exp_stack.emplace(std::move(list));
}

void Token::ShuntingYard(std::stack<std::unique_ptr<Token>> &operator_stack,
                         Expression *postfix_exp, bool recursive) const {
  postfix_exp->AddToken(Clone());
}

const std::map<char, int> OperatorToken::operator_precedence_map_{
    {'^', 4}, {'*', 3}, {'/', 3}, {'+', 2}, {'-', 2}, {'(', 1}};

void OperatorToken::Evaluate(
    std::stack<std::unique_ptr<Token>> &eval_stack) const {
  int arg2 = eval_stack.top()->GetInteger();
  eval_stack.pop();
  int arg1 = eval_stack.top()->GetInteger();
  eval_stack.pop();
  int res = 0;
  switch (value_) {
  case '+': {
    res = arg1 + arg2;
    break;
  }
  case '-': {
    res = arg1 - arg2;
    break;
  }
  case '*': {
    res = arg1 * arg2;
    break;
  }
  case '/': {
    res = arg1 / arg2;
    break;
  }
  case '^': {
    res = round(pow(arg1, arg2));
    break;
  }
  default:
    throw new std::logic_error("Invalid operator");
  }
  eval_stack.push(Token::Create(res));
}

void OperatorToken::HandlePower(
    std::stack<std::list<std::unique_ptr<Token>>> &exp_stack, bool) const {
  std::list<std::unique_ptr<Token>> arg2 = std::move(exp_stack.top());
  exp_stack.pop();
  std::list<std::unique_ptr<Token>> arg1 = std::move(exp_stack.top());
  exp_stack.pop();
  std::list<std::unique_ptr<Token>> res;
  switch (value_) {
  case '+':
  case '-':
  case '*':
  case '/': {
    bool b1 = (arg1.size() > 1);
    bool b2 = (arg2.size() > 1);
    if (b1)
      res.push_back(Token::Create('('));
    res.splice(res.end(), arg1);
    if (b1)
      res.push_back(Token::Create(')'));
    res.push_back(Clone());
    if (b2)
      res.push_back(Token::Create('('));
    res.splice(res.end(), arg2);
    if (b2)
      res.push_back(Token::Create(')'));
    exp_stack.push(std::move(res));
    break;
  }
  case '^': {
    std::list<std::unique_ptr<Token>> list;
    list.push_back(std::move(std::unique_ptr<Token>(
        new OtherFunctionCall("power", {new Expression(std::move(arg1)),
                                        new Expression(std::move(arg2))}))));
    exp_stack.push(std::move(list));
    break;
  }
  default:
    throw new std::logic_error("Invalid operator");
  }
}

bool OperatorToken::HigherPrecedence(const Token *other) const {
  auto op = dynamic_cast<const OperatorToken *>(other);
  if (op == nullptr)
    throw std::logic_error("Invalid operand types, can only compare precedence "
                           "of operator tokens");
  return operator_precedence_map_.at(value_) >=
         operator_precedence_map_.at(op->value_);
}

void OperatorToken::ShuntingYard(
    std::stack<std::unique_ptr<Token>> &operator_stack, Expression *postfix_exp,
    bool recursive) const {
  if (operator_stack.empty() || value_ == '(') {
    operator_stack.push(Clone());
  } else if (value_ == ')') {
    while (operator_stack.top()->GetOperator() != '(') {
      postfix_exp->AddToken(std::move(operator_stack.top()));
      operator_stack.pop();
    }
    operator_stack.pop();
  } else if (operator_stack.top()->HigherPrecedence(this)) {
    postfix_exp->AddToken(std::move(operator_stack.top()));
    operator_stack.pop();
    operator_stack.push(Clone());
  } else {
    operator_stack.push(Clone());
  }
}

std::string
VariableToken::ToString(std::function<std::string(const Token &)>) const {
  if (value_ == "Less")
    return "<";
  if (value_ == "LessEqual")
    return "<=";
  return value_;
}