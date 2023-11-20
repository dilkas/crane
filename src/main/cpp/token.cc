#include "expression.h"
#include "token.h"
#include <sstream>

// ======================== DATA ========================

const std::map<char, int> Token::operator_precedence_map{
    {'^', 4}, {'*', 3}, {'/', 3}, {'+', 2}, {'-', 2}, {'(', 1}};

const std::regex Token::var_pattern{"[a-zA-Z][a-zA-Z0-9]*"};

// ======================== CONSTRUCTORS ========================

Token::Token(std::string s) {
  if (std::regex_match(s, var_pattern)) {
    type = TokenType::kVariable;
    _var = s;
  } else {
    type = TokenType::kFunctionCall;
    _func_call = std::unique_ptr<FunctionCall>(FunctionCall::Create(s));
  }
}

// ======================== FUNCTIONS ========================

bool Token::comparePrecedence(Token op1, Token op2) {
  if (op1.type != TokenType::kOperator || op2.type != TokenType::kOperator)
    throw std::logic_error(
        "Invalid operand types, can only compare precedence of "
        "operators with type TokenType::kOperator");
  return (operator_precedence_map.at(op1.op()) >=
          operator_precedence_map.at(op2.op()));
}

std::string Token::GetFunctionName() const {
  std::stringstream name;
  bool base_func = true;
  name << func().func_name << "_";
  for (auto const &arg : func().func_args) {
    if (arg->Front().type == TokenType::kInteger) {
      name << std::to_string(arg->Front().value());
      base_func = false;
    } else
      name << 'x';
  }
  if (base_func)
    return func().func_name;
  return name.str();
}

std::string Token::GetFunctionSignature(std::string func_ret_pref /*= "int"*/,
                                        std::string var_pref /*= "int"*/,
                                        std::string end /*= ";"*/) const {
  std::stringstream signature;
  // get the argument list for the cpp code.
  std::string arg_list = "";
  for (auto const &arg : func().func_args)
    if (arg->Front().type == TokenType::kVariable)
      arg_list += var_pref + " " + arg->Front().var() + ", ";
  if (arg_list.size() != 0)
    arg_list = arg_list.substr(0, arg_list.size() - 2);
  signature << func_ret_pref << " " << GetFunctionName() << "(" << arg_list
            << ")" << end;
  return signature.str();
}

void Token::setOp(char o) {
  type = TokenType::kOperator;
  _op = o;
}

void Token::setValue(int x) {
  type = TokenType::kInteger;
  _value = x;
}

FunctionCall &Token::func() const {
  if (type != TokenType::kFunctionCall)
    throw std::logic_error("Cannot access value for non-FunctionCall token");
  return *_func_call;
}

char Token::op() const {
  if (type != TokenType::kOperator)
    throw std::logic_error("Cannot access value for non-Operator token");
  return _op;
}

int Token::value() const {
  if (type != TokenType::kInteger)
    throw std::logic_error("Cannot access value for non-Integer token");
  return _value;
}

std::string Token::var() const {
  if (type != TokenType::kVariable)
    throw std::logic_error("Cannot access var for non-Variable token");
  return _var;
}

std::string Token::ToString(std::function<std::string(Token &)> get_func_call) {
  switch (type) {
  case TokenType::kInteger:
    return std::to_string(value());
  case TokenType::kOperator:
    return std::string(1, op());
  case TokenType::kFunctionCall:
    return get_func_call(*this);
  case TokenType::kVariable:
    return var();
  default:
    return "";
  }
}

// ======================== OPERATORS ========================

std::ostream &operator<<(std::ostream &s, const Token &e) {
  switch (e.type) {
  case TokenType::kInteger: {
    s << e.value();
    break;
  }
  case TokenType::kOperator: {
    s << e.op();
    break;
  }
  case TokenType::kFunctionCall: {
    s << e.func();
    break;
  }
  case TokenType::kVariable: {
    s << e.var();
    break;
  }
  default: {
  }
  }
  return s;
}

std::ostream &operator<<(std::ostream &s, const std::list<Token> &l) {
  for (auto const &e : l)
    s << e << " ";
  return s;
}

std::ostream &operator<<(std::ostream &s, TokenType e) {
  switch (e) {
  case TokenType::kInteger: {
    s << "Integer";
    break;
  }
  case TokenType::kOperator: {
    s << "Operator";
    break;
  }
  case TokenType::kFunctionCall: {
    s << "FunctionCall";
    break;
  }
  case TokenType::kVariable: {
    s << "kVariable";
    break;
  }
  default:
    s << "Uninitialized";
  }
  return s;
}
