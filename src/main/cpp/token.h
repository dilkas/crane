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

#ifndef TOKEN_H_
#define TOKEN_H_

#include <list>
#include <map>
#include <memory>
#include <regex>
#include <stack>
#include <string>

class Expression;

class Token {
public:
  Token() {}
  virtual ~Token() {}
  virtual std::unique_ptr<Token> Clone() const = 0;
  static std::unique_ptr<Token> Create(char);
  static std::unique_ptr<Token> Create(int);
  static std::unique_ptr<Token> Create(const std::string &);

  virtual void Evaluate(std::stack<std::unique_ptr<Token>> &) const = 0;

  virtual std::pair<std::string, int> GetFunctionPair() const {
    throw new std::logic_error("This token is not a function");
  }

  virtual std::string GetFunctionSignature(std::string = "int",
                                           std::string = "int",
                                           std::string = ";") const {
    throw new std::logic_error("This token is not a function");
  }

  virtual int GetInteger() const {
    throw new std::logic_error("This token is not an integer");
  }

  virtual char GetOperator() const { return '\0'; }
  virtual std::string GetVariable() const { return ""; }
  virtual void HandlePower(std::stack<std::list<std::unique_ptr<Token>>> &,
                           bool) const;

  virtual bool HigherPrecedence(const Token *other) const {
    throw std::logic_error("Invalid operand types, can only compare precedence "
                           "of operator tokens");
  }

  virtual void MaxDecrementPerVariable(std::stack<Expression const *> &,
                                       std::map<std::string, int> &,
                                       std::string) const {}
  virtual void ShuntingYard(std::stack<std::unique_ptr<Token>> &, Expression *,
                            bool) const;

  // NOTE: cannot be passed by reference
  virtual std::string ToCppString(std::vector<std::string>) const {
    throw new std::logic_error("This token is not a function");
  }

  virtual std::string ToFunctionName() const { return "x"; }

  std::string ToString() const {
    return ToString([](const Token &t) { return t.ToString(); });
  }

  virtual std::string
      ToString(std::function<std::string(const Token &)>) const = 0;
};

class IntegerToken : public Token {
public:
  IntegerToken(int value) : value_(value) {}
  std::unique_ptr<Token> Clone() const { return Token::Create(value_); }

  void Evaluate(std::stack<std::unique_ptr<Token>> &eval_stack) const override {
    eval_stack.push(std::unique_ptr<Token>(new IntegerToken(value_)));
  }

  int GetInteger() const override { return value_; }
  std::string ToFunctionName() const override { return std::to_string(value_); }

  std::string
  ToString(std::function<std::string(const Token &)>) const override {
    return std::to_string(value_);
  }

private:
  int value_;
};

class OperatorToken : public Token {
public:
  OperatorToken(char value) : value_(value) {}
  std::unique_ptr<Token> Clone() const { return Token::Create(value_); }
  void Evaluate(std::stack<std::unique_ptr<Token>> &) const override;
  char GetOperator() const override { return value_; }
  void HandlePower(std::stack<std::list<std::unique_ptr<Token>>> &,
                   bool) const override;
  void ShuntingYard(std::stack<std::unique_ptr<Token>> &, Expression *,
                    bool) const override;

  std::string
  ToString(std::function<std::string(const Token &)>) const override {
    return std::string(1, value_);
  }

  /** Returns true if op1 has >= precedence than op2 */
  bool HigherPrecedence(const Token *) const override;

private:
  char value_;

  /** Stores the operator precedence values, higher precedence means more value
   * in the map */
  static const std::map<char, int> operator_precedence_map_;
};

class VariableToken : public Token {
public:
  VariableToken(const std::string &value) : value_(value) {}

  std::unique_ptr<Token> Clone() const {
    return std::unique_ptr<Token>(new VariableToken(value_));
  }

  void Evaluate(std::stack<std::unique_ptr<Token>> &eval_stack) const override {
    eval_stack.push(Token::Create(0));
  }

  std::string GetVariable() const override { return value_; }

  std::string
      ToString(std::function<std::string(const Token &)>) const override;

private:
  std::string value_;
};

#endif // TOKEN_H_
