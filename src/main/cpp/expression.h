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

#ifndef EXPRESSION_H_
#define EXPRESSION_H_

#include <list>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "token.h"

class Expression {
public:
  Expression() = default;
  Expression(Expression *other);
  Expression(const std::string &);

  Expression(std::unique_ptr<Token> token) {
    tokens_.push_back(std::move(token));
  }

  // NOTE: cannot be passed by reference
  Expression(std::list<std::unique_ptr<Token>>);

  // NOTE: cannot be passed by reference
  Expression(std::vector<std::unique_ptr<Token>> tokens)
      : tokens_(std::move(tokens)){};

  void AddToken(std::unique_ptr<Token> token) {
    tokens_.push_back(std::move(token));
  }

  /** Evaluates an expression in the postfix notation. */
  int Evaluate();

  /** Returns "" in the case of no variables. */
  std::string FirstVariable() const;

  const Token *Front() const { return tokens_.front().get(); }
  void SetFront(int value) { tokens_[0] = Token::Create(value); }

  /** Converts exponent from infix notation (a^b) to the power(a,b) notation. */
  std::unique_ptr<Expression> HandlePower(bool = true);

  std::vector<std::pair<std::string, int>>
  MaxDecrementPerVariable(std::map<std::string, int> &, std::string) const;

  /** Transforms an infix expression into postfix notation.
   *
   *  NOTE: Assumes left to right associativity of all operators.
   */
  virtual std::unique_ptr<Expression> ShuntingYard(bool = true) const;

  std::string ToString() const {
    return ToString([](const Token &t) { return t.ToString(); });
  }

  std::string ToString(std::function<std::string(const Token &)>) const;

private:
  std::vector<std::unique_ptr<Token>> tokens_;
};

#endif // EXPRESSION_H_
