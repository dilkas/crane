#ifndef EXPRESSION_H_
#define EXPRESSION_H_

#include <list>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "token.h"

// TODO (Paulius): have two subclasses for infix and postfix notation.
// ShuntingYard would then transform one to the other, and Evaluate would only
// be valid on the latter.

// TODO (Paulius): (here and in other classes): add 'const', 'static',
// 'const &' where appropriate.

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
  MaxDecrementPerVariable(std::map<std::string, int> max_sub) const;

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
