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
  Expression(Expression *other) : tokens_(other->tokens_) {}
  Expression(const std::string &);
  Expression(Token token) : tokens_{token} {};
  Expression(std::list<Token> tokens)
      : tokens_{tokens.begin(), tokens.end()} {};
  Expression(std::vector<Token> tokens) : tokens_{tokens} {};

  /** Evaluates an expression in the postfix notation. */
  int Evaluate();

  /** Returns "" in the case of no variables. */
  std::string FirstVariable() const;

  Token Front() const { return tokens_.front(); }
  void SetFront(int value) { tokens_.front().setValue(value); }

  /** Converts exponent from infix notation (a^b) to the power(a,b) notation. */
  std::unique_ptr<Expression> HandlePower(bool = true);

  std::vector<std::pair<std::string, int>>
  MaxDecrementPerVariable(std::map<std::string, int> max_sub) const;

  /** Transforms an infix expression into postfix notation.
   *
   *  NOTE: Assumes left to right associativity of all operators.
   */
  std::unique_ptr<Expression> ShuntingYard(bool = true);

  std::string ToString() const;
  std::string ToString(std::function<std::string(Token &)>);
  friend std::ostream &operator<<(std::ostream &, Expression);

private:
  std::vector<Token> tokens_;
};

#endif // EXPRESSION_H_
