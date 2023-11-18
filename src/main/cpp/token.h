#ifndef TOKEN_H_
#define TOKEN_H_

#include <list>
#include <map>
#include <regex>
#include <string>

#include "function_call.h"

// TODO (Paulius): need subclasses

// TODO (Paulius): move into the Token class. Instead of branching on
// TokenType, use inheritance.
enum class TokenType { kNone, kOperator, kInteger, kFunctionCall, kVariable };

class Token {
public:
  // ======================== DATA ========================

  /** Stores the operator precedence values, higher precedence means more value
   * in the map */
  static const std::map<char, int> operator_precedence_map;

  TokenType type;
  static const std::regex var_pattern;

  // ======================== CONSTRUCTORS ========================

  Token(char op) : type{TokenType::kOperator}, _op{op} {}

  Token(const Token &other)
      : type(other.type), _value(other._value), _op(other._op),
        _var(other._var) {
    FunctionCall *function_call =
        (other._func_call.get() == nullptr)
            ? nullptr
            : new FunctionCall(other._func_call.get());
    _func_call = std::unique_ptr<FunctionCall>(function_call);
  }

  Token(int val) : type{TokenType::kInteger}, _value{val} {}
  Token(std::string);
  Token(int val, char op, FunctionCall *func_call, std::string var, TokenType t)
      : _value{val}, _op{op}, _func_call{func_call}, _var{var}, type{t} {}

  // ======================== FUNCTIONS ========================

  /** Returns true if op1 has >= precedence than op2 */
  static bool comparePrecedence(Token op1, Token op2);

  std::string GetFunctionName() const;
  std::string GetFunctionSignature(std::string = "int", std::string = "int",
                                   std::string = ";") const;
  void setOp(char o);
  void setValue(int x);
  FunctionCall &func() const;
  char op() const;
  int value() const;
  std::string var() const;

  std::string toString(std::function<std::string(Token &)> get_func_call =
                           [](Token &e) { return e.func().toString(); });

protected:
  std::unique_ptr<FunctionCall> _func_call;
  char _op = '\0';
  int _value = 0;
  std::string _var = "";
};

// TODO (Paulius): friend functions or not (across all the files)?
std::ostream &operator<<(std::ostream &s, const Token &e);
std::ostream &operator<<(std::ostream &s, const std::list<Token> &e);
std::ostream &operator<<(std::ostream &s, TokenType e);

#endif // TOKEN_H_
