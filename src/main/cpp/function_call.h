#ifndef FUNCTION_CALL_H_
#define FUNCTION_CALL_H_

#include <memory>
#include <string>
#include <vector>

#include "token.h"

class Expression;

class FunctionCall : public Token {
public:
  // TODO (Paulius): shouldn't be public
  std::string func_name;
  std::vector<std::unique_ptr<Expression>> func_args;

  FunctionCall() = default;
  FunctionCall(const std::string &, const std::vector<Expression *> &);
  static FunctionCall *Create(const std::string &);
  virtual ~FunctionCall() = default;

  std::unique_ptr<Token> Clone() const override {
    return std::unique_ptr<Token>(CloneFunctionCall());
  }

  virtual FunctionCall *CloneFunctionCall() const = 0;

  void Evaluate(std::stack<std::unique_ptr<Token>> &eval_stack) const override {
    eval_stack.push(Token::Create(0));
  }

  std::pair<std::string, int> GetFunctionPair() const {
    return std::make_pair(func_name, func_args.size());
  }

  std::string GetFunctionSignature(std::string, std::string,
                                   std::string) const override;
  void HandlePower(std::stack<std::list<std::unique_ptr<Token>>> &,
                   bool) const override;
  void MaxDecrementPerVariable(std::stack<Expression const *> &,
                               std::map<std::string, int> &,
                               std::string) const override;
  void ShuntingYard(std::stack<std::unique_ptr<Token>> &, Expression *,
                    bool) const override;

  std::string ToString() const;

  /** This function cannot be used to print a function call */
  std::string ToString(
      std::function<std::string(const Token &)> get_func_call) const override {
    return get_func_call(*this);
  }

private:
  std::string GetFunctionName() const;
};

class InequalityFunctionCall : public FunctionCall {
public:
  InequalityFunctionCall(const std::string &func_name,
                         const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *CloneFunctionCall() const override;
  std::string ToCppString(std::vector<std::string>) const override;
};

/** Binomial and power functions */
class OtherFunctionCall : public FunctionCall {
public:
  OtherFunctionCall(const std::string &func_name,
                    const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *CloneFunctionCall() const override;
  std::string ToCppString(std::vector<std::string>) const override;
};

class PiecewiseFunctionCall : public FunctionCall {
public:
  PiecewiseFunctionCall(const std::string &func_name,
                        const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *CloneFunctionCall() const override;
  std::string ToCppString(std::vector<std::string>) const override;
};

/** Functions that are defined as part of the solution */
class RealFunctionCall : public FunctionCall {
public:
  RealFunctionCall(const std::string &func_name,
                   const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *CloneFunctionCall() const override;
  void MaxDecrementPerVariable(std::stack<Expression const *> &,
                               std::map<std::string, int> &,
                               std::string) const override;
  std::string ToCppString(std::vector<std::string>) const override;
};

class SumFunctionCall : public FunctionCall {
public:
  SumFunctionCall(const std::string &func_name,
                  const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *CloneFunctionCall() const override;
  std::string ToCppString(std::vector<std::string>) const override;
};

std::ostream &operator<<(std::ostream &, const FunctionCall &);

#endif // FUNCTION_CALL_H_
