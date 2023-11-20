#ifndef FUNCTION_CALL_H_
#define FUNCTION_CALL_H_

#include <iostream>
#include <memory>
#include <string>
#include <vector>

class Expression;

class FunctionCall {
public:
  std::string func_name;
  std::vector<std::unique_ptr<Expression>> func_args;

  FunctionCall(const std::string &, const std::vector<Expression *> &);

  virtual FunctionCall *Clone() = 0;
  static FunctionCall *Create(std::string);
  virtual std::string ToCppString(std::vector<std::string>) = 0;
  std::string ToString(bool = false);
};

/** Binomial and power functions */
class OtherFunctionCall : public FunctionCall {
public:
  OtherFunctionCall(const std::string &func_name,
                    const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *Clone() override;
  std::string ToCppString(std::vector<std::string>) override;
};

/** Functions that are defined as part of the solution */
class RealFunctionCall : public FunctionCall {
public:
  RealFunctionCall(const std::string &func_name,
                   const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *Clone() override;
  std::string ToCppString(std::vector<std::string> free_vars) override {
    return ToString(true);
  }
};

class SumFunctionCall : public FunctionCall {
public:
  SumFunctionCall(const std::string &func_name,
                  const std::vector<Expression *> &func_args)
      : FunctionCall(func_name, func_args) {}
  FunctionCall *Clone() override;
  std::string ToCppString(std::vector<std::string>) override;
};

std::ostream &operator<<(std::ostream &, const FunctionCall &);

#endif // FUNCTION_CALL_H_
