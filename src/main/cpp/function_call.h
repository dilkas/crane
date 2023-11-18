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

  FunctionCall(const std::string &, std::unique_ptr<Expression>,
               std::unique_ptr<Expression>);
  FunctionCall(FunctionCall *);
  FunctionCall(std::string);

  std::string toCppString(std::vector<std::string>);
  std::string toString(bool = false);
};

std::ostream &operator<<(std::ostream &, const FunctionCall &);

#endif // FUNCTION_CALL_H_
