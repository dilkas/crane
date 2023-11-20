#include "expression.h"

#include <cmath>
#include <stack>

// TODO (Paulius): I'm guessing this won't work for a unary minus sign in
// front of anything other than an integer.
Expression::Expression(const std::string &exp_str) {
  tokens_.reserve(exp_str.size());
  for (unsigned i = 0; i < exp_str.size(); i++) {
    if (isspace(exp_str.at(i)))
      continue;
    if (isdigit(exp_str.at(i))) {
      // integer
      unsigned j = i;
      for (; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      tokens_.emplace_back(Token(stoi(exp_str.substr(i, j - i))));
      i = j - 1;
    } else if (isalpha(exp_str.at(i))) {
      // variable or function call
      int num_open_brack = 0;
      unsigned j = i;
      bool isVar = false;
      for (;
           j < exp_str.size() && !(num_open_brack == 1 && exp_str.at(j) == ']');
           j++) {
        if (num_open_brack == 0 && !isalnum(exp_str.at(j)) &&
            exp_str.at(j) != '[') {
          isVar = true;
          break;
        }
        if (exp_str.at(j) == '[')
          num_open_brack++;
        else if (exp_str.at(j) == ']')
          num_open_brack--;
      }
      if (isVar) {
        tokens_.emplace_back(Token(exp_str.substr(i, j - i)));
        i = j - 1;
      } else {
        tokens_.emplace_back(Token(exp_str.substr(i, j - i + 1)));
        i = j;
      }
    } else if (exp_str.at(i) == '-' &&
               (tokens_.size() == 0 ||
                (tokens_.back().type == TokenType::kOperator &&
                 tokens_.back().op() == '('))) {
      // negative integer
      unsigned j = i + 1;
      for (; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      tokens_.emplace_back(Token(stoi(exp_str.substr(i, j - i))));
      i = j - 1;
    } else {
      // (a single-character) operator
      tokens_.emplace_back(Token(exp_str.at(i)));
    }
  }
}

// TODO (Paulius): eliminate these lambdas
int Expression::Evaluate() {
  auto GetFunctionValue = [](const Token &token) {
    if (token.type != TokenType::kFunctionCall)
      throw std::invalid_argument(
          "get_func_val accepts only TokenType::kFunctionCall arguments");
    return 0;
  };

  auto GetVariableValue = [](const Token &token) {
    if (token.type != TokenType::kVariable) {
      throw std::invalid_argument(
          "get_func_val accepts only TokenType::kFunctionCall arguments");
    }
    return 0;
  };

  std::stack<Token> eval_stack;
  for (auto const &e : tokens_) {
    switch (e.type) {
    case TokenType::kInteger: {
      eval_stack.push(e);
      break;
    }
    case TokenType::kVariable: {
      eval_stack.push({GetVariableValue(e)});
      break;
    }
    case TokenType::kOperator: {
      int arg2 = eval_stack.top().value();
      eval_stack.pop();
      int arg1 = eval_stack.top().value();
      eval_stack.pop();
      int res = 0;
      switch (e.op()) {
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
      }
      eval_stack.push({res});
      break;
    }
    case TokenType::kFunctionCall: {
      Token f{e};
      for (auto &arg : e.func().func_args) {
        int v = arg->Evaluate();
        arg = std::unique_ptr<Expression>(new Expression(Token{v}));
      }
      eval_stack.push({GetFunctionValue(f)});
      break;
    }
    default:
      throw std::logic_error("Uninitialized token");
    }
  }
  return eval_stack.top().value();
}

std::string Expression::FirstVariable() const {
  for (auto e : tokens_)
    if (e.type == TokenType::kVariable)
      return e.var();
  return "";
}

std::unique_ptr<Expression> Expression::HandlePower(bool recursive /*= true*/) {
  auto postfix_exp = ShuntingYard(false);
  std::stack<std::list<Token>> exp_stack;
  for (auto e : postfix_exp->tokens_) {
    switch (e.type) {
    case TokenType::kInteger: {
      exp_stack.push({e});
      break;
    }
    case TokenType::kVariable: {
      exp_stack.push({e});
      break;
    }
    case TokenType::kOperator: {
      std::list<Token> arg2 = exp_stack.top();
      exp_stack.pop();
      std::list<Token> arg1 = exp_stack.top();
      exp_stack.pop();
      std::list<Token> res;
      switch (e.op()) {
      case '+':
      case '-':
      case '*':
      case '/': {
        bool b1 = (arg1.size() > 1);
        bool b2 = (arg2.size() > 1);
        if (b1)
          res.push_back({'('});
        res.splice(res.end(), arg1);
        if (b1)
          res.push_back({')'});
        res.push_back({e.op()});
        if (b2)
          res.push_back({'('});
        res.splice(res.end(), arg2);
        if (b2)
          res.push_back({')'});
        exp_stack.push({res});
        break;
      }
      case '^': {
        FunctionCall *pow_func_call = new OtherFunctionCall(
            "power", {new Expression(arg1), new Expression(arg2)});
        exp_stack.push(
            {{Token(0, '\0', pow_func_call, "", TokenType::kFunctionCall)}});
        break;
      }
      }
      break;
    }
    case TokenType::kFunctionCall: {
      Token new_unit{e};
      if (recursive)
        for (auto &arg : new_unit.func().func_args)
          arg = arg->HandlePower();
      exp_stack.push({new_unit});
      break;
    }
    default:
      throw std::logic_error("Uninitialized token");
    }
  }
  return std::unique_ptr<Expression>(new Expression(exp_stack.top()));
}

std::vector<std::pair<std::string, int>>
Expression::MaxDecrementPerVariable(std::map<std::string, int> max_sub) const {
  std::stack<const Expression *> arg_stack;
  arg_stack.push(this);
  while (!arg_stack.empty()) {
    const Expression &arg_exp = *arg_stack.top();
    arg_stack.pop();
    for (const Token &e : arg_exp.tokens_) {
      if (e.type != TokenType::kFunctionCall)
        continue;
      if (e.func().func_name == "Binomial" || e.func().func_name == "power" ||
          e.func().func_name == "Sum") {
        for (auto const &arg : e.func().func_args)
          arg_stack.push(arg.get());
      } else {
        for (auto const &arg : e.func().func_args) {
          std::string var_name = arg->FirstVariable();
          if (max_sub.find(var_name) == max_sub.end())
            continue;
          max_sub.at(var_name) =
              std::max(max_sub.at(var_name), -arg->ShuntingYard()->Evaluate());
        }
      }
    }
  }
  std::vector<std::pair<std::string, int>> max_sub_vec;
  std::copy(max_sub.begin(), max_sub.end(), std::back_inserter(max_sub_vec));
  return max_sub_vec;
}

std::unique_ptr<Expression>
Expression::ShuntingYard(bool recursive /*= true*/) {
  std::stack<Token> operator_stack;
  auto postfix_exp = std::unique_ptr<Expression>(new Expression());
  postfix_exp->tokens_.reserve(tokens_.size());
  for (Token x : tokens_) {
    switch (x.type) {
    case TokenType::kInteger: {
      postfix_exp->tokens_.push_back(x);
      break;
    }
    case TokenType::kVariable: {
      postfix_exp->tokens_.push_back(x);
      break;
    }
    case TokenType::kFunctionCall: {
      Token new_unit{x};
      if (recursive)
        for (auto &arg : new_unit.func().func_args)
          arg = arg->ShuntingYard();
      postfix_exp->tokens_.push_back(new_unit);
      break;
    }
    case TokenType::kOperator: {
      if (operator_stack.empty() || x.op() == '(') {
        operator_stack.push(x);
      } else if (x.op() == ')') {
        while (operator_stack.top().op() != '(') {
          postfix_exp->tokens_.push_back(operator_stack.top());
          operator_stack.pop();
        }
        operator_stack.pop();
      } else if (Token::comparePrecedence(operator_stack.top(), x)) {
        postfix_exp->tokens_.push_back(operator_stack.top());
        operator_stack.pop();
        operator_stack.push(x);
      } else {
        operator_stack.push(x);
      }
      break;
    }
    default:
      throw std::logic_error("Invalid expression containing element of type "
                             "TokenType::kNone");
    }
  }
  while (!operator_stack.empty()) {
    postfix_exp->tokens_.push_back(operator_stack.top());
    operator_stack.pop();
  }
  return postfix_exp;
}

// TODO (Paulius): 1) could use sstream for efficiency, 2) too many ToString
// operations
std::string Expression::ToString() const {
  std::string s;
  for (auto e : tokens_)
    s += e.ToString();
  return s;
}

std::string
Expression::ToString(std::function<std::string(Token &)> get_func_call) {
  std::string s;
  for (auto e : tokens_)
    s += e.ToString(get_func_call);
  return s;
}

std::ostream &operator<<(std::ostream &s, Expression exp) {
  for (auto e : exp.tokens_)
    s << e << " ";
  return s;
}
