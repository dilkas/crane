/*
    TODO :
    (1) Add support for functions where a function appears inside another
   function, specifically power. (2) Handle the sum function of wolfram
   (suggestion : define a separate function for it like power and Binomial) For
   (2), need to add support for the {...} list/tuple syntax of wolfram, or
   replace it beforehand  by something else.
*/

#include <algorithm>
#include <cctype>
#include <fstream>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <utility>
#include <vector>

using namespace std;

enum class exp_unit_type { Operator, Integer, FuncCall, Variable, None };

class Token;

ostream &operator<<(ostream &s, Token e);
ostream &operator<<(ostream &s, exp_unit_type e);
ostream &operator<<(ostream &s, list<Token> e);

vector<Token> Tokenize(string exp_str);
string expression_to_string(vector<Token> exp,
                            function<string(Token &)> get_func_call);
string expression_to_string(vector<Token> exp);
string get_function_name(Token &e);

class FuncCall {
public:
  string func_name;
  vector<vector<Token>> func_args;
  FuncCall(string call_str) {
    int num_open_brack = -1;
    int arg_start = 0;
    for (unsigned i{0}; i < call_str.size(); i++) {
      if (call_str.at(i) == '[') {
        if (num_open_brack == -1) {
          func_name = call_str.substr(0, i);
          arg_start = i + 1;
        }
        num_open_brack += 1;
      } else if (call_str.at(i) == ',' || call_str.at(i) == ']') {
        if (num_open_brack == 0) {
          string arg = call_str.substr(arg_start, i - arg_start);
          if (arg != "")
            func_args.push_back(Tokenize(arg));
          arg_start = i + 1;
        }
        if (call_str.at(i) == ']')
          num_open_brack -= 1;
      }
    }
  }

  FuncCall(const string &fname, const vector<vector<Token>> &fargs)
      : func_name{fname}, func_args{fargs} {}

  string toString(bool use_paren = false) {
    string arg_str = "";
    for (auto &arg : func_args)
      arg_str += expression_to_string(arg) + ",";
    if (use_paren)
      return func_name + "(" + arg_str.substr(0, arg_str.size() - 1) + ")";
    return func_name + "[" + arg_str.substr(0, arg_str.size() - 1) + "]";
  }

  string toCppString(vector<string> free_vars);
};

ostream &operator<<(ostream &s, FuncCall f) {
  string arg_str = "";
  for (auto arg : f.func_args)
    arg_str += expression_to_string(arg) + ",";
  s << f.func_name << "[" << arg_str.substr(0, arg_str.size() - 1) << "]";
  return s;
}

class Token {
protected:
  int _value = 0;
  char _op = '\0';
  FuncCall *_func_call = NULL;
  string _var = "";

public:
  exp_unit_type type = exp_unit_type::None;
  static const regex var_pattern;

  Token(int val) : type{exp_unit_type::Integer}, _value{val} {}
  Token(char op) : type{exp_unit_type::Operator}, _op{op} {}
  Token(string s) {
    if (regex_match(s, var_pattern)) {
      type = exp_unit_type::Variable;
      _var = s;
    } else {
      type = exp_unit_type::FuncCall;
      _func_call = new FuncCall{s};
    }
  }
  Token(int val, char op, FuncCall *func_call, string var, exp_unit_type t)
      : _value{val}, _op{op}, _func_call{func_call}, _var{var}, type{t} {}

  void setValue(int x) {
    type = exp_unit_type ::Integer;
    _value = x;
  }

  void setOp(char o) {
    type = exp_unit_type ::Operator;
    _op = o;
  }

  int &value() {
    if (type != exp_unit_type::Integer)
      throw logic_error("Cannot access value for non-Integer token");
    return _value;
  }

  char &op() {
    if (type != exp_unit_type::Operator)
      throw logic_error("Cannot access value for non-Operator token");
    return _op;
  }

  FuncCall &func() {
    if (type != exp_unit_type ::FuncCall)
      throw logic_error("Cannot access value for non-FuncCall token");
    return *_func_call;
  }

  string &var() {
    if (type != exp_unit_type::Variable)
      throw logic_error("Cannot access var for non-Variable token");
    return _var;
  }

  // stores the operator precedence values, higher precedence means more value
  // in the map
  static const map<char, int> operator_precedence_map;

  // returns true if op1 has >= precedence than op2
  static bool comparePrecedence(Token op1, Token op2) {
    if (op1.type != exp_unit_type::Operator ||
        op2.type != exp_unit_type::Operator) {
      throw logic_error("Invalid operand types, can only compare precedence of "
                        "operators with type exp_unit_type :: Operator");
    }
    return (operator_precedence_map.at(op1.op()) >=
            operator_precedence_map.at(op2.op()));
  }

  string toString(function<string(Token &)> get_func_call = [](Token &e) {
    return e.func().toString();
  }) {
    switch (type) {
    case exp_unit_type ::Integer: {
      return to_string(value());
    }
    case exp_unit_type ::Operator: {
      return string(1, op());
    }
    case exp_unit_type ::FuncCall: {
      return get_func_call(*this);
    }
    case exp_unit_type ::Variable: {
      return var();
    }
    default: {
      return "";
    }
    }
  }

  Token(const Token &e)
      : type{e.type}, _value{e._value}, _op{e._op}, _var{e._var} {
    if (e._func_call)
      _func_call = new FuncCall(*e._func_call);
  }

  Token operator=(const Token &e) {
    type = e.type;
    _value = e._value;
    _op = e._op;
    _var = e._var;
    delete _func_call;
    if (e._func_call)
      _func_call = new FuncCall(*e._func_call);
    return *this;
  }

  ~Token() { delete _func_call; }
};

const map<char, int> Token ::operator_precedence_map{
    {'^', 4}, {'*', 3}, {'/', 3}, {'+', 2}, {'-', 2}, {'(', 1}};
const regex Token ::var_pattern{"[a-zA-Z][a-zA-Z0-9]*"};

string FuncCall ::toCppString(vector<string> free_vars) {
  if (func_name == "Sum") {
    stringstream lambda;
    lambda << "([";
    for (unsigned i{0}; i < free_vars.size(); i++) {
      if (i != 0)
        lambda << ',';
      lambda << free_vars.at(i);
    }
    string iter_var = func_args.at(1).at(0).var();
    free_vars.push_back(iter_var);
    lambda << "](){mpz_class sum{0}; for (unsigned " << iter_var << " = "
           << func_args.at(2).at(0).toString() << "; " << iter_var
           << " <= " << func_args.at(3).at(0).toString() << "; " << iter_var
           << "++){ sum += ("
           << expression_to_string(func_args.at(0),
                                   [free_vars](Token &e) {
                                     if (e.func().func_name == "Binomial" ||
                                         e.func().func_name == "power" ||
                                         e.func().func_name == "Sum")
                                       return e.func().toCppString(free_vars);
                                     return e.func().toString(true);
                                   })
           << ");} return sum;})()";
    return lambda.str();
  } else if (func_name == "power" || func_name == "Binomial") {
    stringstream cpp_exp;
    cpp_exp << func_name << '(';
    for (int i{0}; i < func_args.size(); i++) {
      if (i != 0)
        cpp_exp << ',';
      cpp_exp << expression_to_string(func_args.at(i), [free_vars](Token &e) {
        if (e.func().func_name == "Binomial" || e.func().func_name == "power" ||
            e.func().func_name == "Sum")
          return e.func().toCppString(free_vars);
        return e.func().toString(true);
      });
    }
    cpp_exp << ')';
    return cpp_exp.str();
  } else {
    return toString(true);
  }
}

ostream &operator<<(ostream &s, Token e) {
  switch (e.type) {
  case exp_unit_type ::Integer: {
    s << e.value();
    break;
  }
  case exp_unit_type ::Operator: {
    s << e.op();
    break;
  }
  case exp_unit_type ::FuncCall: {
    s << e.func();
    break;
  }
  case exp_unit_type ::Variable: {
    s << e.var();
    break;
  }
  default: {
  }
  }
  return s;
}

ostream &operator<<(ostream &s, vector<Token> exp) {
  for (auto e : exp)
    s << e << " ";
  return s;
}

ostream &operator<<(ostream &s, list<Token> l) {
  for (auto e : l)
    s << e << " ";
  return s;
}

string expression_to_string(vector<Token> exp) {
  string s;
  for (auto e : exp) {
    s += e.toString();
  }
  return s;
}

string expression_to_string(vector<Token> exp,
                            function<string(Token &)> get_func_call) {
  string s;
  for (auto e : exp) {
    s += e.toString(get_func_call);
  }
  return s;
}

ostream &operator<<(ostream &s, exp_unit_type e) {
  switch (e) {
  case exp_unit_type ::Integer: {
    s << "Integer";
    break;
  }
  case exp_unit_type ::Operator: {
    s << "Operator";
    break;
  }
  case exp_unit_type ::FuncCall: {
    s << "FuncCall";
    break;
  }
  case exp_unit_type ::Variable: {
    s << "Variable";
    break;
  }
  }
  return s;
}

/** Transforms an infix expression into postfix notation.
 *
 *  NOTE: Assumes left to right associativity of all operators.
 */
vector<Token> shunting_yard(vector<Token> infix_exp, bool recursive = true) {
  stack<Token> operator_stack;
  vector<Token> postfix_exp;
  postfix_exp.reserve(infix_exp.size());
  for (Token x : infix_exp) {
    switch (x.type) {
    case exp_unit_type ::Integer: {
      postfix_exp.push_back(x);
      break;
    }
    case exp_unit_type ::Variable: {
      postfix_exp.push_back(x);
      break;
    }
    case exp_unit_type ::FuncCall: {
      Token new_unit{x};
      if (recursive) {
        for (auto &arg : new_unit.func().func_args) {
          arg = shunting_yard(arg);
        }
      }
      postfix_exp.push_back(new_unit);
      break;
    }
    case exp_unit_type::Operator: {
      if (operator_stack.empty() || x.op() == '(') {
        operator_stack.push(x);
      } else if (x.op() == ')') {
        while (operator_stack.top().op() != '(') {
          postfix_exp.push_back(operator_stack.top());
          operator_stack.pop();
        }
        operator_stack.pop();
      } else if (Token::comparePrecedence(operator_stack.top(), x)) {
        postfix_exp.push_back(operator_stack.top());
        operator_stack.pop();
        operator_stack.push(x);
      } else {
        operator_stack.push(x);
      }
      break;
    }
    default: {
      throw logic_error("Invalid expression containing element of type "
                        "operator_unit_type :: None");
    }
    }
  }
  while (!operator_stack.empty()) {
    postfix_exp.push_back(operator_stack.top());
    operator_stack.pop();
  }
  return postfix_exp;
}

// converts exponent from infix notation (a^b) to the power(a,b) notation
vector<Token> handle_pow(vector<Token> exp, bool recursive = true) {
  vector<Token> postfix_exp = shunting_yard(exp, false);
  stack<list<Token>> exp_stack;
  for (auto e : postfix_exp) {
    switch (e.type) {
    case exp_unit_type ::Integer: {
      exp_stack.push({e});
      break;
    }
    case exp_unit_type ::Variable: {
      exp_stack.push({e});
      break;
    }
    case exp_unit_type ::Operator: {
      list<Token> arg2 = exp_stack.top();
      exp_stack.pop();
      list<Token> arg1 = exp_stack.top();
      exp_stack.pop();
      list<Token> res;
      switch (e.op()) {
      case '+': {
        bool b1 = (arg1.size() > 1);
        bool b2 = (arg2.size() > 1);
        if (b1)
          res.push_back({'('});
        res.splice(res.end(), arg1);
        if (b1)
          res.push_back({')'});
        res.push_back({'+'});
        if (b2)
          res.push_back({'('});
        res.splice(res.end(), arg2);
        if (b2)
          res.push_back({')'});
        break;
      }
      case '-': {
        bool b1 = (arg1.size() > 1);
        bool b2 = (arg2.size() > 1);
        if (b1)
          res.push_back({'('});
        res.splice(res.end(), arg1);
        if (b1)
          res.push_back({')'});
        res.push_back({'-'});
        if (b2)
          res.push_back({'('});
        res.splice(res.end(), arg2);
        if (b2)
          res.push_back({')'});
        break;
      }
      case '*': {
        bool b1 = (arg1.size() > 1);
        bool b2 = (arg2.size() > 1);
        if (b1)
          res.push_back({'('});
        res.splice(res.end(), arg1);
        if (b1)
          res.push_back({')'});
        res.push_back({'*'});
        if (b2)
          res.push_back({'('});
        res.splice(res.end(), arg2);
        if (b2)
          res.push_back({')'});
        break;
      }
      case '/': {
        bool b1 = (arg1.size() > 1);
        bool b2 = (arg2.size() > 1);
        if (b1)
          res.push_back({'('});
        res.splice(res.end(), arg1);
        if (b1)
          res.push_back({')'});
        res.push_back({'/'});
        if (b2)
          res.push_back({'('});
        res.splice(res.end(), arg2);
        if (b2)
          res.push_back({')'});
        break;
      }
      case '^': {
        FuncCall *pow_func_call = new FuncCall{
            "power", {{arg1.begin(), arg1.end()}, {arg2.begin(), arg2.end()}}};
        res = {Token(0, '\0', pow_func_call, "", exp_unit_type ::FuncCall)};
        break;
      }
      }
      exp_stack.push({res});
      break;
    }
    case exp_unit_type ::FuncCall: {
      Token new_unit{e};
      if (recursive) {
        for (auto &arg : new_unit.func().func_args) {
          arg = handle_pow(arg);
        }
      }
      exp_stack.push({new_unit});
      break;
    }
    }
  }
  return {exp_stack.top().begin(), exp_stack.top().end()};
}

vector<Token> Tokenize(string exp_str) {
  vector<Token> exp;
  exp.reserve(exp_str.size());
  for (unsigned i{0}; i < exp_str.size(); i++) {
    if (isspace(exp_str.at(i)))
      continue;
    if (isdigit(exp_str.at(i))) {
      unsigned j = i;
      for (j; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      exp.push_back({stoi(exp_str.substr(i, j - i))});
      i = j - 1;
    } else if (isalpha(exp_str.at(i))) {
      int num_open_brack = 0;
      unsigned j = i;
      bool isVar = false;
      for (j;
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
        exp.push_back({exp_str.substr(i, j - i)});
        i = j - 1;
      } else {
        exp.push_back({exp_str.substr(i, j - i + 1)});
        i = j;
      }
    } else if (exp_str.at(i) == '-' && exp.size() == 0 &&
               isdigit(exp_str.at(i + 1))) {
      // unary minus
      unsigned j = i + 1;
      for (j; j < exp_str.size() && isdigit(exp_str.at(j)); j++)
        ;
      exp.push_back({stoi(exp_str.substr(i, j - i))});
      i = j - 1;
    } else {
      exp.push_back({exp_str.at(i)});
    }
  }
  return exp;
}

int get_func_val(Token f) {
  if (f.type != exp_unit_type ::FuncCall) {
    throw invalid_argument(
        "get_func_val accepts only exp_unit_type::FuncCall arguments");
  }
  return 0;
}

int get_var_val(Token f) {
  if (f.type != exp_unit_type ::Variable) {
    throw invalid_argument(
        "get_func_val accepts only exp_unit_type::FuncCall arguments");
  }
  return 0;
}

// evaluates an expression in the postfix notation
int evaluateExpression(vector<Token> exp, function<int(Token)> get_var_val,
                       function<int(Token)> get_func_val) {
  stack<Token> eval_stack;
  for (auto e : exp) {
    switch (e.type) {
    case exp_unit_type ::Integer: {
      eval_stack.push(e);
      break;
    }
    case exp_unit_type ::Variable: {
      eval_stack.push({get_var_val(e)});
      break;
    }
    case exp_unit_type ::Operator: {
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
    case exp_unit_type ::FuncCall: {
      Token f{e};
      for (auto &arg : e.func().func_args) {
        int v = evaluateExpression(arg, get_var_val, get_func_val);
        arg = vector<Token>{Token{v}};
      }
      eval_stack.push({get_func_val(f)});
      break;
    }
    }
  }
  return eval_stack.top().value();
}

string get_function_name(Token &e) {
  stringstream name;
  bool base_func = true;
  name << e.func().func_name << "_";
  for (auto arg : e.func().func_args) {
    if (arg.at(0).type == exp_unit_type ::Integer) {
      name << to_string(arg.at(0).value());
      base_func = false;
    } else
      name << 'x';
  }
  if (base_func)
    return e.func().func_name;
  return name.str();
}

string get_function_signature(Token &e, string func_ret_pref = "int",
                              string var_pref = "int", string end = ";") {
  stringstream signature;
  // get the argument list for the cpp code.
  string arg_list = "";
  for (auto arg : e.func().func_args)
    if (arg.at(0).type == exp_unit_type ::Variable)
      arg_list += var_pref + " " + arg.at(0).var() + ", ";
  if (arg_list.size() != 0)
    arg_list = arg_list.substr(0, arg_list.size() - 2);
  signature << func_ret_pref << " " << get_function_name(e) << "(" << arg_list
            << ")" << end;
  return signature.str();
}

// generates c++ definitions for the equations
string generate_function_def(string eqn) {
  stringstream code;
  size_t loc = eqn.find('=');
  Token lhs = Tokenize(eqn.substr(0, loc)).at(0);
  vector<Token> rhs =
      handle_pow(Tokenize(eqn.substr(loc + 1, eqn.size() - loc - 1)));
  string signature =
      get_function_signature(lhs, "mpz_class", "unsigned long", "");
  code << signature << "{\n";

  // check if the element is present in the cache
  code << "\tmpz_class& stored_val = ";
  stringstream stored_val_loc_stream;
  for (unsigned i{0}; i < lhs.func().func_args.size(); i++) {
    stored_val_loc_stream << "get_elem(";
  }
  stored_val_loc_stream << lhs.func().func_name << "_cache";
  for (unsigned i{0}; i < lhs.func().func_args.size(); i++) {
    stored_val_loc_stream << ", "
                          << expression_to_string(lhs.func().func_args.at(i))
                          << ")";
  }
  stored_val_loc_stream << ".n";
  string stored_val_loc = stored_val_loc_stream.str();
  code << stored_val_loc;
  code << ";\n\tif (stored_val != -1)\n\t\treturn stored_val;\n";

  // find the maximum subtractor among the arguments in the rhs
  map<string, int> max_sub;
  for (auto arg : lhs.func().func_args) {
    if (arg.at(0).type == exp_unit_type ::Variable) {
      max_sub.insert({arg.at(0).var(), 0});
    }
  }
  stack<vector<Token> *> arg_stack;
  arg_stack.push(&rhs);
  while (!arg_stack.empty()) {
    vector<Token> &arg_exp = *arg_stack.top();
    arg_stack.pop();
    for (Token &e : arg_exp) {
      if (e.type == exp_unit_type ::FuncCall) {
        if (e.func().func_name == "Binomial" || e.func().func_name == "power" ||
            e.func().func_name == "Sum") {
          for (vector<Token> &arg : e.func().func_args)
            arg_stack.push(&arg);
          continue;
        }
        for (vector<Token> arg : e.func().func_args) {
          // finding the variable name in the argument
          string var_name;
          for (auto u : arg) {
            if (u.type == exp_unit_type ::Variable) {
              var_name = u.var();
              break;
            }
          }
          if (max_sub.find(var_name) == max_sub.end())
            continue;

          // updating max_sub
          max_sub.at(var_name) =
              max(max_sub.at(var_name),
                  -evaluateExpression(shunting_yard(arg), get_var_val,
                                      get_func_val));
        }
      }
    }
  }
  vector<pair<string, int>> max_sub_vec;
  std::copy(max_sub.begin(), max_sub.end(), back_inserter(max_sub_vec));
  // handling the case where all args are +ve for the call with all variable
  // arguments
  vector<string> free_vars;
  for (vector<Token> &exp : lhs.func().func_args) {
    if (exp.at(0).type == exp_unit_type ::Variable)
      free_vars.push_back(exp.at(0).var());
  }
  if (max_sub_vec.size()) {
    code << "\tif (";
    for (unsigned i{0}; i < max_sub_vec.size(); i++) {
      if (i != 0)
        code << " && ";
      code << max_sub_vec.at(i).first << " >= " << max_sub_vec.at(i).second;
    }
    code << "){\n\t\tmpz_class ret_val = "
         << expression_to_string(rhs,
                                 [free_vars](Token &e) {
                                   if (e.func().func_name == "Binomial" ||
                                       e.func().func_name == "power" ||
                                       e.func().func_name == "Sum")
                                     return e.func().toCppString(free_vars);
                                   return e.func().toString(true);
                                 })
         << ";\n\t\t" << stored_val_loc
         << " = ret_val;\n\t\treturn ret_val;\n\t}\n";
  } else {
    code << "\tmpz_class ret_val = "
         << expression_to_string(rhs,
                                 [free_vars](Token &e) {
                                   if (e.func().func_name == "Binomial" ||
                                       e.func().func_name == "power" ||
                                       e.func().func_name == "Sum")
                                     return e.func().toCppString(free_vars);
                                   return e.func().toString(true);
                                 })
         << ";\n\t" << stored_val_loc << " = ret_val;\n\treturn ret_val;\n";
  }

  // handling the rest of the cases
  for (unsigned i{0}; i < max_sub_vec.size(); i++) {
    for (unsigned sub{0}; sub < max_sub_vec.at(i).second; sub++) {
      code << "\telse if (" << max_sub_vec.at(i).first << " == " << sub << "){";
      code << "\n\t\treturn " << lhs.toString([max_sub_vec, i, sub](Token &e) {
        Token transformed_e{e};
        for (unsigned j{0}; j < transformed_e.func().func_args.size(); j++) {
          if (transformed_e.func().func_args.at(j).at(0).type ==
                  exp_unit_type ::Variable &&
              transformed_e.func().func_args.at(j).at(0).var() ==
                  max_sub_vec.at(i).first) {
            transformed_e.func().func_args.at(j).at(0) = Token(int(sub));
            break;
          }
        }
        return get_function_signature(transformed_e, "", "", "");
      }) << ";\n\t}\n";
    }
  }
  code << "\treturn -1;\n}";
  return code.str();
}

string generate_cpp_code(vector<string> equations) {
  stringstream code;
  for (auto &eqn : equations) {
    code << get_function_signature(Tokenize(eqn.substr(0, eqn.find('='))).at(0),
                                   "mpz_class", "unsigned long")
         << "\n";
  }
  code << "\n";
  for (auto &eqn : equations) {
    code << generate_function_def(eqn) << "\n";
  }
  return code.str();
}

set<pair<string, int>> get_functions(vector<string> equations) {
  set<pair<string, int>> functions;
  for (string eqn : equations) {
    size_t loc = eqn.find('=');
    Token lhs = Tokenize(eqn.substr(0, loc)).at(0);
    functions.insert({lhs.func().func_name, lhs.func().func_args.size()});
  }
  return functions;
}

string generate_cpp_code_with_main(vector<string> equations,
                                   const vector<string> &domains) {
  stringstream code;
  code << "#include <array>" << endl
       << "#include <iostream>" << endl
       << "#include <string>" << endl
       << "#include <vector>" << endl
       << "#include <cmath>" << endl
       << "#include <gmpxx.h>" << endl
       << endl;

  // helper code
  code
      << "class cache_elem{\npublic : \n\tmpz_class n;\n\tcache_elem(mpz_class "
         "x) : n{x} {}\n\tcache_elem() : n{-1} {}\n};\n\n";
  code << "template <class T> T& get_elem(std::vector<T>& a, size_t n){\n\tif "
          "(n >= a.size()){\n\t\ta.resize(n+1);\n\t}\n\treturn a.at(n);\n}\n\n";
  code << "mpz_class Binomial(unsigned long n, unsigned long r){\n\tmpz_t "
          "ans;\n\tmpz_init(ans);\n\tmpz_bin_uiui(ans, n, r);\n\treturn "
          "mpz_class{ans};}\n\n";
  code << "mpz_class power(mpz_class x, unsigned long y){\n\tmpz_t "
          "ans;\n\tmpz_init(ans);\n\tmpz_pow_ui(ans, x.get_mpz_t(), "
          "y);\n\treturn mpz_class{ans};\n}\n\n";

  // make the caches
  set<pair<string, int>> functions = get_functions(equations);
  for (const auto &func : functions) {
    for (int i{0}; i < func.second; i++) {
      code << "std::vector<";
    }
    code << "cache_elem";
    for (int i{0}; i < func.second; i++) {
      code << ">";
    }
    code << " " << func.first << "_cache"
         << ";\n";
  }
  code << "\n";
  code << generate_cpp_code(equations) << endl;

  code << "int main(int argc, char *argv[]) {" << endl
       << "\tif (argc != " << domains.size() << " + 1) {" << endl
       << "\t\tstd::cerr << \"Please provide " << domains.size()
       << " arguments for the following domains (in this order): ";
  for (int i = 0; i < domains.size(); i++) {
    if (i != 0)
      code << ", ";
    code << domains.at(i);
  }
  code << "\" << std::endl;" << endl
       << "\t\texit(1);" << endl
       << "\t}" << endl
       << "\tstd::array<unsigned long, " << domains.size() << "> arguments;"
       << endl
       << "\tfor (int i = 1; i < argc; i++)" << endl
       << "\t\targuments[i-1] = std::stoul(argv[i]);" << endl
       << "\tstd::cout << std::apply(f0, arguments) << std::endl;" << endl
       << "}" << endl;

  return code.str();
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cerr << "Specify the input filename as the first argument." << endl;
    exit(1);
  }
  ifstream in_file{argv[1]};
  int num_equations{0};
  in_file >> num_equations;
  vector<string> equations;
  equations.resize(num_equations);
  for (unsigned i{0}; i < num_equations; i++) {
    in_file >> equations.at(i);
  }

  int num_domains;
  in_file >> num_domains;
  vector<string> domains;
  for (int i = 0; i < num_domains; i++) {
    string domain;
    in_file >> domain;
    domains.push_back(domain);
  }

  cout << generate_cpp_code_with_main(equations, domains) << endl;
}