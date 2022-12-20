#pragma once

#include "lzhcc.h"

namespace lzhcc {

auto low_assign_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr *;

class Variable {
  enum class Kind : uint8_t {
    null,
    value,
    type,
  };
  using enum Kind;

public:
  Variable() : data(0), kind(null) {}
  Variable(Value *value) : data(from(value)), kind(Kind::value) {}
  Variable(Type *type) : data(from(type)), kind(Kind::type) {}
  operator Value *() { return kind == value ? into<Value>() : nullptr; }
  operator Type *() { return kind == type ? into<Type>() : nullptr; }

private:
  static auto from(void *pointer) -> uint64_t {
    return reinterpret_cast<uint64_t>(pointer) >> 2;
  }
  template <class T> auto into() -> T * {
    return reinterpret_cast<T *>(data << 2);
  }
  uint64_t data : 62;
  Kind kind : 2;
};

struct Scope {
  int old_stack_size;
  std::unordered_map<int, Variable> var_map;
  std::unordered_map<int, Type *> tag_map;
};

using ParamNames = std::vector<Token *>;

class Parser {
public:
  Parser(Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Module;

private:
  auto integer() -> Expr *;
  auto string() -> Expr *;
  auto call(Token *name) -> Expr *;
  auto primary() -> Expr *;
  auto unary() -> Expr *;
  auto cast() -> Expr *;
  auto postfix() -> Expr *;
  auto multiplicative() -> Expr *;
  auto additive() -> Expr *;
  auto relational() -> Expr *;
  auto equality() -> Expr *;
  auto assignment() -> Expr *;
  auto expression() -> Expr *;

  auto block_stmt(bool is_top = false) -> Stmt *;
  auto expr_stmt() -> Stmt *;
  auto for_stmt() -> Stmt *;
  auto if_stmt() -> Stmt *;
  auto return_stmt() -> Stmt *;
  auto while_stmt() -> Stmt *;
  auto statement() -> Stmt *;

  auto struct_decl() -> Type *;
  auto union_decl() -> Type *;

  struct VarAttr {
    bool is_typedef;
  };

  auto is_typename(Token *token) -> bool;
  auto declspec(VarAttr *attr = 0) -> Type *;
  auto pointers(Type *base) -> Type *;
  auto array_dimensions(Type *base) -> Type *;
  auto function_parameters(Type *base, ParamNames *param_names) -> Type *;
  auto suffix_type(Type *base, ParamNames *param_names) -> Type *;
  auto declarator(Type *base, ParamNames *param_names = 0)
      -> std::pair<Token *, Type *>;
  auto abstract_declarator(Type *base) -> Type *;
  auto type_define(Type *base) -> void;
  auto declaration() -> std::vector<Stmt *>;
  auto global(Token *name, Type *base, Type *type) -> void;
  auto function(Token *name, Type *type, ParamNames param_names) -> void;

  auto next_is(TokenKind kind) -> bool;
  auto next_kind() -> TokenKind;
  auto consume() -> Token *;
  auto consume(TokenKind kind) -> Token *;
  auto consume_if(TokenKind kind) -> Token *;

  auto entry_scope() -> void;
  auto leave_scope() -> void;
  auto create_declaration(Token *token, Type *type) -> void;
  auto create_typedef(Token *token, Type *type) -> void;
  auto create_local(Token *token, Type *type) -> LValue *;
  auto create_global(Token *token, Type *type, uint8_t *init = 0) -> void;
  auto create_function(Token *token, Type *type, int stack_size, Stmt *stmt,
                       std::vector<LValue *> params) -> void;
  auto create_anon(Type *type, uint8_t *init = 0) -> GValue *;
  auto create_tag(Token *token, Type *type) -> void;
  auto find_var(int name) -> Variable;
  auto find_value(int name) -> Value *;
  auto find_type(int name) -> Type *;
  auto find_tag(int name) -> Type *;
  auto unique_name() -> std::pair<std::string_view, int>;

  Token *position_;
  Context *context_;

  Type *ret_;

  int stack_size_;
  int max_stack_size_;
  std::deque<Scope> scopes_;
  int unique_id_ = 0;
};

} // namespace lzhcc