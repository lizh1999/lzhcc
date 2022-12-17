#pragma once

#include "lzhcc.h"

namespace lzhcc {

struct Scope {
  int old_stack_size;
  std::unordered_map<int, Value *> var_map;
  std::unordered_map<int, Type *> tag_map;
};

using ParamNames = std::vector<Token *>;

class Parser {
public:
  Parser(Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Module;

private:
  auto primary() -> Expr *;
  auto unary() -> Expr *;
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
  auto declspec() -> Type *;
  auto pointers(Type *base) -> Type *;
  auto array_dimensions(Type *base) -> Type *;
  auto function_parameters(Type *base, ParamNames *param_names) -> Type *;
  auto suffix_type(Type *base, ParamNames *param_names) -> Type *;
  auto declarator(Type *base, ParamNames *param_names = 0)
      -> std::pair<Token *, Type *>;
  auto declaration() -> std::vector<Stmt *>;
  auto global(Token *name, Type *base, Type *type) -> void;
  auto function(Token *name, Type *type, ParamNames param_names) -> void;

  auto next_kind() -> TokenKind;
  auto consume() -> Token *;
  auto consume(TokenKind kind) -> Token *;
  auto consume_if(TokenKind kind) -> Token *;

  template <class T, class... Args> auto create(Args &&...args) {
    return context_->create<T>(std::forward<Args>(args)...);
  }

  auto entry_scope() -> void;
  auto leave_scope() -> void;
  auto create_local(Token *token, Type *type) -> LValue *;
  auto create_global(Token *token, Type *type, uint8_t *init = 0) -> void;
  auto create_function(Token *token, Type *type, int stack_size, Stmt *stmt,
                       std::vector<LValue *> params) -> void;
  auto create_anon(Type *type, uint8_t *init = 0) -> GValue *;
  auto create_tag(Token *token, Type *type) -> void;
  auto find_var(int name) -> Value *;
  auto find_tag(int name) -> Type *;
  auto unique_name() -> std::pair<std::string_view, int>;

  Token *position_;
  Context *context_;

  int stack_size_;
  int max_stack_size_;
  std::deque<Scope> scopes_;
  int unique_id_ = 0;
};

} // namespace lzhcc