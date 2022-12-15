#pragma once

#include "lzhcc.h"
#include <unordered_map>
#include <variant>

namespace lzhcc {

struct Scope {
  Scope *parent;
  std::unordered_map<int, Variable> var_map;
};

class Parser {
public:
  Parser(const Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Ast;

private:
  auto primary() -> Expression *;
  auto unary() -> Expression *;
  auto postfix() -> Expression *;
  auto multiplicative() -> Expression *;
  auto additive() -> Expression *;
  auto relational() -> Expression *;
  auto equality() -> Expression *;
  auto assignment() -> Expression *;
  auto expression() -> Expression *;

  auto block_stmt(bool is_top = false) -> Statement *;
  auto expr_stmt() -> Statement *;
  auto for_stmt() -> Statement *;
  auto if_stmt() -> Statement *;
  auto return_stmt() -> Statement *;
  auto while_stmt() -> Statement *;
  auto statement() -> Statement *;

  auto declspec() -> Type *;
  auto pointers(Type *base) -> Type *;
  auto array_dimensions(Type *base) -> Type *;
  auto function_parameters(Type *base) -> Type *;
  auto suffix_type(Type *base) -> Type *;
  auto declarator(Type *base) -> std::pair<const Token *, Type *>;
  auto declaration() -> std::vector<Statement *>;
  auto global(const Token *name, Type *base, Type *type)
      -> std::vector<Global *>;
  auto function(const Token *name, Type *type) -> Function *;

  auto next_kind() const -> TokenKind;
  auto consume() -> const Token *;
  auto consume(TokenKind kind) -> const Token *;
  auto consume_if(TokenKind kind) -> const Token *;

  template <class T, class... Args> auto create(Args &&...args) {
    return context_->create<T>(std::forward<Args>(args)...);
  }

  auto entry_scope() -> void;
  auto leave_scope() -> void;
  auto create_local(const Token *token, const Type *type) -> Local *;
  auto create_global(const Token *token, const Type *type, int init = -1)
      -> Global *;
  auto create_anon(const Type *type, int init = -1) -> Global *;
  auto find_var(const Token *token) -> Variable;
  auto unique_name() -> std::string_view;

  const Token *position_;
  Context *context_;

  int stack_size;
  int max_stack_size;
  Scope *current_;
  Scope file_scope_;
  int unique_id_ = 0;

  std::vector<Global *> *globals_;
};

} // namespace lzhcc