#pragma once

#include "lzhcc.h"
#include <unordered_map>

namespace lzhcc {

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

inline auto size_of = overloaded{
    [](const IntegerType &type) -> const int { return type.size_bytes; },
    [](const PointerType &) -> const int { return 8; },
};

struct Scope {
  Scope *parent;
  std::unordered_map<int, Variable *> var_map;
};

class Parser {
public:
  Parser(const Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Function *;

private:
  auto primary() -> Expression *;
  auto unary() -> Expression *;
  auto multiplicative() -> Expression *;
  auto additive() -> Expression *;
  auto relational() -> Expression *;
  auto equality() -> Expression *;
  auto assignment() -> Expression *;
  auto expression() -> Expression *;

  auto block_stmt() -> Statement *;
  auto expr_stmt() -> Statement *;
  auto for_stmt() -> Statement *;
  auto if_stmt() -> Statement *;
  auto return_stmt() -> Statement *;
  auto while_stmt() -> Statement *;
  auto statement() -> Statement *;

  auto declspec() -> Type *;
  auto pointers(Type *base) -> Type *;
  auto declarator(Type *base) -> Variable *;
  auto declaration() -> std::vector<Statement*>;

  auto next_kind() const -> TokenKind;
  auto consume() -> const Token *;
  auto consume(TokenKind kind) -> const Token *;
  auto consume_if(TokenKind kind) -> const Token *;

  template <class T, class... Args> auto create(Args &&...args) {
    return context_->create<T>(std::forward<Args>(args)...);
  }

  auto entry_scope() -> void;
  auto leave_scope() -> void;
  auto create_var(const Token *token, const Type *type) -> Variable *;
  auto find_var(const Token *token) -> Variable *;

  const Token *position_;
  Context *context_;

  int stack_size;
  int max_stack_size;
  Scope *scope_;
};

} // namespace lzhcc