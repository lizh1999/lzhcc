#pragma once

#include "lzhcc.h"
#include <unordered_map>

namespace lzhcc {

class Parser {
public:
  Parser(const Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Statement *;

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
  auto return_stmt() -> Statement *;
  auto statement() -> Statement *;

  auto next_kind() const -> TokenKind;
  auto consume() -> const Token*;
  auto consume(TokenKind kind) -> const Token*;

  template <class T, class... Args> auto create(Args &&...args) {
    return context_->create<T>(std::forward<Args>(args)...);
  }

  auto get_or_allocate(int identifier) -> Variable *;

  const Token *position_;
  Context *context_;

  std::unordered_map<int, Variable *> var_map_;
};

} // namespace lzhcc