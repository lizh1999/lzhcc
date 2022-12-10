#pragma once

#include "lzhcc.h"

namespace lzhcc {

class Parser {
public:
  Parser(const Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Expression *;

private:
  auto primary() -> Expression *;
  auto unary() -> Expression *;
  auto multiplicative() -> Expression *;
  auto additive() -> Expression *;
  auto relational() -> Expression *;
  auto equality() -> Expression *;
  auto expression() -> Expression *;

  auto next_kind() const -> TokenKind;
  auto consume() -> Token;
  auto consume(TokenKind kind) -> Token;

  template <class T, class... Args> auto create(Args &&...args) {
    return context_->create<T>(std::forward<Args>(args)...);
  }

  const Token *position_;
  Context *context_;
};

} // namespace lzhcc