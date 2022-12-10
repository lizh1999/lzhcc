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
  auto additive() -> Expression *;
  auto expression() -> Expression *;

  const Token *position_;
  Context *context_;
};

} // namespace lzhcc