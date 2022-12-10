#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>
#include <charconv>

namespace lzhcc {

auto Parser::primary() -> Expression * {
  assert(position_->kind == TokenKind::numeric);
  auto token = *position_++;
  auto text = context_->literal(token.inner);
  int64_t value;
  std::from_chars(text.begin(), text.end(), value);
  return context_->create<IntegerExpr>(context_->int64(), value);
}

auto Parser::additive() -> Expression * {
  auto lhs = primary();
loop:
  switch (position_->kind) {
  case TokenKind::plus: {
    position_++;
    auto rhs = primary();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::Add, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::minus: {
    position_++;
    auto rhs = primary();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::Subtract, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::expression() -> Expression * { return additive(); }

} // namespace lzhcc