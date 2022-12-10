#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <charconv>

namespace lzhcc {

auto Parser::primary() -> Expression * {
  switch (next_kind()) {
  case TokenKind::numeric: {
    auto token = consume();
    auto text = context_->literal(token.inner);
    int64_t value;
    std::from_chars(text.begin(), text.end(), value);
    return context_->create<IntegerExpr>(context_->int64(), value);
  }
  case TokenKind::open_paren: {
    consume();
    auto expr = expression();
    consume(TokenKind::close_paren);
    return expr;
  }
  default:
    context_->fatal(position_->location, "");
  }
}

auto Parser::multiplicative() -> Expression * {
  auto lhs = primary();
loop:
  switch (next_kind()) {
  case TokenKind::star: {
    consume();
    auto rhs = primary();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::multiply, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::slash: {
    consume();
    auto rhs = primary();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::divide, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::additive() -> Expression * {
  auto lhs = multiplicative();
loop:
  switch (next_kind()) {
  case TokenKind::plus: {
    consume();
    auto rhs = multiplicative();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::add, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::minus: {
    consume();
    auto rhs = multiplicative();
    auto type = context_->int64();
    lhs = context_->create<BinaryExpr>(BinaryKind::subtract, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::expression() -> Expression * { return additive(); }

} // namespace lzhcc