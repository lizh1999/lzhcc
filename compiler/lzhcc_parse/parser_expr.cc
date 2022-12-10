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
    return create<IntegerExpr>(context_->int64(), value);
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

auto Parser::unary() -> Expression * {
  switch (next_kind()) {
  case TokenKind::plus:
    consume();
    return unary();
  case TokenKind::minus: {
    consume();
    auto operand = unary();
    auto type = context_->int64();
    return create<UnaryExpr>(UnaryKind::negative, type, operand);
  }
  default:
    return primary();
  }
}

auto Parser::multiplicative() -> Expression * {
  auto lhs = unary();
loop:
  switch (next_kind()) {
  case TokenKind::star: {
    consume();
    auto rhs = unary();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::multiply, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::slash: {
    consume();
    auto rhs = unary();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::divide, type, lhs, rhs);
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
    lhs = create<BinaryExpr>(BinaryKind::add, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::minus: {
    consume();
    auto rhs = multiplicative();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::subtract, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::relational() -> Expression * {
  auto lhs = additive();
loop:
  switch (next_kind()) {
  case TokenKind::less: {
    consume();
    auto rhs = additive();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::less_than, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::less_equal: {
    consume();
    auto rhs = additive();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::less_equal, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::greater: {
    consume();
    auto rhs = additive();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::less_than, type, rhs, lhs);
    goto loop;
  }
  case TokenKind::greater_equal: {
    consume();
    auto rhs = additive();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::less_equal, type, rhs, lhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::equality() -> Expression * {
  auto lhs = relational();
loop:
  switch (next_kind()) {
  case TokenKind::equal_equal: {
    consume();
    auto rhs = relational();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::equal, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::exclaim_equal: {
    consume();
    auto rhs = relational();
    auto type = context_->int64();
    lhs = create<BinaryExpr>(BinaryKind::not_equal, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::expression() -> Expression * { return equality(); }

} // namespace lzhcc