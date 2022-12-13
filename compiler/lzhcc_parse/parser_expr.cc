#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <charconv>
#include <type_traits>
#include <variant>

namespace lzhcc {

auto Parser::primary() -> Expression * {
  switch (next_kind()) {
  case TokenKind::numeric: {
    auto token = consume();
    auto text = context_->literal(token->inner);
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
  case TokenKind::identifier: {
    auto token = consume();
    if (consume_if(TokenKind::open_paren)) {
      consume(TokenKind::close_paren);
      return create<CallExpr>(token, context_->int64());
    } else {
      auto var = find_var(token);
      return create<VarRefExpr>(var);
    }
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
  case TokenKind::amp: {
    consume();
    auto operand = unary();
    auto type = context_->pointer_to(operand->type());
    return create<UnaryExpr>(UnaryKind::refrence, type, operand);
  }
  case TokenKind::star: {
    auto token = consume();
    auto operand = unary();
    auto visitor = [&](auto &&arg) -> const Type * {
      using T = std::decay_t<decltype(arg)>;
      if constexpr (std::is_same_v<T, PointerType>) {
        return arg.base;
      } else {
        context_->fatal(token->location, "");
      }
    };
    auto type = std::visit(visitor, *operand->type());
    return create<UnaryExpr>(UnaryKind::deref, type, operand);
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
    auto loc = consume()->location;
    auto rhs = multiplicative();

    auto visitor = overloaded{
        [&](const IntegerType &, const IntegerType &) -> const Type * {
          return lhs->type();
        },
        [&](const IntegerType &, const PointerType &ptr) -> const Type * {
          int size_bytes = std::visit(size_of, *ptr.base);
          auto size = create<IntegerExpr>(context_->int64(), size_bytes);
          lhs = create<BinaryExpr>(BinaryKind::multiply, context_->int64(), lhs,
                                   size);
          return rhs->type();
        },
        [&](const PointerType &ptr, const IntegerType &) -> const Type * {
          int size_bytes = std::visit(size_of, *ptr.base);
          auto size = create<IntegerExpr>(context_->int64(), size_bytes);
          rhs = create<BinaryExpr>(BinaryKind::multiply, context_->int64(), rhs,
                                   size);
          return lhs->type();
        },
        [&](auto &&, auto &&) -> const Type * { context_->fatal(loc, ""); },
    };
    auto type = std::visit(visitor, *lhs->type(), *rhs->type());
    lhs = create<BinaryExpr>(BinaryKind::add, type, lhs, rhs);
    goto loop;
  }
  case TokenKind::minus: {
    auto loc = consume()->location;
    auto rhs = multiplicative();
    int divide = 1;
    auto visitor = overloaded{
        [&](const IntegerType &, const IntegerType &) -> const Type * {
          return lhs->type();
        },
        [&](const PointerType &ptr, const IntegerType &) -> const Type * {
          int size_bytes = std::visit(size_of, *ptr.base);
          auto size = create<IntegerExpr>(context_->int64(), size_bytes);
          rhs = create<BinaryExpr>(BinaryKind::multiply, context_->int64(), rhs,
                                   size);
          return lhs->type();
        },
        [&](const PointerType &ptr, const PointerType &) -> const Type * {
          divide = std::visit(size_of, *ptr.base);
          return context_->int64();
        },
        [&](auto &&, auto &&) -> const Type * { context_->fatal(loc, ""); },
    };
    auto type = std::visit(visitor, *lhs->type(), *rhs->type());
    lhs = create<BinaryExpr>(BinaryKind::subtract, type, lhs, rhs);
    if (divide != 1) {
      auto size = create<IntegerExpr>(context_->int64(), divide);
      lhs = create<BinaryExpr>(BinaryKind::divide, type, lhs, size);
    }
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

auto Parser::assignment() -> Expression * {
  auto lhs = equality();
loop:
  switch (next_kind()) {
  case TokenKind::equal: {
    int loc = consume()->location;
    auto rhs = assignment();
    auto type = rhs->type();

    auto visitor = [&](auto &&l, auto &&r) {
      using T = std::decay_t<decltype(l)>;
      using U = std::decay_t<decltype(r)>;
      if constexpr (!std::is_same_v<T, U>) {
        context_->fatal(loc, "");
      }
    };
    std::visit(visitor, *lhs->type(), *rhs->type());
    lhs = create<BinaryExpr>(BinaryKind::assign, type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::expression() -> Expression * { return assignment(); }

} // namespace lzhcc