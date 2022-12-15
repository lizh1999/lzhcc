#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>
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
      std::vector<Expression *> arguments;
      while (!consume_if(TokenKind::close_paren)) {
        if (!arguments.empty()) {
          consume(TokenKind::comma);
        }
        arguments.push_back(assignment());
      }
      return create<CallExpr>(token, context_->int64(), std::move(arguments));
    } else {
      auto var = find_var(token);
      return create<VarRefExpr>(var);
    }
  }
  case TokenKind::string: {
    auto token = consume();
    auto raw = context_->literal(token->inner);
    assert(raw.front() == '"' && raw.back() == '"');
    raw = raw.substr(1, raw.size() - 2);
    std::string text;
    text.append(raw);
    text.push_back('\0');
    auto type = context_->array_of(context_->int8(), text.size());
    int inner = context_->push_literal(std::move(text));
    auto var = create_anon(type, inner);
    return create<VarRefExpr>(var);
  }
  case TokenKind::kw_sizeof: {
    consume();
    auto type = unary()->type();
    int size = context_->size_of(type);
    return context_->integer(size);
  }
  default:
    context_->fatal(position_->location, "");
  }
}

struct RefenceOplower {
  auto operator()(const ArrayType &arr) -> Expression * {
    auto type = context->pointer_to(arr.base);
    return context->create<UnaryExpr>(UnaryKind::refrence, type, operand);
  }
  auto operator()(auto &&) -> Expression * {
    auto type = context->pointer_to(operand->type());
    return context->create<UnaryExpr>(UnaryKind::refrence, type, operand);
  }
  Context *context;
  Expression *operand;
};

struct DerefOpLower {
  auto operator()(const ArrayType &arr) -> Expression * {
    return context->create<UnaryExpr>(UnaryKind::deref, arr.base, operand);
  }

  auto operator()(const PointerType &ptr) -> Expression * {
    return context->create<UnaryExpr>(UnaryKind::deref, ptr.base, operand);
  }

  auto operator()(auto &&) -> Expression * {
    context->fatal(position->location, "");
  }

  Context *context;
  Expression *operand;
  const Token *position;
};

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
    auto lower = RefenceOplower{context_, operand};
    return std::visit(lower, *operand->type());
  }
  case TokenKind::star: {
    auto token = consume();
    auto operand = unary();
    auto lower = DerefOpLower{context_, operand, token};
    return std::visit(lower, *operand->type());
  }
  default:
    return postfix();
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

struct AddOpLower {
  auto operator()(const IntegerType &, const IntegerType &) -> Expression * {
    return context->add(lhs->type(), lhs, rhs);
  }

  auto offset_of(const Type *type, Expression *n) -> Expression * {
    int size_bytes = context->size_of(type);
    auto size = context->integer(size_bytes);
    return context->multiply(size->type(), n, size);
  }

  auto operator()(const IntegerType &, const PointerType &ptr) -> Expression * {
    auto offset = offset_of(ptr.base, lhs);
    return context->add(rhs->type(), offset, rhs);
  }

  auto operator()(const IntegerType &, const ArrayType &arr) -> Expression * {
    auto offset = offset_of(arr.base, lhs);
    return context->add(rhs->type(), offset, rhs);
  }

  auto operator()(const PointerType &ptr, const IntegerType &) -> Expression * {
    auto offset = offset_of(ptr.base, rhs);
    return context->add(lhs->type(), lhs, offset);
  }

  auto operator()(const ArrayType &arr, const IntegerType &) -> Expression * {
    auto offset = offset_of(arr.base, rhs);
    return context->add(lhs->type(), lhs, offset);
  }

  auto operator()(auto &&, auto &&) -> Expression * {
    context->fatal(position->location, "");
  }

  Context *context;
  Expression *lhs;
  Expression *rhs;
  const Token *position;
};

struct SubtractOpLower {
  auto operator()(const IntegerType &, const IntegerType &) -> Expression * {
    return context->subtract(lhs->type(), lhs, rhs);
  }

  auto operator()(const PointerType &ptr, const IntegerType &) -> Expression * {
    int size_bytes = context->size_of(ptr.base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type(), rhs, size);
    return context->subtract(lhs->type(), lhs, offset);
  }

  auto operator()(const PointerType &ptr, const PointerType &) -> Expression * {
    int size_bytes = context->size_of(ptr.base);
    auto size = context->integer(size_bytes);
    auto bytes = context->subtract(size->type(), lhs, rhs);
    return context->divide(size->type(), bytes, size);
  }

  auto operator()(auto &&, auto &&) -> Expression * {
    context->fatal(position->location, "");
  }

  Context *context;
  Expression *lhs;
  Expression *rhs;
  const Token *position;
};

auto Parser::additive() -> Expression * {
  auto lhs = multiplicative();
loop:
  switch (next_kind()) {
  case TokenKind::plus: {
    auto token = consume();
    auto rhs = multiplicative();
    auto lower = AddOpLower{context_, lhs, rhs, token};
    lhs = std::visit(lower, *lhs->type(), *rhs->type());
    goto loop;
  }
  case TokenKind::minus: {
    auto token = consume();
    auto rhs = multiplicative();
    auto lower = SubtractOpLower{context_, lhs, rhs, token};
    lhs = std::visit(lower, *lhs->type(), *rhs->type());
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::postfix() -> Expression * {
  auto lhs = primary();
  while (auto token = consume_if(TokenKind::open_bracket)) {
    auto rhs = expression();
    consume(TokenKind::close_bracket);
    auto lower1 = AddOpLower{context_, lhs, rhs, token};
    lhs = std::visit(lower1, *lhs->type(), *rhs->type());
    auto lower2 = DerefOpLower{context_, lhs, token};
    lhs = std::visit(lower2, *lhs->type());
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
    auto type = lhs->type();

    auto visitor = overloaded{
        [](const PointerType &, const ArrayType &) -> void {},
        [&](const auto &l, const auto &r) -> void {
          using T = std::decay_t<decltype(l)>;
          using U = std::decay_t<decltype(r)>;
          if constexpr (!std::is_same_v<T, U>) {
            context_->fatal(loc, "");
          }
        },
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