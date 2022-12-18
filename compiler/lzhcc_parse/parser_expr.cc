#include "lzhcc_parse.h"
#include <cassert>
#include <cctype>
#include <charconv>

namespace lzhcc {

auto Parser::primary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::numeric: {
    auto token = consume();
    auto text = context_->storage(token->inner);
    int32_t value;
    std::from_chars(text.begin(), text.end(), value);
    return context_->integer(value);
  }
  case TokenKind::open_paren: {
    auto token = consume();
    if (next_kind() != TokenKind::open_brace) {
      auto expr = expression();
      consume(TokenKind::close_paren);
      return expr;
    } else {
      auto stmt = cast<BlockStmt>(block_stmt());
      consume(TokenKind::close_paren);
      auto &stmts = stmt->stmts;
      if (stmts.empty() || stmts.back()->kind != StmtKind::expr) {
        context_->fatal(token->location, "");
      }
      auto type = cast<ExprStmt>(stmts.back())->expr->type;
      return context_->stmt_expr(type, stmt);
    }
  }
  case TokenKind::identifier: {
    auto token = consume();
    auto name = context_->storage(token->inner);
    if (consume_if(TokenKind::open_paren)) {
      std::vector<Expr *> arguments;
      while (!consume_if(TokenKind::close_paren)) {
        if (!arguments.empty()) {
          consume(TokenKind::comma);
        }
        arguments.push_back(assignment());
      }
      return context_->call(name, context_->int32(), std::move(arguments));
    } else {
      auto var = find_var(token->inner);
      if (!var) {
        context_->fatal(token->location, "");
      }
      return context_->value(var);
    }
  }
  case TokenKind::string:
    return string();
  case TokenKind::kw_sizeof: {
    consume();
    auto type = unary()->type;
    int size = context_->size_of(type);
    return context_->integer(size);
  }
  default:
    context_->fatal(position_->location, "");
  }
}

static auto low_refernce_op(Context *context, Expr *operand) -> Expr * {
  if (operand->type->kind == TypeKind::array) {
    auto arr = cast<ArrayType>(operand->type);
    auto type = context->pointer_to(arr->base);
    return context->refrence(type, operand);
  } else {
    auto type = context->pointer_to(operand->type);
    return context->refrence(type, operand);
  }
}

static auto low_deref_op(Context *context, Expr *operand, int loc) -> Expr * {
  switch (operand->type->kind) {
  case TypeKind::pointer: {
    auto ptr = cast<PointerType>(operand->type);
    return context->deref(ptr->base, operand);
  }
  case TypeKind::array: {
    auto arr = cast<ArrayType>(operand->type);
    return context->deref(arr->base, operand);
  }
  case TypeKind::integer:
  case TypeKind::function:
  case TypeKind::record:
    context->fatal(loc, "");
  }
}

auto Parser::unary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::plus:
    consume();
    return unary();
  case TokenKind::minus: {
    consume();
    auto operand = unary();
    auto type = context_->int32();
    return context_->negative(type, operand);
  }
  case TokenKind::amp: {
    consume();
    auto operand = unary();
    return low_refernce_op(context_, operand);
  }
  case TokenKind::star: {
    auto token = consume();
    auto operand = unary();
    return low_deref_op(context_, operand, token->location);
  }
  default:
    return postfix();
  }
}

auto Parser::multiplicative() -> Expr * {
  auto lhs = unary();
loop:
  switch (next_kind()) {
  case TokenKind::star: {
    consume();
    auto rhs = unary();
    auto type = context_->int32();
    lhs = context_->multiply(type, lhs, rhs);
    goto loop;
  }
  case TokenKind::slash: {
    consume();
    auto rhs = unary();
    auto type = context_->int32();
    lhs = context_->divide(type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

static constexpr auto pattern(TypeKind lhs, TypeKind rhs) -> int {
  return static_cast<int>(lhs) * 65536 | static_cast<int>(rhs);
}

static auto low_add_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::integer, TypeKind::array):
    std::swap(lhs, rhs);
    [[fallthrough]];
  case pattern(TypeKind::array, TypeKind::integer): {
    auto arr = cast<ArrayType>(lhs->type);
    int size_bytes = context->size_of(arr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, size, rhs);
    return context->add(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::integer, TypeKind::pointer):
    std::swap(lhs, rhs);
    [[fallthrough]];
  case pattern(TypeKind::pointer, TypeKind::integer): {
    auto ptr = cast<PointerType>(lhs->type);
    int size_bytes = context->size_of(ptr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, size, rhs);
    return context->add(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::integer, TypeKind::integer): {
    return context->add(lhs->type, lhs, rhs);
  }
  default:
    context->fatal(loc, "");
  }
}

static auto low_sub_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::integer, TypeKind::integer):
    return context->subtract(lhs->type, lhs, rhs);
  case pattern(TypeKind::pointer, TypeKind::integer): {
    auto ptr = cast<PointerType>(lhs->type);
    int size_bytes = context->size_of(ptr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, rhs, size);
    return context->subtract(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::pointer, TypeKind::pointer): {
    auto ptr = cast<PointerType>(lhs->type);
    int size_bytes = context->size_of(ptr->base);
    auto size = context->integer(size_bytes);
    auto bytes = context->subtract(size->type, lhs, rhs);
    return context->divide(size->type, bytes, size);
  }
  default:
    context->fatal(loc, "");
  }
}

auto Parser::additive() -> Expr * {
  auto lhs = multiplicative();
loop:
  switch (next_kind()) {
  case TokenKind::plus: {
    auto token = consume();
    auto rhs = multiplicative();
    lhs = low_add_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  case TokenKind::minus: {
    auto token = consume();
    auto rhs = multiplicative();
    lhs = low_sub_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

static auto low_member_op(Context *context, Expr *lhs, int rhs, int loc)
    -> Expr * {
  if (lhs->type->kind != TypeKind::record) {
    context->fatal(loc, "");
  }
  auto record = cast<RecordType>(lhs->type);
  auto it = record->member_map.find(rhs);
  if (it == record->member_map.end()) {
    context->fatal(loc, "");
  }
  auto &member = it->second;
  return context->member(member.type, lhs, member.offset);
}

auto Parser::postfix() -> Expr * {
  auto lhs = primary();
loop:
  switch (next_kind()) {
  case TokenKind::open_bracket: {
    auto token = consume();
    auto rhs = expression();
    consume(TokenKind::close_bracket);
    lhs = low_add_op(context_, lhs, rhs, token->location);
    lhs = low_deref_op(context_, lhs, token->location);
    goto loop;
  }
  case TokenKind::dot: {
    auto token = consume();
    auto ident = consume(TokenKind::identifier);
    lhs = low_member_op(context_, lhs, ident->inner, token->location);
    goto loop;
  }
  case TokenKind::arrow: {
    auto token = consume();
    auto ident = consume(TokenKind::identifier);
    lhs = low_deref_op(context_, lhs, token->location);
    lhs = low_member_op(context_, lhs, ident->inner, token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::relational() -> Expr * {
  auto lhs = additive();
loop:
  switch (next_kind()) {
  case TokenKind::less: {
    consume();
    auto rhs = additive();
    auto type = context_->int32();
    lhs = context_->less_than(type, lhs, rhs);
    goto loop;
  }
  case TokenKind::less_equal: {
    consume();
    auto rhs = additive();
    auto type = context_->int32();
    lhs = context_->less_equal(type, lhs, rhs);
    goto loop;
  }
  case TokenKind::greater: {
    consume();
    auto rhs = additive();
    auto type = context_->int32();
    lhs = context_->less_than(type, rhs, lhs);
    goto loop;
  }
  case TokenKind::greater_equal: {
    consume();
    auto rhs = additive();
    auto type = context_->int32();
    lhs = context_->less_equal(type, rhs, lhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::equality() -> Expr * {
  auto lhs = relational();
loop:
  switch (next_kind()) {
  case TokenKind::equal_equal: {
    consume();
    auto rhs = relational();
    auto type = context_->int32();
    lhs = context_->equal(type, lhs, rhs);
    goto loop;
  }
  case TokenKind::exclaim_equal: {
    consume();
    auto rhs = relational();
    auto type = context_->int32();
    lhs = context_->not_equal(type, lhs, rhs);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

static auto low_assign_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::integer, TypeKind::integer):
  case pattern(TypeKind::pointer, TypeKind::pointer):
  case pattern(TypeKind::pointer, TypeKind::array):
  case pattern(TypeKind::record, TypeKind::record):
    return context->assign(lhs->type, lhs, rhs);
  default:
    context->fatal(loc, "");
  }
}

auto Parser::assignment() -> Expr * {
  auto lhs = equality();
loop:
  switch (next_kind()) {
  case TokenKind::equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = low_assign_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::expression() -> Expr * {
  auto lhs = assignment();
  while (consume_if(TokenKind::comma)) {
    auto rhs = expression();
    lhs = context_->comma(rhs->type, lhs, rhs);
  }
  return lhs;
}

} // namespace lzhcc