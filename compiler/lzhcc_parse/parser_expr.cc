#include "lzhcc_parse.h"
#include <cassert>
#include <cctype>

namespace lzhcc {

auto Parser::call(Token *token) -> Expr * {
  auto name = context_->storage(token->inner);
  consume(TokenKind::open_paren);
  std::vector<Expr *> args;
  while (!consume_if(TokenKind::close_paren)) {
    if (!args.empty()) {
      consume(TokenKind::comma);
    }
    args.push_back(assignment());
  }
  auto var = find_value(token->inner);
  if (var->type->kind != TypeKind::function) {
    context_->fatal(token->inner, "");
  }
  auto function_type = cast<FunctionType>(var->type);
  int n = args.size();
  auto &params = function_type->params;
  if (n != params.size()) {
    context_->fatal(token->location, "");
  }
  for (int i = 0; i < n; i++) {
    args[i] = context_->cast(params[i], args[i]);
  }
  return context_->call(name, function_type->ret, std::move(args));
}

auto Parser::primary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::numeric:
    return integer();
  case TokenKind::character:
    return character();
  case TokenKind::open_paren: {
    auto token = consume();
    if (!next_is(TokenKind::open_brace)) {
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
    if (next_is(TokenKind::open_paren)) {
      return call(token);
    } else {
      auto var = find_var(token->inner);
      if (Value *value = var; value) {
        return context_->value(value);
      } else if (int value; var.get(&value)) {
        return context_->integer(value);
      } else {
        context_->fatal(token->location, "");
      }
    }
  }
  case TokenKind::string:
    return string();
  case TokenKind::kw_sizeof: {
    consume();
    Type *type = nullptr;
    if (!next_is(TokenKind::open_paren) || !is_typename(position_ + 1)) {
      type = unary()->type;
    } else {
      consume(TokenKind::open_paren);
      type = abstract_declarator(declspec());
      consume(TokenKind::close_paren);
    }
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
    if (ptr->base->kind == TypeKind::kw_void) {
      context->fatal(loc, "");
    }
    return context->deref(ptr->base, operand);
  }
  case TypeKind::array: {
    auto arr = cast<ArrayType>(operand->type);
    return context->deref(arr->base, operand);
  }
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::function:
  case TypeKind::record:
  case TypeKind::kw_void:
    context->fatal(loc, "");
  }
}

static auto low_cast_op(Context *context, Type *type, Expr *operand) -> Expr * {
  return context->cast(type, operand);
}

static auto convert(Context *context, Expr *lhs, Expr *rhs)
    -> std::tuple<Expr *, Expr *, Type *> {
  if (lhs->type->kind == TypeKind::boolean) {
    lhs = context->cast(context->int32(), lhs);
  }
  if (rhs->type->kind == TypeKind::boolean) {
    rhs = context->cast(context->int32(), rhs);
  }
  assert(lhs->type->kind == TypeKind::integer);
  assert(rhs->type->kind == TypeKind::integer);
  auto lhs_type = cast<IntegerType>(lhs->type);
  auto rhs_type = cast<IntegerType>(rhs->type);
  using enum IntegerKind;

  auto target = std::max({lhs_type->kind, rhs_type->kind, word});
  auto type = target == word ? context->int32() : context->int64();
  if (target != lhs_type->kind) {
    lhs = low_cast_op(context, type, lhs);
  }
  if (target != rhs_type->kind) {
    rhs = low_cast_op(context, type, rhs);
  }
  return std::tuple(lhs, rhs, type);
}

auto convert(Context *context, Expr *operand) -> std::pair<Expr *, Type *> {
  assert(operand->type->kind == TypeKind::integer);
  auto operand_type = cast<IntegerType>(operand->type);
  using enum IntegerKind;

  auto target = std::max(operand_type->kind, word);
  auto type = target == word ? context->int32() : context->int64();
  if (target != operand_type->kind) {
    operand = low_cast_op(context, type, operand);
  }
  return std::pair(operand, type);
}

auto Parser::unary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::plus: {
    auto token = consume();
    auto operand = cast();
    if (operand->type->kind != TypeKind::integer) {
      context_->fatal(token->location, "");
    }
    return convert(context_, operand).first;
  }
  case TokenKind::minus: {
    auto token = consume();
    auto operand = cast();
    if (operand->type->kind != TypeKind::integer) {
      context_->fatal(token->location, "");
    }
    auto [o, type] = convert(context_, operand);
    return context_->negative(type, o);
  }
  case TokenKind::amp: {
    consume();
    auto operand = cast();
    return low_refernce_op(context_, operand);
  }
  case TokenKind::star: {
    auto token = consume();
    auto operand = cast();
    return low_deref_op(context_, operand, token->location);
  }
  default:
    return postfix();
  }
}

auto Parser::cast() -> Expr * {
  if (!next_is(TokenKind::open_paren) || !is_typename(position_ + 1)) {
    return unary();
  } else {
    consume(TokenKind::open_paren);
    auto type = abstract_declarator(declspec());
    consume(TokenKind::close_paren);
    auto operand = cast();
    return low_cast_op(context_, type, operand);
  }
}

auto Parser::multiplicative() -> Expr * {
  auto lhs = cast();
loop:
  switch (next_kind()) {
  case TokenKind::star: {
    consume();
    auto rhs = cast();
    auto [l, r, t] = convert(context_, lhs, rhs);
    lhs = context_->multiply(t, l, r);
    goto loop;
  }
  case TokenKind::slash: {
    consume();
    auto rhs = cast();
    auto [l, r, t] = convert(context_, lhs, rhs);
    lhs = context_->divide(t, l, r);
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
    int64_t size_bytes = context->size_of(arr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, size, rhs);
    return context->add(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::integer, TypeKind::pointer):
    std::swap(lhs, rhs);
    [[fallthrough]];
  case pattern(TypeKind::pointer, TypeKind::integer): {
    auto ptr = cast<PointerType>(lhs->type);
    int64_t size_bytes = context->size_of(ptr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, size, rhs);
    return context->add(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::boolean, TypeKind::boolean):
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::integer, TypeKind::boolean):
  case pattern(TypeKind::integer, TypeKind::integer): {
    auto [l, r, t] = convert(context, lhs, rhs);
    return context->add(t, l, r);
  }
  default:
    context->fatal(loc, "");
  }
}

static auto low_sub_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::boolean, TypeKind::boolean):
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::integer, TypeKind::boolean):
  case pattern(TypeKind::integer, TypeKind::integer): {
    auto [l, r, t] = convert(context, lhs, rhs);
    return context->subtract(t, l, r);
  }
  case pattern(TypeKind::pointer, TypeKind::integer): {
    auto ptr = cast<PointerType>(lhs->type);
    int64_t size_bytes = context->size_of(ptr->base);
    auto size = context->integer(size_bytes);
    auto offset = context->multiply(size->type, rhs, size);
    return context->subtract(lhs->type, lhs, offset);
  }
  case pattern(TypeKind::pointer, TypeKind::pointer): {
    auto ptr = cast<PointerType>(lhs->type);
    int64_t size_bytes = context->size_of(ptr->base);
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
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->less_than(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::less_equal: {
    consume();
    auto rhs = additive();
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->less_equal(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::greater: {
    consume();
    auto rhs = additive();
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->less_than(context_->int32(), r, l);
    goto loop;
  }
  case TokenKind::greater_equal: {
    consume();
    auto rhs = additive();
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->less_equal(context_->int32(), r, l);
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
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->equal(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::exclaim_equal: {
    consume();
    auto rhs = relational();
    auto [l, r, _] = convert(context_, lhs, rhs);
    lhs = context_->not_equal(context_->int32(), l, r);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto low_assign_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::integer, TypeKind::integer): {
    auto src = cast<IntegerType>(rhs);
    auto dest = cast<IntegerType>(lhs);
    if (src->kind != dest->kind) {
      rhs = low_cast_op(context, lhs->type, rhs);
    }
    return context->assign(lhs->type, lhs, rhs);
  }
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::boolean, TypeKind::pointer):
  case pattern(TypeKind::boolean, TypeKind::array):
    rhs = context->cast(lhs->type, rhs);
    return context->assign(lhs->type, lhs, rhs);
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