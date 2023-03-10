#include "lzhcc_parse.h"
#include <cassert>
#include <cctype>

namespace lzhcc {

auto Parser::call(Expr *func, FunctionType *type) -> Expr * {
  auto token = consume(TokenKind::open_paren);
  std::vector<Expr *> args;
  while (!consume_if(TokenKind::close_paren)) {
    if (!args.empty()) {
      consume(TokenKind::comma);
    }
    args.push_back(assignment());
  }
  auto &params = type->params;
  if (args.size() < params.size()) {
    context_->fatal(token->location, "");
  }
  if (args.size() > params.size() && !type->is_variadic) {
    context_->fatal(token->location, "");
  }
  for (int i = 0; i < params.size(); i++) {
    args[i] = context_->cast(params[i], args[i]);
  }
  for (int i = params.size(); i < args.size(); i++) {
    if (args[i]->type->kind == TypeKind::floating) {
      args[i] = context_->cast(context_->float64(), args[i]);
    }
  }
  LValue *ret_buffer = nullptr;
  if (type->ret->kind == TypeKind::record) {
    ret_buffer = create_anon_local(type->ret);
  }
  return context_->call(type->ret, func, std::move(args), params.size(),
                        ret_buffer);
}

auto Parser::primary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::numeric:
    return numeric();
  case TokenKind::character:
    return character();
  case TokenKind::open_paren: {
    auto token = consume();
    if (!next_is(TokenKind::open_brace)) {
      auto expr = expression();
      consume(TokenKind::close_paren);
      return expr;
    } else {
      entry_scope();
      auto stmt = cast<BlockStmt>(block_stmt());
      leave_scope();
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
    auto var = find_var(token->inner);
    if (Value *value = var; value) {
      return context_->value(value);
    } else if (int value; var.get(&value)) {
      return context_->integer(value);
    } else {
      context_->fatal(token->location, "");
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
    return context_->integer(context_->uint64(), size);
  }
  case TokenKind::kw_alignof: {
    consume();
    consume(TokenKind::open_paren);
    auto type = abstract_declarator(declspec());
    consume(TokenKind::close_paren);
    int size = context_->align_of(type);
    return context_->integer(context_->uint64(), size);
  }
  default:
    context_->fatal(position_->location, "");
  }
}

auto Parser::unary() -> Expr * {
  switch (next_kind()) {
  case TokenKind::plus: {
    auto token = consume();
    return convert(context_, cast(), token->location).first;
  }
  case TokenKind::minus: {
    auto token = consume();
    auto [operand, type] = convert(context_, cast(), token->location);
    return context_->negative(type, operand);
  }
  case TokenKind::amp: {
    auto token = consume();
    auto operand = cast();
    if (operand->kind == ExprKind::member) {
      auto mem = cast<MemberExpr>(operand);
      if (mem->member->is_bitfield) {
        context_->fatal(token->location, "cannot take address of bitfield");
      }
    }
    return low_refernce_op(context_, operand);
  }
  case TokenKind::star: {
    auto token = consume();
    auto operand = cast();
    if (operand->type->kind == TypeKind::function) {
      return operand;
    } else {
      return low_deref_op(context_, operand, token->location);
    }
  }
  case TokenKind::exclaim: {
    auto token = consume();
    auto operand = cast();
    return context_->logical_not(context_->int32(), operand);
  }
  case TokenKind::tilde: {
    auto token = consume();
    auto [operand, type] = convert(context_, cast(), token->location);
    return context_->bitwise_not(type, operand);
  }
  case TokenKind::plus_plus: {
    auto token = consume();
    auto lhs = unary();
    auto rhs = context_->integer(1);
    return assign_to(lhs, rhs, low_add_op, token->location);
  }
  case TokenKind::minus_minus: {
    auto token = consume();
    auto lhs = unary();
    auto rhs = context_->integer(1);
    return assign_to(lhs, rhs, low_sub_op, token->location);
  }
  default:
    return postfix();
  }
}

auto Parser::cast() -> Expr * {
  if (!next_is(TokenKind::open_paren) || !is_typename(position_ + 1) ||
      position_[position_->inner + 1].kind == TokenKind::open_brace) {
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
    auto token = consume();
    auto rhs = cast();
    lhs = low_mul_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  case TokenKind::slash: {
    auto token = consume();
    auto rhs = cast();
    lhs = low_div_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  case TokenKind::percent: {
    auto token = consume();
    auto rhs = cast();
    lhs = low_mod_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
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

auto Parser::shift() -> Expr * {
  auto lhs = additive();
loop:
  switch (next_kind()) {
  case TokenKind::less_less: {
    auto token = consume();
    auto rhs = additive();
    lhs = low_shift_left_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  case TokenKind::greater_greater: {
    auto token = consume();
    auto rhs = additive();
    lhs = low_shift_right_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::post_inc(Expr *lhs, Expr *rhs, int loc) -> Expr * {
  // (decltype(lhs)) ((lhs += rhs) - rhs)
  auto add = assign_to(lhs, rhs, low_add_op, loc);
  auto sub = low_sub_op(context_, add, rhs, loc);
  return context_->cast(lhs->type, sub);
}

auto Parser::postfix() -> Expr * {
  if (next_is(TokenKind::open_paren) && is_typename(position_ + 1)) {
    auto token = consume();
    auto type = abstract_declarator(declspec());
    consume(TokenKind::close_paren);
    auto init = this->init(type);
    if (type->kind == TypeKind::array) {
      auto array_type = cast<ArrayType>(type);
      auto array_init = cast<ArrayInit>(init);
      if (array_type->length == -1) {
        int length = array_init->children.size();
        type = context_->array_of(array_type->base, length);
      }
    }
    if (scopes_.size() == 1) {
      std::string buffer;
      buffer.resize(context_->size_of(type));
      auto data = (uint8_t *)&buffer[0];

      std::vector<Relocation> relocations;
      low_global(init, {data, buffer.size()}, relocations, token->location);

      for (auto &rel : relocations) {
        rel.index = reinterpret_cast<uint8_t *>(rel.index) - data;
      }

      auto index = context_->push_literal(std::move(buffer));
      auto view = context_->storage(index);
      auto init_data = (uint8_t *)&view[0];
      auto gvalue = create_anon_global(type, init_data, std::move(relocations));
      return context_->value(gvalue);
    } else {
      auto value = create_anon_local(type);
      auto result = context_->value(value);
      int64_t size_bytes = context_->size_of(value->type);
      auto expr = context_->zero(result, size_bytes);
      if (auto rhs = low_local(result, init, token->location)) {
        expr = context_->comma(rhs->type, expr, rhs);
      }
      return context_->comma(result->type, expr, result);
    }
  }
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
  case TokenKind::open_paren: {
    auto type = lhs->type;
    if (type->kind == TypeKind::function) {
      lhs = call(lhs, cast<FunctionType>(type));
      goto loop;
    } else if (type->kind == TypeKind::pointer) {
      auto pointer = cast<PointerType>(type);
      if (pointer->base->kind == TypeKind::function) {
        lhs = call(lhs, cast<FunctionType>(pointer->base));
        goto loop;
      }
    }
    context_->fatal(position_->location, "");
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
  case TokenKind::plus_plus: {
    auto token = consume();
    lhs = post_inc(lhs, context_->integer(1), token->location);
    goto loop;
  }
  case TokenKind::minus_minus: {
    auto token = consume();
    lhs = post_inc(lhs, context_->integer(-1), token->location);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::relational() -> Expr * {
  auto lhs = shift();
loop:
  switch (next_kind()) {
  case TokenKind::less: {
    auto token = consume();
    auto rhs = shift();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
    lhs = context_->less_than(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::less_equal: {
    auto token = consume();
    auto rhs = shift();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
    lhs = context_->less_equal(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::greater: {
    auto token = consume();
    auto rhs = shift();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
    lhs = context_->less_than(context_->int32(), r, l);
    goto loop;
  }
  case TokenKind::greater_equal: {
    auto token = consume();
    auto rhs = shift();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
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
    auto token = consume();
    auto rhs = relational();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
    lhs = context_->equal(context_->int32(), l, r);
    goto loop;
  }
  case TokenKind::exclaim_equal: {
    auto token = consume();
    auto rhs = relational();
    auto [l, r] = convert_cmp(context_, lhs, rhs, token->location);
    lhs = context_->not_equal(context_->int32(), l, r);
    goto loop;
  }
  default:
    break;
  }
  return lhs;
}

auto Parser::bitwise_and() -> Expr * {
  auto lhs = equality();
  while (auto token = consume_if(TokenKind::amp)) {
    lhs = low_bitwise_and_op(context_, lhs, equality(), token->location);
  }
  return lhs;
}

auto Parser::bitwise_xor() -> Expr * {
  auto lhs = bitwise_and();
  while (auto token = consume_if(TokenKind::caret)) {
    lhs = low_bitwise_xor_op(context_, lhs, bitwise_and(), token->location);
  }
  return lhs;
}

auto Parser::bitwise_or() -> Expr * {
  auto lhs = bitwise_xor();
  while (auto token = consume_if(TokenKind::pipe)) {
    lhs = low_bitwise_or_op(context_, lhs, bitwise_xor(), token->location);
  }
  return lhs;
}

auto Parser::logical_and() -> Expr * {
  auto lhs = bitwise_or();
  while (auto token = consume_if(TokenKind::amp_amp)) {
    auto rhs = bitwise_or();
    auto [l, r, _] = convert(context_, lhs, rhs, token->location);
    lhs = context_->logical_and(context_->int32(), l, r);
  }
  return lhs;
}

auto Parser::logical_or() -> Expr * {
  auto lhs = logical_and();
  while (auto token = consume_if(TokenKind::pipe_pipe)) {
    auto rhs = logical_and();
    auto [l, r, _] = convert(context_, lhs, rhs, token->location);
    lhs = context_->logical_or(context_->int32(), l, r);
  }
  return lhs;
}

auto Parser::condition() -> Expr * {
  auto cond = logical_or();
  auto token = consume_if(TokenKind::question);
  if (!token) {
    return cond;
  }
  auto *then = expression();
  consume(TokenKind::colon);
  auto *else_ = condition();
  if (then->type->kind == TypeKind::kw_void ||
      else_->type->kind == TypeKind::kw_void) {
    return context_->condition(context_->void_type(), cond, then, else_);
  } else {
    auto [lhs, rhs, type] = convert(context_, then, else_, token->location);
    return context_->condition(type, cond, lhs, rhs);
  }
}

auto Parser::assign_to(Expr *lhs, Expr *rhs, LowFn lower, int loc) -> Expr * {
  // Convert `A.x op= C to tmp = &A, (*tmp).x = (*tmp).x op C`
  if (lhs->kind == ExprKind::member) {
    auto mem = cast<MemberExpr>(lhs);
    auto tmp = create_anon_local(context_->pointer_to(mem->record->type));
    auto ref = context_->value(tmp);
    auto expr1 = context_->assign(tmp->type, ref,
                                  context_->refrence(tmp->type, mem->record));
    auto expr2 = context_->member(
        lhs->type, context_->deref(mem->record->type, expr1), mem->member);
    auto expr3 =
        context_->assign(lhs->type, expr2, lower(context_, expr2, rhs, loc));
    return context_->comma(expr3->type,
                           context_->comma(expr2->type, expr1, expr2), expr3);
  }

  // decltype(lhs) * tmp;
  auto tmp = create_anon_local(context_->pointer_to(lhs->type));
  auto ref = context_->value(tmp);

  // tmp = &lhs
  auto expr1 =
      context_->assign(tmp->type, ref, context_->refrence(tmp->type, lhs));

  // *tmp = *tmp op rhs
  auto deref = context_->deref(lhs->type, ref);
  auto expr2 =
      low_assign_op(context_, deref, lower(context_, deref, rhs, loc), loc);

  return context_->comma(expr2->type, expr1, expr2);
}

auto Parser::assignment() -> Expr * {
  auto lhs = condition();
loop:
  switch (next_kind()) {
  case TokenKind::equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = low_assign_op(context_, lhs, rhs, token->location);
    goto loop;
  }
  case TokenKind::plus_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_add_op, token->location);
    goto loop;
  }
  case TokenKind::minus_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_sub_op, token->location);
    goto loop;
  }
  case TokenKind::star_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_mul_op, token->location);
    goto loop;
  }
  case TokenKind::slash_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_div_op, token->location);
    goto loop;
  }
  case TokenKind::percent_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_mod_op, token->location);
    goto loop;
  }
  case TokenKind::amp_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_bitwise_and_op, token->location);
    goto loop;
  }
  case TokenKind::caret_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_bitwise_xor_op, token->location);
    goto loop;
  }
  case TokenKind::pipe_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_bitwise_or_op, token->location);
    goto loop;
  }
  case TokenKind::less_less_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_shift_left_op, token->location);
    goto loop;
  }
  case TokenKind::greater_greater_equal: {
    auto token = consume();
    auto rhs = assignment();
    lhs = assign_to(lhs, rhs, low_shift_right_op, token->location);
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

static constexpr auto pattern(auto lhs, auto rhs) -> int {
  return static_cast<int>(lhs) * 65536 | static_cast<int>(rhs);
}

auto low_refernce_op(Context *context, Expr *operand) -> Expr * {
  if (operand->type->kind == TypeKind::array) {
    auto arr = cast<ArrayType>(operand->type);
    auto type = context->pointer_to(arr->base);
    return context->refrence(type, operand);
  } else {
    auto type = context->pointer_to(operand->type);
    return context->refrence(type, operand);
  }
}

auto low_deref_op(Context *context, Expr *operand, int loc) -> Expr * {
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
  case TypeKind::floating:
  case TypeKind::kw_void:
    context->fatal(loc, "");
  }
}

auto low_cast_op(Context *context, Type *type, Expr *operand) -> Expr * {
  return context->cast(type, operand);
}

auto convert(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> std::tuple<Expr *, Expr *, Type *> {
  auto cast_expr = [&](Expr *expr) {
    if (expr->type->kind == TypeKind::boolean) {
      return context->cast(context->int32(), lhs);
    } else if (expr->type->kind == TypeKind::function) {
      auto type = context->pointer_to(expr->type);
      return context->cast(type, expr);
    } else if (expr->type->kind == TypeKind::array) {
      auto array = cast<ArrayType>(expr->type);
      auto type = context->pointer_to(array->base);
      return context->cast(type, expr);
    } else {
      return expr;
    }
  };
  lhs = cast_expr(lhs);
  rhs = cast_expr(rhs);
  auto floating = [&] {
    auto lhs_type = cast<FloatingType>(lhs->type);
    auto rhs_type = cast<FloatingType>(rhs->type);
    switch (pattern(lhs_type->kind, rhs_type->kind)) {
    case pattern(FloatingKind::float32, FloatingKind::float32):
    case pattern(FloatingKind::float64, FloatingKind::float64):
      break;
    case pattern(FloatingKind::float32, FloatingKind::float64):
      lhs = low_cast_op(context, rhs->type, lhs);
      break;
    case pattern(FloatingKind::float64, FloatingKind::float32):
      rhs = low_cast_op(context, lhs->type, lhs);
      break;
    default:
      context->fatal(loc, "");
    }
  };
  auto integer = [&] {
    auto lhs_type = cast<IntegerType>(lhs->type);
    auto rhs_type = cast<IntegerType>(rhs->type);
    using enum IntegerKind;

    if (lhs_type->kind < IntegerKind::word) {
      lhs_type = cast<IntegerType>(context->int32());
    }

    if (rhs_type->kind < IntegerKind::word) {
      rhs_type = cast<IntegerType>(context->int32());
    }

    Type *type = nullptr;
    if (lhs_type->kind != rhs_type->kind) {
      type = lhs_type->kind < rhs_type->kind ? rhs_type : lhs_type;
    } else if (rhs_type->sign == Sign::unsign) {
      type = rhs_type;
    } else {
      type = lhs_type;
    }

    lhs = low_cast_op(context, type, lhs);
    rhs = low_cast_op(context, type, rhs);
  };
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::floating, TypeKind::floating):
    floating();
    break;
  case pattern(TypeKind::integer, TypeKind::integer):
    integer();
    break;
  case pattern(TypeKind::integer, TypeKind::floating):
    lhs = low_cast_op(context, rhs->type, lhs);
    break;
  case pattern(TypeKind::floating, TypeKind::integer):
    rhs = low_cast_op(context, lhs->type, rhs);
    break;
  case pattern(TypeKind::pointer, TypeKind::pointer):
    break;
  default:
    context->fatal(loc, "");
  }
  return std::tuple(lhs, rhs, lhs->type);
}

auto convert_cmp(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> std::pair<Expr *, Expr *> {
  auto is_pointer_like = [](Type *type) {
    switch (type->kind) {
    case TypeKind::kw_void:
    case TypeKind::boolean:
    case TypeKind::record:
    case TypeKind::integer:
    case TypeKind::floating:
      return false;
    case TypeKind::pointer:
    case TypeKind::function:
    case TypeKind::array:
      return true;
    }
  };
  if (is_pointer_like(lhs->type) && is_pointer_like(rhs->type)) {
    return std::pair(lhs, rhs);
  } else {
    auto [l, r, _] = convert(context, lhs, rhs, loc);
    return std::pair(l, r);
  }
}

auto convert(Context *context, Expr *operand, int loc)
    -> std::pair<Expr *, Type *> {
  auto integer = [&](IntegerType *type) {
    using enum IntegerKind;
    if (type->kind < IntegerKind::word) {
      operand = low_cast_op(context, context->int32(), operand);
    }
  };
  switch (operand->type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
    operand = low_cast_op(context, context->int32(), operand);
    break;
  case TypeKind::integer:
    integer(cast<IntegerType>(operand->type));
    break;
  case TypeKind::floating:
    break;
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    context->fatal(loc, "");
  }
  return std::pair(operand, operand->type);
}

auto low_mul_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  auto [l, r, t] = convert(context, lhs, rhs, loc);
  return context->multiply(t, l, r);
}

auto low_div_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  auto [l, r, t] = convert(context, lhs, rhs, loc);
  return context->divide(t, l, r);
}

auto low_mod_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  auto [l, r, t] = convert(context, lhs, rhs, loc);
  return context->modulo(t, l, r);
}

auto low_add_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
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
  default:
    auto [l, r, t] = convert(context, lhs, rhs, loc);
    return context->add(t, l, r);
  }
}

auto low_sub_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::array, TypeKind::integer):
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
    auto [l, r, t] = convert(context, lhs, rhs, loc);
    return context->subtract(t, l, r);
  }
}

auto low_shift_left_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  if (lhs->type->kind != TypeKind::integer ||
      rhs->type->kind != TypeKind::integer) {
    context->fatal(loc, "");
  }
  auto lhs_type = cast<IntegerType>(lhs->type);
  if (lhs_type->kind < IntegerKind::word) {
    if (lhs_type->sign == Sign::sign) {
      lhs = context->cast(context->int32(), lhs);
    } else {
      lhs = context->cast(context->uint32(), lhs);
    }
  }
  return context->shift_left(lhs->type, lhs, rhs);
}

auto low_shift_right_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  if (lhs->type->kind != TypeKind::integer ||
      rhs->type->kind != TypeKind::integer) {
    context->fatal(loc, "");
  }
  auto lhs_type = cast<IntegerType>(lhs->type);
  if (lhs_type->kind < IntegerKind::word) {
    if (lhs_type->sign == Sign::sign) {
      lhs = context->cast(context->int32(), lhs);
    } else {
      lhs = context->cast(context->uint32(), lhs);
    }
  }
  return context->shift_right(lhs->type, lhs, rhs);
}

auto low_assign_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::integer, TypeKind::integer):
  case pattern(TypeKind::floating, TypeKind::floating):
  case pattern(TypeKind::integer, TypeKind::floating):
  case pattern(TypeKind::floating, TypeKind::integer):
  case pattern(TypeKind::boolean, TypeKind::floating):
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::boolean, TypeKind::pointer):
  case pattern(TypeKind::boolean, TypeKind::array):
    rhs = context->cast(lhs->type, rhs);
    return context->assign(lhs->type, lhs, rhs);
  case pattern(TypeKind::pointer, TypeKind::pointer):
  case pattern(TypeKind::pointer, TypeKind::array):
  case pattern(TypeKind::pointer, TypeKind::function):
  case pattern(TypeKind::record, TypeKind::record):
    return context->assign(lhs->type, lhs, rhs);
  default:
    context->fatal(loc, "");
  }
}

auto low_bitwise_and_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  auto [l, r, type] = convert(context, lhs, rhs, loc);
  return context->bitwise_and(type, l, r);
}

auto low_bitwise_xor_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  auto [l, r, type] = convert(context, lhs, rhs, loc);
  return context->bitwise_xor(type, l, r);
}

auto low_bitwise_or_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  auto [l, r, type] = convert(context, lhs, rhs, loc);
  return context->bitwise_or(type, l, r);
}

auto low_member_op(Context *context, Expr *lhs, int rhs, int loc) -> Expr * {
  if (lhs->type->kind != TypeKind::record) {
    context->fatal(loc, "");
  }

  std::vector<Member *> stack;
  std::function<bool(RecordType *)> dfs = [&](RecordType *record) -> bool {
    for (auto &mem : record->members) {
      stack.push_back(&mem);
      auto child = cast<RecordType>(mem.type);
      if (mem.name == rhs || (mem.name < 0 && dfs(child))) {
        return true;
      } else {
        stack.pop_back();
      }
    }
    return false;
  };
  if (!dfs(cast<RecordType>(lhs->type))) {
    context->fatal(loc, "");
  }
  Expr *result = lhs;
  for (auto &mem : stack) {
    result = context->member(mem->type, result, mem);
  }
  return result;
}

} // namespace lzhcc