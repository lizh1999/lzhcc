#include "lzhcc_parse.h"
#include <cassert>
#include <cctype>

namespace lzhcc {

static auto low_refernce_op(Context *, Expr *) -> Expr *;
static auto low_deref_op(Context *, Expr *, int) -> Expr *;
static auto low_cast_op(Context *, Type *, Expr *) -> Expr *;
static auto convert(Context *, Expr *, Expr *, int)
    -> std::tuple<Expr *, Expr *, Type *>;
static auto convert_cmp(Context *, Expr *, Expr *, int)
    -> std::pair<Expr *, Expr *>;
static auto convert(Context *, Expr *) -> std::pair<Expr *, Type *>;
static auto low_mul_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_div_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_mod_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_add_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_sub_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_shift_left_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_shift_right_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_bitwise_and_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_bitwise_xor_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_bitwise_or_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_assign_op(Context *, Expr *, Expr *, int) -> Expr *;
static auto low_member_op(Context *, Expr *, int, int) -> Expr *;

auto Parser::scalar_init() -> Init * {
  auto open = consume_if(TokenKind::open_brace);
  auto expr = assignment();
  if (open) {
    consume(TokenKind::close_brace);
  }
  return context_->scalar_init(expr);
}

auto Parser::array_init(ArrayType *array) -> Init * {
  std::vector<Init *> children;
  switch (next_kind()) {
  case TokenKind::string: {
    auto base = array->base;
    if (base->kind != TypeKind::integer ||
        cast<IntegerType>(base)->kind != IntegerKind::byte) {
      context_->fatal(position_->location, "");
    }
    auto str = cook_string();
    int n = str.size();
    if (array->length != -1 && array->length < n) {
      n = array->length;
    }
    for (int i = 0; i < n; i++) {
      auto expr = context_->integer(str[i]);
      auto init = context_->scalar_init(expr);
      children.push_back(init);
    }
    return context_->array_init(std::move(children), base);
  }
  case TokenKind::open_brace: {
    consume();
    int length = array->length == -1 ? INT_MAX : array->length;
    for (int i = 0; !consume_if(TokenKind::close_brace); i++) {
      auto init = this->init(array->base);
      if (children.size() < length) {
        children.push_back(init);
      }
      if (!consume_if(TokenKind::comma)) {
        consume(TokenKind::close_brace);
        break;
      }
    }
    return context_->array_init(std::move(children), array->base);
  }
  default:
    return array_init2(array);
  }
}

auto Parser::record_init(RecordType *record) -> Init * {
  if (next_is(TokenKind::open_brace)) {
    RecordInit::Children children;
    consume(TokenKind::open_brace);
    auto &members = record->members;
    int member_size = 0;
    if (!members.empty()) {
      member_size = members.back().offset ? members.size() : 1;
    }
    for (int i = 0; !consume_if(TokenKind::close_brace); i++) {
      if (i == member_size) {
        context_->fatal(position_->location, "");
      }
      auto init = this->init(members[i].type);
      children.emplace_back(&members[i], init);
      if (!consume_if(TokenKind::comma)) {
        consume(TokenKind::close_brace);
        break;
      }
    }
    return context_->record_init(std::move(children));
  } else {
    auto origin = position_;
    auto expr = assignment();
    if (expr->type == record) {
      return context_->scalar_init(expr);
    }
    position_ = origin;
    return record_init2(record);
  }
}

auto Parser::init(Type *type) -> Init * {
  switch (type->kind) {
  case TypeKind::array:
    return array_init(cast<ArrayType>(type));
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::pointer:
  case TypeKind::function:
    return scalar_init();
  case TypeKind::record:
    return record_init(cast<RecordType>(type));
  case TypeKind::kw_void:
    assert(false);
  }
}

auto Parser::array_init2(ArrayType *array) -> Init * {
  std::vector<Init *> children;
  int length = array->length == -1 ? INT_MAX : array->length;
  for (int i = 0; i < length; i++) {
    if (next_is(TokenKind::close_brace)) {
      break;
    }
    if (i != 0 && !consume_if(TokenKind::comma)) {
      break;
    }
    auto init = this->init2(array->base);
    children.push_back(init);
  }
  return context_->array_init(children, array->base);
}

auto Parser::record_init2(RecordType *record) -> Init * {
  RecordInit::Children children;
  auto &members = record->members;
  int member_size = 0;
  if (!members.empty()) {
    member_size = member_size = members.back().offset ? members.size() : 1;
  }
  for (int i = 0; i < member_size; i++) {
    if (next_is(TokenKind::close_brace)) {
      break;
    }
    if (i != 0 && !consume_if(TokenKind::comma)) {
      break;
    }
    auto init = this->init2(members[i].type);
    children.emplace_back(&members[i], init);
  }
  return context_->record_init(std::move(children));
}

auto Parser::init2(Type *type) -> Init * {
  switch (type->kind) {
  case TypeKind::array:
    return array_init2(cast<ArrayType>(type));
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::pointer:
  case TypeKind::function:
    return scalar_init();
  case TypeKind::record:
    return record_init2(cast<RecordType>(type));
  case TypeKind::kw_void:
    assert(false);
  }
}

auto Parser::init_local_scalar(Expr *expr, ScalarInit *init, int loc)
    -> Expr * {
  return low_assign_op(context_, expr, init->expr, loc);
}

auto Parser::init_local_array(Expr *expr, ArrayInit *init, int loc) -> Expr * {
  Expr *result = nullptr;
  auto &children = init->children;
  for (int64_t i = 0; i < children.size(); i++) {
    auto index = context_->integer(i);
    auto base =
        low_deref_op(context_, low_add_op(context_, expr, index, loc), loc);
    auto rhs = this->init_local(base, children[i], loc);
    if (!result) {
      result = rhs;
    } else if (rhs) {
      result = context_->comma(rhs->type, result, rhs);
    }
  }
  return result;
}

auto Parser::init_local_record(Expr *expr, RecordInit *init, int loc)
    -> Expr * {
  Expr *result = nullptr;
  auto &children = init->children;
  for (auto [member, init] : init->children) {
    auto base = context_->member(member->type, expr, member->offset);
    auto rhs = this->init_local(base, init, loc);
    if (!result) {
      result = rhs;
    } else if (rhs) {
      result = context_->comma(rhs->type, result, rhs);
    }
  }
  return result;
}

auto Parser::init_local(Expr *expr, Init *init, int loc) -> Expr * {
  switch (init->kind) {
  case InitKind::array:
    return init_local_array(expr, cast<ArrayInit>(init), loc);
  case InitKind::scalar:
    return init_local_scalar(expr, cast<ScalarInit>(init), loc);
  case InitKind::record:
    return init_local_record(expr, cast<RecordInit>(init), loc);
  }
}

auto Parser::init_global_scalar(ScalarInit *init, std::span<uint8_t> out,
                                std::vector<Relocation> &relocations, int loc)
    -> void {
  int64_t value;
  std::string_view *label = nullptr;
  if (!const_int(init->expr, &value, &label)) {
    context_->fatal(loc, "");
  }
  if (label) {
    assert(out.size() == 8);
    int64_t index = reinterpret_cast<int64_t>(&out[0]);
    relocations.push_back({index, *label, value});
    return;
  }
  auto write = [&](auto value) mutable {
    using T = decltype(value);
    *reinterpret_cast<T *>(&out[0]) = value;
  };
  switch (out.size()) {
  case 1:
    return write(static_cast<uint8_t>(value));
  case 2:
    return write(static_cast<uint16_t>(value));
  case 4:
    return write(static_cast<uint32_t>(value));
  case 8:
    return write(static_cast<uint64_t>(value));
  default:
    assert(false);
  }
}

auto Parser::init_global_record(RecordInit *init, std::span<uint8_t> out,
                                std::vector<Relocation> &relocations, int loc)
    -> void {
  auto &children = init->children;
  for (auto [member, init] : init->children) {
    int start = member->offset;
    int count = context_->size_of(member->type);
    init_global(init, out.subspan(start, count), relocations, loc);
  }
}

auto Parser::init_global_array(ArrayInit *init, std::span<uint8_t> out,
                               std::vector<Relocation> &relocations, int loc)
    -> void {
  auto &children = init->children;
  int size = context_->size_of(init->base);
  for (int i = 0; i < children.size(); i++) {
    init_global(children[i], out.subspan(i * size, size), relocations, loc);
  }
}

auto Parser::init_global(Init *init, std::span<uint8_t> out,
                         std::vector<Relocation> &relocations, int loc)
    -> void {
  switch (init->kind) {
  case InitKind::array:
    return init_global_array(cast<ArrayInit>(init), out, relocations, loc);
  case InitKind::scalar:
    return init_global_scalar(cast<ScalarInit>(init), out, relocations, loc);
  case InitKind::record:
    return init_global_record(cast<RecordInit>(init), out, relocations, loc);
  }
}

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
  auto &params = function_type->params;
  if (args.size() < params.size()) {
    context_->fatal(token->location, "");
  }
  if (args.size() > params.size() && !function_type->is_variadic) {
    context_->fatal(token->location, "");
  }
  for (int i = 0; i < params.size(); i++) {
    args[i] = context_->cast(params[i], args[i]);
  }
  return context_->call(name, function_type->ret, std::move(args));
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
    return convert(context_, cast()).first;
  }
  case TokenKind::minus: {
    auto token = consume();
    auto [operand, type] = convert(context_, cast());
    return context_->negative(type, operand);
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
  case TokenKind::exclaim: {
    auto token = consume();
    auto operand = cast();
    return context_->logical_not(context_->int32(), operand);
  }
  case TokenKind::tilde: {
    auto token = consume();
    auto [operand, type] = convert(context_, cast());
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
      init_global(init, {data, buffer.size()}, relocations, token->location);

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
      if (auto rhs = init_local(result, init, token->location)) {
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

static constexpr auto pattern(TypeKind lhs, TypeKind rhs) -> int {
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
  case TypeKind::kw_void:
    context->fatal(loc, "");
  }
}

auto low_cast_op(Context *context, Type *type, Expr *operand) -> Expr * {
  return context->cast(type, operand);
}

auto convert(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> std::tuple<Expr *, Expr *, Type *> {
  if (lhs->type->kind == TypeKind::boolean) {
    lhs = context->cast(context->int32(), lhs);
  }
  if (rhs->type->kind == TypeKind::boolean) {
    rhs = context->cast(context->int32(), rhs);
  }
  if (lhs->type->kind != TypeKind::integer ||
      rhs->type->kind != TypeKind::integer) {
    context->fatal(loc, "");
  }
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
  return std::tuple(lhs, rhs, type);
}

auto convert_cmp(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> std::pair<Expr *, Expr *> {
  auto is_pointer_like = [](Type *type) {
    switch (type->kind) {
    case TypeKind::kw_void:
    case TypeKind::boolean:
    case TypeKind::record:
    case TypeKind::integer:
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

auto convert(Context *context, Expr *operand) -> std::pair<Expr *, Type *> {
  assert(operand->type->kind == TypeKind::integer);
  auto operand_type = cast<IntegerType>(operand->type);
  using enum IntegerKind;
  if (operand_type->kind < IntegerKind::word) {
    operand = low_cast_op(context, context->int32(), operand);
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
  case pattern(TypeKind::boolean, TypeKind::boolean):
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::integer, TypeKind::boolean):
  case pattern(TypeKind::integer, TypeKind::integer): {
    auto [l, r, t] = convert(context, lhs, rhs, loc);
    return context->add(t, l, r);
  }
  default:
    context->fatal(loc, "");
  }
}

auto low_sub_op(Context *context, Expr *lhs, Expr *rhs, int loc) -> Expr * {
  switch (pattern(lhs->type->kind, rhs->type->kind)) {
  case pattern(TypeKind::boolean, TypeKind::boolean):
  case pattern(TypeKind::boolean, TypeKind::integer):
  case pattern(TypeKind::integer, TypeKind::boolean):
  case pattern(TypeKind::integer, TypeKind::integer): {
    auto [l, r, t] = convert(context, lhs, rhs, loc);
    return context->subtract(t, l, r);
  }
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
    context->fatal(loc, "");
  }
}

auto low_shift_left_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  return context->shift_left(lhs->type, lhs, rhs);
}

auto low_shift_right_op(Context *context, Expr *lhs, Expr *rhs, int loc)
    -> Expr * {
  return context->shift_right(lhs->type, lhs, rhs);
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
  auto record = cast<RecordType>(lhs->type);
  size_t i = 0;
  for (; i < record->members.size(); i++) {
    if (record->members[i].name == rhs) {
      break;
    }
  }
  if (i == record->members.size()) {
    context->fatal(loc, "");
  }
  auto &member = record->members[i];
  return context->member(member.type, lhs, member.offset);
}

} // namespace lzhcc