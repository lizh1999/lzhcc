#include "lzhcc_codegen.h"
#include <cassert>

namespace lzhcc {

auto Generator::value_addr(ValueExpr *expr) -> void {
  switch (expr->value->kind) {
  case ValueKind::local: {
    auto lvalue = cast<LValue>(expr->value);
    println("  add a0, fp, %d", lvalue->offset);
    break;
  }
  case ValueKind::global: {
    auto gvalue = cast<GValue>(expr->value);
    auto name = gvalue->name;
    println("  la a0, %.*s", (int)name.size(), name.data());
    break;
  }
  case ValueKind::function: {
    auto function = cast<Function>(expr->value);
    auto name = function->name;
    println("  la a0, %.*s", (int)name.size(), name.data());
    break;
  }
  case ValueKind::declaraion: {
    auto declaration = cast<Declaration>(expr->value);
    auto name = declaration->name;
    println("  la a0, %.*s", (int)name.size(), name.data());
    break;
  }
  }
}

auto Generator::unary_addr(UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::deref:
    return expr_proxy(expr->operand);
  default:
    expect_lvalue();
  }
}

auto Generator::binary_addr(BinaryExpr *expr) -> void {
  switch (expr->kind) {
  case BinaryKind::comma:
    expr_proxy(expr->lhs);
    return addr_proxy(expr->rhs);
  default:
    expect_lvalue();
  }
}

auto Generator::member_addr(MemberExpr *expr) -> void {
  addr_proxy(expr->record);
  println("  addi a0, a0, %d", expr->offset);
}

auto Generator::addr_proxy(Expr *expr) -> void {
  switch (expr->kind) {
  case ExperKind::value:
    return value_addr(cast<ValueExpr>(expr));
  case ExperKind::unary:
    return unary_addr(cast<UnaryExpr>(expr));
  case ExperKind::binary:
    return binary_addr(cast<BinaryExpr>(expr));
  case ExperKind::member:
    return member_addr(cast<MemberExpr>(expr));
  default:
    expect_lvalue();
  }
}

auto Generator::load_integer(IntegerType *type) -> void {
  const char *suffix = type->sign == Sign::sign ? "" : "u";
  switch (type->kind) {
  case IntegerKind::byte:
    return println("  lb%s a0, 0(a0)", suffix);
  case IntegerKind::half:
    return println("  lh%s a0, 0(a0)", suffix);
  case IntegerKind::word:
    return println("  lw%s a0, 0(a0)", suffix);
  case IntegerKind::dword:
    return println("  ld%s a0, 0(a0)", suffix);
  }
}

auto Generator::load(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::boolean:
    return println("  lb a0, 0(a0)");
  case TypeKind::integer: {
    auto integer = cast<IntegerType>(type);
    return load_integer(integer);
  }
  case TypeKind::pointer:
    println("  ld a0, 0(a0)");
    break;
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    break;
  case TypeKind::kw_void:
    std::abort();
  }
}

auto Generator::zero_expr(ZeroExpr *expr) -> void {
  addr_proxy(expr->expr);
  println("  li a1, 0");
  println("  li a2, %ld", expr->size);
  println("  call memset");
}

auto Generator::value_expr(ValueExpr *expr) -> void {
  value_addr(expr);
  load(expr->type);
}

auto Generator::integer_expr(IntegerExpr *expr) -> void {
  println("  li a0, %ld", expr->value);
}

enum {
  i8,
  i16,
  i32,
  i64,
  u8,
  u16,
  u32,
  u64,
};

static const char i64i8[] = "  slliw a0, a0, 24\n"
                            "  sraiw a0, a0, 24";
static const char i64i16[] = "  slliw a0, a0, 16\n"
                             "  sraiw a0, a0, 16";
static const char i64i32[] = "  sext.w  a0, a0";

static const char i64u8[] = "  andi a0, a0, 255";
static const char i64u16[] = "  slli a0, a0, 48\n"
                             "  srli a0, a0, 48";
static const char i64u32[] = "  slli a0, a0, 32\n"
                             "  srli a0, a0, 32";

// clang-format off
static const char *cast_table[][8] = {
    {nullptr, nullptr,  nullptr,  nullptr,  i64u8,  nullptr,  nullptr,  nullptr}, // i8
    {i64i8,   nullptr,  nullptr,  nullptr,  i64u8,  i64u16,   nullptr,  nullptr}, // i16
    {i64i8,   i64i16,   nullptr,  nullptr,  i64u8,  i64u16,   i64u32,   nullptr}, // i32
    {i64i8,   i64i16,   i64i32,   nullptr,  i64u8,  i64u16,   i64u32,   nullptr}, // i64
    {i64i8,   nullptr,  nullptr,  nullptr,  nullptr,nullptr,  nullptr,  nullptr}, // u8
    {i64i8,   i64i16,   nullptr,  nullptr,  i64u8,  nullptr,  nullptr,  nullptr}, // u16
    {i64i8,   i64i16,   i64i32,   nullptr,  i64u8,  i64u16,   nullptr,  nullptr}, // u32
    {i64i8,   i64i16,   i64i32,   nullptr,  i64u8,  i64u16,   i64u32,   nullptr}, // u64
};
// clang-format on

static auto type_id(IntegerType *type) -> int {
  switch (type->kind) {
  case IntegerKind::byte:
    return type->sign == Sign::sign ? i8 : u8;
  case IntegerKind::half:
    return type->sign == Sign::sign ? i16 : u16;
  case IntegerKind::word:
    return type->sign == Sign::sign ? i32 : u32;
  case IntegerKind::dword:
    return type->sign == Sign::sign ? i64 : u64;
  }
}

static auto type_id(Type *type) -> int {
  switch (type->kind) {
  case TypeKind::kw_void:
    return 0;
  case TypeKind::integer:
    return type_id(cast<IntegerType>(type));
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
    return 3;
  case TypeKind::boolean:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::cast(Type *src, Type *dest) -> void {
  if (src->kind == TypeKind::boolean) {
    return;
  }
  if (dest->kind == TypeKind::boolean) {
    return println("snez a0, a0");
  }
  int lhs = type_id(src);
  int rhs = type_id(dest);
  if (cast_table[lhs][rhs]) {
    println("%s", cast_table[lhs][rhs]);
  }
}

auto Generator::unary_expr(UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::negative:
    expr_proxy(expr->operand);
    return println("  neg a0, a0");
  case UnaryKind::deref:
    expr_proxy(expr->operand);
    return load(expr->type);
  case UnaryKind::refrence:
    return addr_proxy(expr->operand);
  case UnaryKind::cast:
    expr_proxy(expr->operand);
    return cast(expr->operand->type, expr->type);
  case UnaryKind::logical_not:
    expr_proxy(expr->operand);
    return println("  seqz a0, a0");
  case UnaryKind::bitwise_not:
    expr_proxy(expr->operand);
    return println("  not a0, a0");
  }
}

auto Generator::store_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
    return println("  sb a1, 0(a0)");
  case IntegerKind::half:
    return println("  sh a1, 0(a0)");
  case IntegerKind::word:
    return println("  sw a1, 0(a0)");
  case IntegerKind::dword:
    return println("  sd a1, 0(a0)");
  }
}

auto Generator::store_record(RecordType *type) -> void {
  int size = context_->size_of(type);
  for (int i = 0; i < size; i++) {
    println("  lb  t0, %d(a1)", i);
    println("  sb  t0, %d(a0)", i);
  }
}

auto Generator::store(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::boolean:
    return println("  sb a1, 0(a0)");
  case TypeKind::integer: {
    auto integer = cast<IntegerType>(type);
    return store_integer(integer);
  }
  case TypeKind::pointer:
    println("  sd a1, 0(a0)");
    break;
  case TypeKind::record:
    return store_record(cast<RecordType>(type));
  case TypeKind::array:
  case TypeKind::function:
  case TypeKind::kw_void:
    std::abort();
  }
}

auto Generator::add(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return add_integer(cast<IntegerType>(type));
  case TypeKind::array:
  case TypeKind::pointer:
    return println("  add a0, a0, a1");
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::function:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::add_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
  case IntegerKind::half:
  case IntegerKind::word:
    println("  addw a0, a0, a1");
    if (type->sign == Sign::unsign) {
      println("  slli a0, a0, 32");
      println("  srli a0, a0, 32");
    }
    break;
  case IntegerKind::dword:
    return println("  add a0, a0, a1");
  }
}

auto Generator::subtract(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return subtract_integer(cast<IntegerType>(type));
  case TypeKind::array:
  case TypeKind::pointer:
    return println("  sub a0, a0, a1");
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::function:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::subtract_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
  case IntegerKind::half:
  case IntegerKind::word:
    println("  subw a0, a0, a1");
    if (type->sign == Sign::unsign) {
      println("  slli a0, a0, 32");
      println("  srli a0, a0, 32");
    }
    break;
  case IntegerKind::dword:
    return println("  sub a0, a0, a1");
  }
}

auto Generator::multiply(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  auto integer = cast<IntegerType>(type);
  switch (integer->kind) {
  case IntegerKind::word:
    println("  mulw a0, a0, a1");
    if (integer->sign == Sign::unsign) {
      println("  slli a0, a0, 32");
      println("  srli a0, a0, 32");
    }
    break;
  case IntegerKind::dword:
    return println("  mul a0, a0, a1");
  default:
    assert(false);
  }
}

auto Generator::divide(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  auto integer = cast<IntegerType>(type);
  switch (integer->kind) {
  case IntegerKind::word:
    switch (integer->sign) {
    case Sign::sign:
      return println("  divw a0, a0, a1");
    case Sign::unsign:
      return println("  divuw a0, a0, a1");
    }
  case IntegerKind::dword:
    switch (integer->sign) {
    case Sign::sign:
      return println("  div a0, a0, a1");
    case Sign::unsign:
      return println("  divu a0, a0, a1");
    }
  default:
    assert(false);
  }
}

auto Generator::modulo(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  auto integer = cast<IntegerType>(type);
  switch (integer->kind) {
  case IntegerKind::word:
    switch (integer->sign) {
    case Sign::sign:
      return println("  remw a0, a0, a1");
    case Sign::unsign:
      return println("  remuw a0, a0, a1");
    }
  case IntegerKind::dword:
    switch (integer->sign) {
    case Sign::sign:
      return println("  rem a0, a0, a1");
    case Sign::unsign:
      return println("  remu a0, a0, a1");
    }
  default:
    assert(false);
  }
}

auto Generator::shift_left(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  switch (cast<IntegerType>(type)->kind) {
  case IntegerKind::word:
    return println("  sllw a0, a0, a1");
  case IntegerKind::dword:
    return println("  sll a0, a0, a1");
  default:
    assert(false);
  }
}

auto Generator::shift_right(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  auto integer = cast<IntegerType>(type);
  char c = integer->sign == Sign::sign ? 'a' : 'l';
  switch (integer->kind) {
  case IntegerKind::word:
    return println("  sr%cw a0, a0, a1", c);
  case IntegerKind::dword:
    return println("  sr%c a0, a0, a1", c);
  default:
    assert(false);
  }
}

auto Generator::less_than(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::record:
    assert(false);
  case TypeKind::integer:
    switch (cast<IntegerType>(type)->sign) {
    case Sign::sign:
      return println("  slt a0, a0, a1");
    case Sign::unsign:
      return println("  sltu a0, a0, a1");
    }
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
    return println("  sltu a0, a0, a1");
  }
}

auto Generator::less_equal(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::record:
    assert(false);
  case TypeKind::integer:
    switch (cast<IntegerType>(type)->sign) {
    case Sign::sign:
      println("   slt a0, a1, a0");
      break;
    case Sign::unsign:
      println("  sltu a0, a1, a0");
      break;
    }
    break;
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
    println("  sltu a0, a1, a0");
    break;
  }
  println("  xori a0, a0, 1");
}


auto Generator::binary_expr(BinaryExpr *expr) -> void {
#define rvalue()                                                               \
  expr_proxy(expr->rhs);                                                       \
  push("a0");                                                                  \
  expr_proxy(expr->lhs);                                                       \
  pop("a1")

  switch (expr->kind) {
  case BinaryKind::add:
    rvalue();
    return add(expr->type);
  case BinaryKind::subtract:
    rvalue();
    return subtract(expr->type);
  case BinaryKind::multiply:
    rvalue();
    return multiply(expr->type);
  case BinaryKind::divide:
    rvalue();
    return divide(expr->type);
  case BinaryKind::modulo:
    rvalue();
    return modulo(expr->type);
  case BinaryKind::bitwise_and:
    rvalue();
    return println("  and a0, a0, a1");
  case BinaryKind::bitwise_xor:
    rvalue();
    return println("  xor a0, a0, a1");
  case BinaryKind::bitwise_or:
    rvalue();
    return println("  or a0, a0, a1");
  case BinaryKind::less_than:
    rvalue();
    return less_than(expr->lhs->type);
  case BinaryKind::less_equal:
    rvalue();
    return less_equal(expr->lhs->type);
  case BinaryKind::equal:
    rvalue();
    println("  xor a0, a0, a1");
    return println("  seqz a0, a0");
  case BinaryKind::not_equal:
    rvalue();
    println("  xor a0, a0, a1");
    return println("  snez a0, a0");
  case BinaryKind::shift_left:
    rvalue();
    return shift_left(expr->type);
  case BinaryKind::shift_right:
    rvalue();
    return shift_right(expr->type);
  case BinaryKind::assign:
    expr_proxy(expr->rhs);
    push("a0");
    addr_proxy(expr->lhs);
    pop("a1");
    store(expr->type);
    return println("  mv a0, a1");
  case BinaryKind::comma:
    expr_proxy(expr->lhs);
    return expr_proxy(expr->rhs);
  case BinaryKind::logical_and: {
    int label = counter_++;
    expr_proxy(expr->lhs);
    println("  beq a0, zero, .L.and.%d", label);
    expr_proxy(expr->rhs);
    println(".L.and.%d:", label);
    return println("  snez a0, a0");
  }
  case BinaryKind::logical_or: {
    int label = counter_++;
    expr_proxy(expr->lhs);
    println("  bne a0, zero, .L.or.%d", label);
    expr_proxy(expr->rhs);
    println(".L.or.%d:", label);
    return println("  snez a0, a0");
  }
  }
}

auto Generator::call_expr(CallExpr *expr) -> void {
  for (auto arg : expr->args) {
    expr_proxy(arg);
    push("a0");
  }
  char reg[] = "a0";
  for (int i = expr->args.size(); i--;) {
    reg[1] += i;
    pop(reg);
    reg[1] -= i;
  }
  if (depth_ % 2 != 0) {
    println("  addi sp, sp, -8");
    println("  call %.*s", (int)expr->name.size(), expr->name.data());
    println("  addi sp, sp, 8");
  } else {
    println("  call %.*s", (int)expr->name.size(), expr->name.data());
  }
}

auto Generator::stmt_expr(StmtExpr *expr) -> void {
  return block_stmt(expr->stmt);
}

auto Generator::member_expr(MemberExpr *expr) -> void {
  addr_proxy(expr);
  load(expr->type);
}

auto Generator::condition_expr(ConditionExpr *expr) -> void {
  expr_proxy(expr->cond);
  int label = counter_++;
  println("  beqz a0, .L.else.%d", label);
  expr_proxy(expr->then);
  println("  j .L.end.%d", label);
  println(".L.else.%d:", label);
  expr_proxy(expr->else_);
  println(".L.end.%d:", label);
}

auto Generator::push(const char *reg) -> void {
  depth_++;
  println("  addi sp, sp, -8");
  println("  sd %s, 0(sp)", reg);
}

auto Generator::pop(const char *reg) -> void {
  depth_--;
  println("  ld %s, 0(sp)", reg);
  println("  addi sp, sp, 8");
}

auto Generator::expr_proxy(Expr *expr) -> void {
  switch (expr->kind) {
  case ExperKind::zero:
    return zero_expr(cast<ZeroExpr>(expr));
  case ExperKind::value:
    return value_expr(cast<ValueExpr>(expr));
  case ExperKind::integer:
    return integer_expr(cast<IntegerExpr>(expr));
  case ExperKind::unary:
    return unary_expr(cast<UnaryExpr>(expr));
  case ExperKind::binary:
    return binary_expr(cast<BinaryExpr>(expr));
  case ExperKind::call:
    return call_expr(cast<CallExpr>(expr));
  case ExperKind::stmt:
    return stmt_expr(cast<StmtExpr>(expr));
  case ExperKind::member:
    return member_expr(cast<MemberExpr>(expr));
  case ExperKind::condition:
    return condition_expr(cast<ConditionExpr>(expr));
  }
}

} // namespace lzhcc