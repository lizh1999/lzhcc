#include "lzhcc_codegen.h"
#include <bit>
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
  switch (pattern(type->kind, type->sign)) {
  case Scalar::int8:
    return println("  lb a0, 0(a0)");
  case Scalar::int16:
    return println("  lh a0, 0(a0)");
  case Scalar::int32:
    return println("  lw a0, 0(a0)");
  case Scalar::int64:
    return println("  ld a0, 0(a0)");
  case Scalar::uint8:
    return println("  lbu a0, 0(a0)");
  case Scalar::uint16:
    return println("  lhu a0, 0(a0)");
  case Scalar::uint32:
    return println("  lwu a0, 0(a0)");
  case Scalar::uint64:
    return println("  ldu a0, 0(a0)");
  }
}

auto Generator::load_floating(FloatingType *type) -> void {
  switch (type->kind) {
    case FloatingKind::float32:
      return println("  flw fa0, 0(a0)");
    case FloatingKind::float64:
      return println("  fld fa0, 0(a0)");
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
  case TypeKind::floating: {
    auto floating = cast<FloatingType>(type);
    return load_floating(floating);
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

auto Generator::floating_expr(FloatingExpr *expr) -> void {
  assert(expr->type->kind == TypeKind::floating);
  auto floating = cast<FloatingType>(expr->type);
  switch (floating->kind) {
  case FloatingKind::float32: {
    float f32 = expr->value;
    println("  li a0, %u", std::bit_cast<uint32_t>(f32));
    return println("  fmv.w.x fa0, a0");
  }
  case FloatingKind::float64:
    println("  li a0, %lu", std::bit_cast<uint64_t>(expr->value));
    return println("  fmv.d.x fa0, a0");
  }
}

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
static const char i64f32[] = "  fcvt.s.l fa0, a0";
static const char i64f64[] = "  fcvt.d.l fa0, a0";

static const char u64f32[] = "  fcvt.s.lu fa0, a0";
static const char u64f64[] = "  fcvt.d.lu fa0, a0";

static const char f32i8[] = "  fcvt.w.s a0, fa0, rtz\n"
                            "  slli a0, a0, 56\n"
                            "  srai a0, a0, 56";
static const char f32i16[] = "  fcvt.w.s a0, fa0, rtz\n"
                             "  slli a0, a0, 48\n"
                             "  srai a0, a0, 48";
static const char f32i32[] = "  fcvt.w.s a0, fa0, rtz";
static const char f32i64[] = "  fcvt.l.s a0, fa0, rtz";
static const char f32u8[] = "  fcvt.wu.s a0, fa0, rtz\n"
                            "  slli a0, a0, 56\n"
                            "  srli a0, a0, 56";
static const char f32u16[] = "  fcvt.wu.s a0, fa0, rtz\n"
                             "  slli a0, a0, 48\n"
                             "  srli a0, a0, 48";
static const char f32u32[] = "  fcvt.wu.s a0, fa0, rtz";
static const char f32u64[] = "  fcvt.lu.s a0, fa0, rtz";
static const char f32f64[] = "  fcvt.d.s fa0, fa0";

static const char f64i8[] = "  fcvt.w.d a0, fa0, rtz\n"
                            "  slli a0, a0, 56\n"
                            "  srai a0, a0, 56";
static const char f64i16[] = "  fcvt.w.d a0, fa0, rtz\n"
                             "  slli a0, a0, 48\n"
                             "  srai a0, a0, 48";
static const char f64i32[] = "  fcvt.w.d a0, fa0, rtz";
static const char f64i64[] = "  fcvt.l.d a0, fa0, rtz";

static const char f64u8[] = "  fcvt.wu.d a0, fa0, rtz\n"
                            "  slli a0, a0, 56\n"
                            "  srli a0, a0, 56";
static const char f64u16[] = "  fcvt.wu.d a0, fa0, rtz\n"
                             "  slli a0, a0, 48\n"
                             "  srli a0, a0, 48";
static const char f64u32[] = "  fcvt.wu.d a0, fa0, rtz";
static const char f64u64[] = "  fcvt.lu.d a0, fa0, rtz";

static const char f64f32[] = "  fcvt.s.d fa0, fa0";

// clang-fromat off
static const char *cast_table[][10] = {
    {0,     0,      0,      0,      i64u8,  0,      0,      0,      i64f32, i64f64},  // i8
    {i64i8, 0,      0,      0,      i64u8,  i64u16, 0,      0,      i64f32, i64f64},  // i16
    {i64i8, i64i16, 0,      0,      i64u8,  i64u16, i64u32, 0,      i64f32, i64f64},  // i32
    {i64i8, i64i16, i64i32, 0,      i64u8,  i64u16, i64u32, 0,      i64f32, i64f64},  // i64
    {i64i8, 0,      0,      0,      0,      0,      0,      0,      u64f32, u64f64},  // u8
    {i64i8, i64i16, 0,      0,      i64u8,  0,      0,      0,      u64f32, u64f64},  // u16
    {i64i8, i64i16, i64i32, 0,      i64u8,  i64u16, 0,      0,      u64f32, u64f64},  // u32
    {i64i8, i64i16, i64i32, 0,      i64u8,  i64u16, i64u32, 0,      u64f32, u64f64},  // u64
    {f32i8, f32i16, f32i32, f32i64, f32u8,  f32u16, f32u32, f32u64, 0,      f32f64},  // f32
    {f64i8, f64i16, f64i32, f64i64, f64u8,  f64u16, f64u32, f64u64, f64f32, 0},       // f64
};
// clang-format on

static auto type_id(IntegerType *type) -> int {
  return static_cast<int>(pattern(type->kind, type->sign));
}

static auto type_id(FloatingType *type) -> int {
  switch (type->kind) {
    case FloatingKind::float32:
      return 8;
    case FloatingKind::float64:
      return 9;
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
  case TypeKind::floating:
    return type_id(cast<FloatingType>(type));
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
    return println("  snez a0, a0");
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

auto Generator::store_floating(FloatingType *type) -> void {
  switch (type->kind) {
  case FloatingKind::float32:
    return println("  fsw fa0, 0(a0)");
  case FloatingKind::float64:
    return println("  fsd fa0, 0(a0)");
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
  case TypeKind::floating: {
    auto floating = cast<FloatingType>(type);
    return store_floating(floating);
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
  case TypeKind::floating:
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
  case TypeKind::floating:
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
  case TypeKind::floating:
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
  case TypeKind::floating:
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
  case ExperKind::floating:
    return floating_expr(cast<FloatingExpr>(expr));
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