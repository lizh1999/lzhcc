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
  println("  addi a0, a0, %d", expr->member->offset);
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
  case ExperKind::call:
    return call_expr(cast<CallExpr>(expr));
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
  case Scalar::uint64:
    return println("  ld a0, 0(a0)");
  case Scalar::uint8:
    return println("  lbu a0, 0(a0)");
  case Scalar::uint16:
    return println("  lhu a0, 0(a0)");
  case Scalar::uint32:
    return println("  lwu a0, 0(a0)");
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
    assert(false);
  }
}

auto Generator::zero_expr(ZeroExpr *expr) -> void {
  addr_proxy(expr->expr);
  assert(expr->size != 0);
  println("  li a1, %ld", expr->size);
  int label = counter_++;
  println(".L.%d:", label);
  println("  sb zero, 0(a0)");
  println("  addi a1, a1, -1");
  println("  addi a0, a0, 1");
  println("  bne zero, a1, .L.%d", label);
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

static const char *cast_table[][10] = {
    {
        nullptr,                                // i8i8
        nullptr,                                // i8i16
        nullptr,                                // i8i32
        nullptr,                                // i8i64
        "  andi a0, a0, 0xff",                  // i8u8
        "  slli a0, a0, 48\n  srli a0, a0, 48", // i8u16
        nullptr,                                // i8u32
        nullptr,                                // i8u64
        "  fcvt.s.w fa0, a0",                   // i8f32
        "  fcvt.d.w fa0, a0",                   // i8f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // i16i8
        nullptr,                                  // i16i16
        nullptr,                                  // i16i32
        nullptr,                                  // i16i64
        "  andi a0, a0, 0xff",                    // i16u8
        "  slli a0, a0, 48\n  srli a0, a0, 48",   // i16u16
        nullptr,                                  // i16u32
        nullptr,                                  // i16u64
        "  fcvt.s.w fa0, a0",                     // i16f32
        "  fcvt.d.w fa0, a0",                     // i16f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // i32i8
        "  slliw a0, a0, 16\n  sraiw a0, a0, 16", // i32i16
        nullptr,                                  // i32i32
        nullptr,                                  // i32i64
        "  andi a0, a0, 0xff",                    // i32u8
        "  slli a0, a0, 48\n  srli a0, a0, 48",   // i32u16
        nullptr,                                  // i32u32
        nullptr,                                  // i32u64
        "  fcvt.s.w fa0, a0",                     // i32f32
        "  fcvt.d.w fa0, a0",                     // i32f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // i64i8
        "  slliw a0, a0, 16\n  sraiw a0, a0, 16", // i64i16
        "  sext.w a0, a0",                        // i64i32
        nullptr,                                  // i64i64
        "  andi a0, a0, 0xff",                    // i64u8
        "  slli a0, a0, 48\n  srli a0, a0, 48",   // i64u16
        "  sext.w a0, a0",                        // i64u32
        nullptr,                                  // i64u64
        "  fcvt.s.l fa0, a0",                     // i64f32
        "  fcvt.d.l fa0, a0",                     // i64f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // u8i8
        nullptr,                                  // u8i16
        nullptr,                                  // u8i32
        nullptr,                                  // u8i64
        nullptr,                                  // u8u8
        nullptr,                                  // u8u16
        nullptr,                                  // u8u32
        nullptr,                                  // u8u64
        "  fcvt.s.wu fa0, a0",                    // u8f32
        "  fcvt.d.wu fa0, a0",                    // u8f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // u16i8
        "  slliw a0, a0, 16\n  sraiw a0, a0, 16", // u16i16
        nullptr,                                  // u16i32
        nullptr,                                  // u16i64
        "  andi a0, a0, 0xff",                    // u16u8
        nullptr,                                  // u16u16
        nullptr,                                  // u16u32
        nullptr,                                  // u16u64
        "  fcvt.s.wu fa0, a0",                    // u16f32
        "  fcvt.d.wu fa0, a0",                    // u16f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // u32i8
        "  slliw a0, a0, 16\n  sraiw a0, a0, 16", // u32i16
        nullptr,                                  // u32i32
        "  slli a0, a0, 32\n  srli a0, a0, 32",   // u32i64
        "  andi a0, a0, 0xff",                    // u32u8
        "  slli a0, a0, 48\n  srli a0, a0, 48",   // u32u16
        nullptr,                                  // u32u32
        "  slli a0, a0, 32\n  srli a0, a0, 32",   // u32u64
        "  fcvt.s.wu fa0, a0",                    // u32f32
        "  fcvt.d.wu fa0, a0",                    // u32f64
    },
    {
        "  slliw a0, a0, 24\n  sraiw a0, a0, 24", // u64i8
        "  slliw a0, a0, 16\n  sraiw a0, a0, 16", // u64i16
        "  sext.w a0, a0",                        // u64i32
        nullptr,                                  // u64i64
        "  andi a0, a0, 0xff",                    // u64u8
        "  slli a0, a0, 48\n  srli a0, a0, 48",   // u64u16
        "  sext.w a0, a0",                        // u64u32
        nullptr,                                  // u64u64
        "  fcvt.s.lu fa0, a0",                    // u64f32
        "  fcvt.d.lu fa0, a0",                    // u64f64
    },
    {
        "  fcvt.w.s a0, fa0, rtz\n  slliw a0, a0, 24\n  sraiw a0, a0, "
        "24", // f32i8
        "  fcvt.w.s a0, fa0, rtz\n  slliw a0, a0, 16\n  sraiw a0, a0, "
        "16",                                            // f32i16
        "  fcvt.w.s a0, fa0, rtz\n  sext.w a0, a0",      // f32i32
        "  fcvt.l.s a0, fa0, rtz",                       // f32i64
        "  fcvt.wu.s a0, fa0, rtz\n  andi a0, a0, 0xff", // f32u8
        "  fcvt.wu.s a0, fa0, rtz\n  slli a0, a0, 48\n  srli a0, a0, "
        "48",                                        // f32u16
        "  fcvt.wu.s a0, fa0, rtz\n  sext.w a0, a0", // f32u32
        "  fcvt.lu.s a0, fa0, rtz",                  // f32u64
        nullptr,                                     // f32f32
        "  fcvt.d.s fa0, fa0",                       // f32f64
    },
    {
        "  fcvt.w.d a0, fa0, rtz\n  slliw a0, a0, 24\n  sraiw a0, a0, "
        "24", // f64i8
        "  fcvt.w.d a0, fa0, rtz\n  slliw a0, a0, 16\n  sraiw a0, a0, "
        "16",                                            // f64i16
        "  fcvt.w.d a0, fa0, rtz\n  sext.w a0, a0",      // f64i32
        "  fcvt.l.d a0, fa0, rtz",                       // f64i64
        "  fcvt.wu.d a0, fa0, rtz\n  andi a0, a0, 0xff", // f64u8
        "  fcvt.wu.d a0, fa0, rtz\n  slli a0, a0, 48\n  srli a0, a0, "
        "48",                                        // f64u16
        "  fcvt.wu.d a0, fa0, rtz\n  sext.w a0, a0", // f64u32
        "  fcvt.lu.d a0, fa0, rtz",                  // f64u64
        "  fcvt.s.d fa0, fa0",                       // f64f32
        nullptr,                                     // f64f64
    }};

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
    assert(false);
  }
}

auto Generator::cast(Type *src, Type *dest) -> void {
  if (src->kind == TypeKind::boolean) {
    return;
  }
  if (src == dest) {
    return;
  }
  if (dest->kind == TypeKind::boolean) {
    cmp_zero(src);
    return println("  snez a0, a0");
  }
  int lhs = type_id(src);
  int rhs = type_id(dest);
  if (cast_table[lhs][rhs]) {
    println("%s", cast_table[lhs][rhs]);
  }
}

auto Generator::negative(UnaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->kind) {
    case IntegerKind::word:
      return println("  negw a0, a0");
    case IntegerKind::dword:
      return println("  neg a0, a0");
    default:
      assert(false);
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fneg.s fa0, fa0");
    case FloatingKind::float64:
      return println("  fneg.d fa0, fa0");
    }
  };
  auto type = expr->type;
  switch (type->kind) {
  case TypeKind::integer:
    expr_proxy(expr->operand);
    return integer(cast<IntegerType>(type));
  case TypeKind::floating:
    expr_proxy(expr->operand);
    return floating(cast<FloatingType>(type));
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    assert(false);
  }
}

auto Generator::unary_expr(UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::negative:
    return negative(expr);
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
    cmp_zero(expr->operand->type);
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
  println("  mv t0, a0");
  println("  mv t1, a1");
  println("  li t2, %d", size);
  println("  add t2, t2, t0");
  int label = counter_++;
  println(".L.%d:", label);
  println("  lb t3, 0(t1)");
  println("  sb t3, 0(t0)");
  println("  addi t1, t1, 1");
  println("  addi t0, t0, 1");
  println("  bne t0, t2, .L.%d", label);
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
    assert(false);
  }
}

auto Generator::add(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->kind) {
    case IntegerKind::word:
      return println("  addw a0, a0, a1");
    case IntegerKind::dword:
      return println("  add a0, a0, a1");
    default:
      assert(false);
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fadd.s fa0, fa0, fa1");
    case FloatingKind::float64:
      return println("  fadd.d fa0, fa0, fa1");
    }
  };
  auto type = expr->type;
  switch (type->kind) {
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::array:
  case TypeKind::pointer:
    visit(expr);
    return println("  add a0, a0, a1");
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::function:
  case TypeKind::record:
    assert(false);
  }
}
auto Generator::subtract(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->kind) {
    case IntegerKind::word:
      return println("  subw a0, a0, a1");
    case IntegerKind::dword:
      return println("  sub a0, a0, a1");
    default:
      assert(false);
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fsub.s fa0, fa0, fa1");
    case FloatingKind::float64:
      return println("  fsub.d fa0, fa0, fa1");
    }
  };
  auto type = expr->type;
  switch (type->kind) {
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::array:
  case TypeKind::pointer:
    visit(expr);
    return println("  sub a0, a0, a1");
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::function:
  case TypeKind::record:
    assert(false);
  }
}

auto Generator::multiply(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->kind) {
    case IntegerKind::word:
      return println("  mulw a0, a0, a1");
    case IntegerKind::dword:
      return println("  mul a0, a0, a1");
    default:
      assert(false);
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fmul.s fa0, fa0, fa1");
    case FloatingKind::float64:
      return println("  fmul.d fa0, fa0, fa1");
    }
  };
  auto type = expr->type;
  switch (type->kind) {
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    assert(false);
  }
}

auto Generator::divide(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (pattern(type->kind, type->sign)) {
    case Scalar::int32:
      return println("  divw a0, a0, a1");
    case Scalar::int64:
      return println("  div a0, a0, a1");
    case Scalar::uint32:
      return println("  divuw a0, a0, a1");
    case Scalar::uint64:
      return println("  divu a0, a0, a1");
    default:
      assert(false);
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fdiv.s fa0, fa0, fa1");
    case FloatingKind::float64:
      return println("  fdiv.d fa0, fa0, fa1");
    }
  };
  auto type = expr->type;
  switch (type->kind) {
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    break;
  }
}

auto Generator::modulo(Type *type) -> void {
  assert(type->kind == TypeKind::integer);
  auto integer = cast<IntegerType>(type);
  switch (pattern(integer->kind, integer->sign)) {
  case Scalar::int32:
    return println("  remw a0, a0, a1");
  case Scalar::int64:
    return println("  rem a0, a0, a1");
  case Scalar::uint32:
    return println("  remuw a0, a0, a1");
  case Scalar::uint64:
    return println("  remu a0, a0, a1");
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
  switch (pattern(integer->kind, integer->sign)) {
  case Scalar::int32:
    return println("  sraw a0, a0, a1");
  case Scalar::int64:
    return println("  sra a0, a0, a1");
  case Scalar::uint32:
    return println("  srlw a0, a0, a1");
  case Scalar::uint64:
    return println("  srl a0, a0, a1");
  default:
    assert(false);
  }
}

auto Generator::less_than(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->sign) {
    case Sign::sign:
      return println("  slt a0, a0, a1");
    case Sign::unsign:
      return println("  sltu a0, a0, a1");
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  flt.s a0, fa0, fa1");
    case FloatingKind::float64:
      return println("  flt.d a0, fa0, fa1");
    }
  };
  auto type = expr->lhs->type;
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::record:
    assert(false);
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
    visit(expr);
    return println("  sltu a0, a0, a1");
  }
}

auto Generator::less_equal(BinaryExpr *expr) -> void {
  auto integer = [&](IntegerType *type) {
    switch (type->sign) {
    case Sign::sign:
      println("  slt a0, a1, a0");
      return println("  xori a0, a0, 1");
    case Sign::unsign:
      println("  sltu a0, a1, a0");
      return println("  xori a0, a0, 1");
    }
  };
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fle.s a0, fa0, fa1");
    case FloatingKind::float64:
      return println("  fle.d a0, fa0, fa1");
    }
  };
  auto type = expr->lhs->type;
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::record:
    assert(false);
  case TypeKind::integer:
    visit(expr);
    return integer(cast<IntegerType>(type));
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
    visit(expr);
    println("  sltu a0, a1, a0");
    return println("  xori a0, a0, 1");
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  }
}

auto Generator::equal(BinaryExpr *expr) -> void {
  auto floating = [&](FloatingType *type) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  feq.s a0, fa0, fa1");
    case FloatingKind::float64:
      return println("  feq.d a0, fa0, fa1");
    }
  };

  auto type = expr->lhs->type;
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
  case TypeKind::record:
    assert(false);
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::integer:
    visit(expr);
    println("  sub a0, a0, a1");
    return println("  seqz a0, a0");
  case TypeKind::floating:
    visitf(expr);
    return floating(cast<FloatingType>(type));
  }
}

auto Generator::visit(BinaryExpr *expr) -> void {
  expr_proxy(expr->rhs);
  push("a0");
  expr_proxy(expr->lhs);
  pop("a1");
}

auto Generator::visitf(BinaryExpr *expr) -> void {
  expr_proxy(expr->rhs);
  pushf("fa0");
  expr_proxy(expr->lhs);
  popf("fa1");
}

static auto get_member(Expr *expr) -> Member * {
  if (expr->kind != ExperKind::member) {
    return nullptr;
  } else {
    return cast<MemberExpr>(expr)->member;
  }
}

auto Generator::binary_expr(BinaryExpr *expr) -> void {
#define rvalue()                                                               \
  expr_proxy(expr->rhs);                                                       \
  push("a0");                                                                  \
  expr_proxy(expr->lhs);                                                       \
  pop("a1")

  switch (expr->kind) {
  case BinaryKind::add:
    return add(expr);
  case BinaryKind::subtract:
    return subtract(expr);
  case BinaryKind::multiply:
    return multiply(expr);
  case BinaryKind::divide:
    return divide(expr);
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
    return less_than(expr);
  case BinaryKind::less_equal:
    return less_equal(expr);
  case BinaryKind::equal:
    return equal(expr);
  case BinaryKind::not_equal:
    equal(expr);
    return println("  xori a0, a0, 1");
  case BinaryKind::shift_left:
    rvalue();
    return shift_left(expr->type);
  case BinaryKind::shift_right:
    rvalue();
    return shift_right(expr->type);
  case BinaryKind::assign:
    if (expr->type->kind == TypeKind::floating) {
      expr_proxy(expr->rhs);
      pushf("fa0");
      addr_proxy(expr->lhs);
      popf("fa0");
      store(expr->type);
    } else {
      expr_proxy(expr->rhs);
      push("a0");
      addr_proxy(expr->lhs);
      pop("a1");
      auto mem = get_member(expr->lhs);
      if (mem && mem->is_bitfield) {
        println("  li t0, %ld", (1l << mem->bit_width) - 1);
        println("  and a1, a1, t0");
        println("  slli a1, a1, %d", mem->bit_offset);
        println("  mv t1, a0");
        load(mem->type);
        long mask =((1l << mem->bit_width) - 1) << mem->bit_offset;
        println("  li t0, %ld", ~mask);
        println("  and a0, a0, t0");
        println("  or a1, a0, a1");
        println("  mv  a0, t1");
      }
      store(expr->type);
      println("  mv a0, a1");
    }
    break;
  case BinaryKind::comma:
    expr_proxy(expr->lhs);
    return expr_proxy(expr->rhs);
  case BinaryKind::logical_and: {
    int label = counter_++;
    expr_proxy(expr->lhs);
    cmp_zero(expr->lhs->type);
    println("  beq a0, zero, .L.and.%d", label);
    expr_proxy(expr->rhs);
    cmp_zero(expr->lhs->type);
    println(".L.and.%d:", label);
    return println("  snez a0, a0");
  }
  case BinaryKind::logical_or: {
    int label = counter_++;
    expr_proxy(expr->lhs);
    cmp_zero(expr->lhs->type);
    println("  bne a0, zero, .L.or.%d", label);
    expr_proxy(expr->rhs);
    cmp_zero(expr->lhs->type);
    println(".L.or.%d:", label);
    return println("  snez a0, a0");
  }
  }
}

static auto push(Type *&first, Type *&second, Type *in) -> bool {
  if (first && second) {
    return false;
  } else if (first) {
    second = in;
  } else {
    first = in;
  }
  return true;
}

static auto dump(Context *ctx, RecordType *record, Type *&first, Type *&second,
                 int *offset) -> bool {
  if (record->is_union) {
    return false;
  }
  for (auto member : record->members) {
    if (!dump(ctx, member.type, first, second, offset)) {
      return false;
    } else if (second) {
      *offset += member.offset;
    }
  }
  return true;
}

auto dump(Context *ctx, Type *type, Type *&first, Type *&second, int *offset)
    -> bool {
  switch (type->kind) {
  default:
    assert(false);
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::floating:
  case TypeKind::pointer:
    return push(first, second, type);
  case TypeKind::record:
    return dump(ctx, cast<RecordType>(type), first, second, offset);
  case TypeKind::array: {
    auto array = cast<ArrayType>(type);
    for (int i = 0; i < array->length; i++) {
      if (!dump(ctx, array->base, first, second, offset)) {
        return false;
      } else if (second) {
        *offset += i * ctx->size_of(array->base);
      }
    }
    return true;
  }
  }
}

auto Calling::operator()(CallExpr *expr) -> std::vector<Pass> {
  std::vector<Pass> pass(expr->args.size());
  auto &args = expr->args;
  if (16 < ctx_->size_of(expr->type)) {
    gp_++;
  }
  for (int i = 0; i < expr->arg_num; i++) {
    pass[i] = floating(args[i]->type);
  }
  for (int i = expr->arg_num; i < args.size(); i++) {
    pass[i] = integer(args[i]->type);
  }
  return pass;
}

auto Calling::operator()(Function *func) -> std::vector<Pass> {
  auto &param = func->params;
  auto func_type = cast<FunctionType>(func->type);
  if (16 < ctx_->size_of(func_type->ret)) {
    gp_++;
  }
  std::vector<Pass> pass(param.size());
  for (int i = 0; i < param.size(); i++) {
    pass[i] = floating(param[i]->type);
  }
  return pass;
}

static auto is_float(Type *type) -> bool {
  return type->kind == TypeKind::floating;
}

auto Calling::fpfp() -> bool { return fp_ + 1 < fp_max; }
auto Calling::gpgp() -> bool { return gp_ + 1 < gp_max; }
auto Calling::fpgp() -> bool { return fp_ < fp_max && gp_ < gp_max; }

auto Calling::floating(Type *type) -> Pass {
  if (is_float(type)) {
    return fp_ < fp_max ? Pass{PassKind::fp, fp_++} : integer(type);
  } else if (type->kind == TypeKind::record) {
    Type *first = 0, *second = 0;
    int offset = 0;
    auto record = cast<RecordType>(type);
    if (!dump(ctx_, record, first, second, &offset)) {
      return integer(type);
    } else if (second == 0) {
      return floating(first);
    } else if (is_float(first) && is_float(second) && fpfp()) {
      return Pass{PassKind::fpfp,        fp_++, fp_++, ctx_->size_of(first),
                  ctx_->size_of(second), offset};
    } else if (is_float(first) && !is_float(second) && fpgp()) {
      return Pass{PassKind::fpgp,        fp_++, gp_++, ctx_->size_of(first),
                  ctx_->size_of(second), offset};
    } else if (!is_float(first) && is_float(second) && fpgp()) {
      return Pass{PassKind::gpfp,        gp_++, fp_++, ctx_->size_of(first),
                  ctx_->size_of(second), offset};
    }
  }
  return integer(type);
}

auto Calling::integer(Type *type) -> Pass {
  int size = ctx_->size_of(type);
  if (size <= 8 && gp_ < gp_max) {
    return Pass{PassKind::gp, gp_++};
  } else if (size <= 8 /* && gp_ == gp_max */) {
    return Pass{PassKind::sp, sp_++};
  } else if (size <= 16 && gpgp()) {
    return Pass{PassKind::gpgp, gp_++, gp_++};
  } else if (size <= 16 && gp_ < gp_max) {
    return Pass{PassKind::gpsp, gp_++, sp_++};
  } else if (size <= 16 /* && gp_ == gp_max */) {
    auto pass = Pass{PassKind::spsp, sp_++, sp_++};
    return pass;
  } else {
    int align = ctx_->align_of(type);
    ref_ = align_to(ref_, std::min(align, 16));
    auto pass = gp_ < gp_max ? Pass{PassKind::refgp, ref_, gp_++}
                             : Pass{PassKind::refsp, ref_, sp_++};
    ref_ += size;
    return pass;
  }
}

auto Generator::call_expr(CallExpr *expr) -> void {
  auto &args = expr->args;
  Calling calling(context_);
  auto pass = calling(expr);

  int ref = -align_to(-depth_, 2) * 8 - calling.ref_aligned_bytes();
  int stk = ref - calling.stack_aligned_bytes();
  int reg = stk - calling.reg_bytes();
  int old_depth = depth_;
  depth_ = reg / 8;
  expr_proxy(expr->func);

  println("# old_dep: %d new_dep: %d ref: %d stk: %d reg: %d", old_depth * 8,
          depth_ * 8, calling.ref_aligned_bytes(),
          calling.stack_aligned_bytes(), calling.reg_bytes());
  push("a0");

  auto memcpy = [&](int dest, int bytes) {
    println("  li t0, %d", dest);
    println("  li t1, %d", bytes + dest);
    println("  add t1, sp, t1");
    println("  add t0, sp, t0");
    int label = counter_++;
    println(".L.%d:", label);
    println("  lb t2, 0(a0)");
    println("  sb t2, 0(t0)");
    println("  addi a0, a0, 1");
    println("  addi t0, t0, 1");
    println("  bne t0, t1, .L.%d", label);
  };

  auto store_dword = [&](Type *type, int dest) {
    if (type->kind == TypeKind::record) {
      println("  ld a0, 0(a0)");
      println("  sd a0, %d(sp)", dest);
    } else if (type->kind == TypeKind::floating) {
      println("  fsd fa0, %d(sp)", dest);
    } else {
      println("  sd a0, %d(sp)", dest);
    }
  };

  auto store_gp = [&](int size, int src, int dest) {
    switch (size) {
    case 1:
      println("  lb t0, %d(a0)", src);
      break;
    case 2:
      println("  lh t0, %d(a0)", src);
      break;
    case 4:
      println("  lw t0, %d(a0)", src);
      break;
    case 8:
      println("  ld t0, %d(a0)", src);
      break;
    default:
      assert(false);
    }
    println("  sd t0, %d(sp)", dest);
  };

  auto store_fp = [&](int size, int src, int dest) {
    switch (size) {
    case 4:
      println("  flw fa0, %d(a0)", src);
      break;
    case 8:
      println("  fld fa0, %d(a0)", src);
      break;
    default:
      fprintf(stderr, "error: %d\n", size);
      assert(false);
    }
    println("  fsd fa0, %d(sp)", dest);
  };

  int cnt = 0;
  for (int i = args.size(); i--;) {
    println("# generate %dth argument", i);
    expr_proxy(expr->args[i]);
    println("# push %dth argument", i);
    auto type = expr->args[i]->type;
    int size = context_->size_of(type);
    auto [kind, inner0, inner1, inner2, inner3, inner4] = pass[i];
    switch (kind) {
    case PassKind::gp:
    case PassKind::fp:
      store_dword(type, reg + cnt++ * 8);
      break;
    case PassKind::sp:
      println("# sp");
      store_dword(type, stk + inner0 * 8);
      break;
    case PassKind::spsp:
      println("  ld t0, 0(a0)");
      println("  ld t1, 8(a0)");
      println("  sd t0, %d(sp)", stk + inner0 * 8);
      println("  sd t1, %d(sp)", stk + inner1 * 8);
      break;
    case PassKind::gpsp:
      println("# gpsp");
      println("  ld t0, 0(a0)");
      println("  ld t1, 8(a0)");
      println("  sd t0, %d(sp)", reg + cnt++ * 8);
      println("  sd t1, %d(sp)", stk + inner1 * 8);
      break;
    case PassKind::gpgp:
      println("  ld t0, 0(a0)");
      println("  ld t1, 8(a0)");
      println("  sd t0, %d(sp)", reg + cnt++ * 8);
      println("  sd t1, %d(sp)", reg + cnt++ * 8);
      break;
    case PassKind::fpfp:
      store_fp(inner2, 0, reg + cnt++ * 8);
      store_fp(inner3, inner4, reg + cnt++ * 8);
      break;
    case PassKind::fpgp:
      store_fp(inner2, 0, reg + cnt++ * 8);
      store_gp(inner3, inner4, reg + cnt++ * 8);
      break;
    case PassKind::gpfp:
      store_gp(inner2, 0, reg + cnt++ * 8);
      store_fp(inner3, inner4, reg + cnt++ * 8);
      break;
    case PassKind::refgp:
      memcpy(ref + inner0, size);
      break;
    case PassKind::refsp:
      memcpy(ref + inner0, size);
      println("  li t0, %d", ref + inner0);
      println("  addi t0, t0, sp");
      println("  sd t0, %d(sp)", stk + inner1 * 8);
      break;
    }
  }
  for (int i = 0; i < args.size(); i++) {
    println("# pop %dth argument", i);
    switch (pass[i].kind) {
    case PassKind::gp:
    case PassKind::gpsp:
      println("  ld a%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::fp:
      println("  fld fa%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::gpgp:
      println("  ld a%d, %d(sp)", pass[i].inner1, reg + --cnt * 8);
      println("  ld a%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::fpfp:
      println("  fld fa%d, %d(sp)", pass[i].inner1, reg + --cnt * 8);
      println("  fld fa%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::fpgp:
      println("  ld a%d, %d(sp)", pass[i].inner1, reg + --cnt * 8);
      println("  fld fa%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::gpfp:
      println("  fld fa%d, %d(sp)", pass[i].inner1, reg + --cnt * 8);
      println("  ld a%d, %d(sp)", pass[i].inner0, reg + --cnt * 8);
      break;
    case PassKind::refgp:
      println("  li a%d, %d", pass[i].inner1, ref + pass[i].inner0);
      println("  add a%d, sp, a%d", pass[i].inner1, pass[i].inner1);
      break;
    case PassKind::sp:
    case PassKind::refsp:
    case PassKind::spsp:
      break;
    }
  }
  if (16 < context_->size_of(expr->type)) {
    println("  li a0, %d", expr->ret_buffer->offset);
    println("  add a0, fp, a0");
  }
  pop("t1");
  assert(depth_ == reg / 8);
  println("  li t0, %d", stk);
  println("  add sp, sp, t0");
  println("  jalr t1");
  println("  li t0, %d", -stk);
  println("  add sp, sp, t0");
  depth_ = old_depth;

  if (!expr->ret_buffer) {
    return;
  }
  auto stroe_fp2 = [&](FloatingType *type, int src, int dest) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  fsw fa%d, %d(t2)", src, dest);
    case FloatingKind::float64:
      return println("  fsd fa%d, %d(t2)", src, dest);
    default:
      assert(false);
    }
  };
  auto store_gp2 = [&](IntegerType *type, int src, int dest) {
    switch (type->kind) {
    case IntegerKind::byte:
      return println("  sb a%d, %d(t2)", src, dest);
    case IntegerKind::half:
      return println("  sh a%d, %d(t2)", src, dest);
    case IntegerKind::word:
      return println("  sw a%d, %d(t2)", src, dest);
    case IntegerKind::dword:
      return println("  sd a%d, %d(t2)", src, dest);
    }
  };
  auto store = [&](Type *type, int src, int dest) {
    switch (type->kind) {
    case TypeKind::boolean:
      return println("  sb a%d, %d(t2)", src, dest);
    case TypeKind::integer:
      return store_gp2(cast<IntegerType>(type), src, dest);
    case TypeKind::floating:
      return stroe_fp2(cast<FloatingType>(type), src, dest);
    case TypeKind::pointer:
      return println("  sd a%d, %d(t2)", src, dest);
    case TypeKind::function:
    case TypeKind::array:
    case TypeKind::record:
    case TypeKind::kw_void:
      assert(false);
    }
  };
  auto store_size = [&](int bytes, int src, int dest) {
    println("  mv t0, a%d", src);
    switch (bytes) {
    case 8:
      return println("  sd t0, %d(t2)", dest);
    case 7:
      println("  srli t1, t0, 48");
      println("  sb t1, %d(t2)", dest + 6);
    case 6:
      println("  srli t1, a0, 32");
      println("  sh t1, %d(sp)", dest + 4);
      return println("  sw t0, %d(t2)", dest);
    case 5:
      println("  srli t1, a0, 32");
      println("  sb t1, %d(t2)", dest + 4);
    case 4:
      return println("  sw t0, %d(t2)", dest);
    case 3:
      println("  srli t1, t0, 16");
      println("  sb t1, %d(t2)", dest + 2);
    case 2:
      return println("  sh t0, %d(t2)", dest);
    case 1:
      return println("  sb t0, %d(t2)", dest);
    }
  };
  Type *first = 0, *second = 0;
  int offset = 0, size = context_->size_of(expr->type);
  println("# save result");
  println("  li t0, %d", expr->ret_buffer->offset);
  println("  add t2, fp, t0");
  if (dump(context_, expr->type, first, second, &offset)) {
    if (!second) {
      store(first, 0, 0);
      return println("  mv a0, t2");
    } else if (is_float(first) && is_float(second)) {
      store(first, 0, 0);
      store(second, 1, offset);
      return println("  mv a0, t2");
    } else if (is_float(first) && !is_float(second)) {
      store(first, 0, 0);
      store(second, 0, offset);
      return println("  mv a0, t2");
    } else if (!is_float(first) && is_float(second)) {
      store(first, 0, 0);
      store(second, 0, offset);
      return println("  mv a0, t2");
    }
  }
  if (size <= 8) {
    store_size(size, 0, 0);
  } else if (size <= 16) {
    store_size(8, 0, 0);
    store_size(size - 8, 1, 8);
  }
  return println("  mv a0, t2");
}

auto Generator::stmt_expr(StmtExpr *expr) -> void {
  return block_stmt(expr->stmt);
}

auto Generator::member_expr(MemberExpr *expr) -> void {
  addr_proxy(expr);
  load(expr->type);
  auto mem = expr->member;
  if (mem->is_bitfield) {
    println("  slli a0, a0, %d", 64 - mem->bit_width - mem->bit_offset);
    if (mem->type->kind != TypeKind::integer ||
        cast<IntegerType>(mem->type)->sign != Sign::sign) {
      println("  srli a0, a0, %d", 64 - mem->bit_width);
    } else {
      println("  srai a0, a0, %d", 64 - mem->bit_width);
    }
  }
}

auto Generator::condition_expr(ConditionExpr *expr) -> void {
  expr_proxy(expr->cond);
  cmp_zero(expr->cond->type);
  int label = counter_++;
  println("  beqz a0, .L.else.%d", label);
  expr_proxy(expr->then);
  println("  j .L.end.%d", label);
  println(".L.else.%d:", label);
  expr_proxy(expr->else_);
  println(".L.end.%d:", label);
}

auto Generator::push(const char *reg) -> void {
  println("# push");
  println("  sd %s, %d(sp)", reg, --depth_ * 8);
  println("# push depth %d", depth_);
}

auto Generator::pop(const char *reg) -> void {
  println("# pop depth %d", depth_);
  println("  ld %s, %d(sp)", reg, depth_++ * 8);
  println("# pop");
}

auto Generator::pushf(const char *reg) -> void {
  println("  fsd %s, %d(sp)", reg, --depth_ * 8);
}

auto Generator::popf(const char *reg) -> void {
  println("  fld %s, %d(sp)", reg, depth_++ * 8);
}

auto Generator::pop(int reg) -> void {
  println("  ld a%d, %d(sp)", reg, depth_++ * 8);
}

auto Generator::popf(int reg) -> void {
  println("  fld fa%d, %d(sp)", reg, depth_++ * 8);
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

auto Generator::cmp_zero(Type *type) -> void {
  if (type->kind != TypeKind::floating) {
    return;
  }
  switch (cast<FloatingType>(type)->kind) {
  case FloatingKind::float32:
    println("  fmv.w.x fa1, zero");
    println("  feq.s a0, fa0, fa1");
    println("  xori a0, a0, 1");
    break;
  case FloatingKind::float64:
    println("  fmv.d.x fa1, zero");
    println("  feq.d a0, fa0, fa1");
    println("  xori a0, a0, 1");
    break;
  }
}

} // namespace lzhcc