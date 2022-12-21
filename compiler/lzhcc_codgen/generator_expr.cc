#include "lzhcc_codegen.h"

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
  case ValueKind::declaraion:
    std::abort();
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
  switch (type->kind) {
  case IntegerKind::byte:
    return println("  lb a0, 0(a0)");
  case IntegerKind::half:
    return println("  lh a0, 0(a0)");
  case IntegerKind::word:
    return println("  lw a0, 0(a0)");
  case IntegerKind::dword:
    return println("  ld a0, 0(a0)");
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

auto Generator::value_expr(ValueExpr *expr) -> void {
  value_addr(expr);
  load(expr->type);
}

auto Generator::integer_expr(IntegerExpr *expr) -> void {
  println("  li a0, %ld", expr->value);
}

static const char i64i8[] = "  slliw a0, a0, 24\n"
                            "  sraiw a0, a0, 24";
static const char i64i16[] = "  slliw a0, a0, 16\n"
                             "  sraiw a0, a0, 16";
static const char i64i32[] = "  sext.w  a0, a0";

static const char neq[] = "snez a0, a0";

// clang-format off
static const char *cast_table[][5] = {
    {nullptr, nullptr,  nullptr,  nullptr,  neq},     // i8
    {i64i8,   nullptr,  nullptr,  nullptr,  neq},     // i16
    {i64i8,   i64i16,   nullptr,  nullptr,  neq},     // i32
    {i64i8,   i64i16,   i64i32,   nullptr,  neq},     // i64
    {nullptr, nullptr,  nullptr,  nullptr,  nullptr}, // boolean
};
// clang-format on

static auto type_id(IntegerType *type) -> int {
  switch (type->kind) {
  case IntegerKind::byte:
    return 0;
  case IntegerKind::half:
    return 1;
  case IntegerKind::word:
    return 2;
  case IntegerKind::dword:
    return 3;
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
    return 4;
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::cast(Type *src, Type *dest) -> void {
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
    return println("  addw a0, a0, a1");
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
    return println("  subw a0, a0, a1");
  case IntegerKind::dword:
    return println("  sub a0, a0, a1");
  }
}

auto Generator::multiply(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return multiply_integer(cast<IntegerType>(type));
  case TypeKind::boolean:
  case TypeKind::array:
  case TypeKind::pointer:
  case TypeKind::kw_void:
  case TypeKind::function:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::multiply_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
  case IntegerKind::half:
  case IntegerKind::word:
    return println("  mulw a0, a0, a1");
  case IntegerKind::dword:
    return println("  mul a0, a0, a1");
  }
}

auto Generator::divide(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return divide_integer(cast<IntegerType>(type));
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::divide_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
  case IntegerKind::half:
  case IntegerKind::word:
    return println("  divw a0, a0, a1");
  case IntegerKind::dword:
    return println("  div a0, a0, a1");
  }
}

auto Generator::modulo(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return modulo_integer(cast<IntegerType>(type));
  case TypeKind::boolean:
  case TypeKind::kw_void:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
    std::abort();
  }
}

auto Generator::modulo_integer(IntegerType *type) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
  case IntegerKind::half:
  case IntegerKind::word:
    return println("  remw a0, a0, a1");
  case IntegerKind::dword:
    return println("  rem a0, a0, a1");
  }
}

auto Generator::binary_expr(BinaryExpr *expr) -> void {
  if (expr->kind == BinaryKind::assign) {
    expr_proxy(expr->rhs);
    push("a0");
    addr_proxy(expr->lhs);
    pop("a1");
    store(expr->type);
    println("  mv a0, a1");
    return;
  }
  if (expr->kind == BinaryKind::comma) {
    expr_proxy(expr->lhs);
    expr_proxy(expr->rhs);
    return;
  }
  expr_proxy(expr->rhs);
  push("a0");
  expr_proxy(expr->lhs);
  pop("a1");
  switch (expr->kind) {
  case BinaryKind::add:
    return add(expr->type);
  case BinaryKind::subtract:
    return subtract(expr->type);
  case BinaryKind::multiply:
    return multiply(expr->type);
  case BinaryKind::divide:
    return divide(expr->type);
  case BinaryKind::modulo:
    return modulo(expr->type);
  case BinaryKind::less_than:
    println("  slt a0, a0, a1");
    break;
  case BinaryKind::less_equal:
    println("  slt a0, a1, a0");
    println("  xori a0, a0, 1");
    break;
  case BinaryKind::equal:
    println("  xor a0, a0, a1");
    println("  seqz a0, a0");
    break;
  case BinaryKind::not_equal:
    println("  xor a0, a0, a1");
    println("  snez a0, a0");
    break;
  case BinaryKind::assign:
  case BinaryKind::comma:
    break;
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

  push("ra");
  println("  call %.*s", (int)expr->name.size(), expr->name.data());
  pop("ra");
}

auto Generator::stmt_expr(StmtExpr *expr) -> void {
  return block_stmt(expr->stmt);
}

auto Generator::member_expr(MemberExpr *expr) -> void {
  addr_proxy(expr);
  load(expr->type);
}

auto Generator::push(const char *reg) -> void {
  println("  addi sp, sp, -8");
  println("  sd %s, 0(sp)", reg);
}

auto Generator::pop(const char *reg) -> void {
  println("  ld %s, 0(sp)", reg);
  println("  addi sp, sp, 8");
}

auto Generator::expr_proxy(Expr *expr) -> void {
  switch (expr->kind) {
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
  }
}

} // namespace lzhcc