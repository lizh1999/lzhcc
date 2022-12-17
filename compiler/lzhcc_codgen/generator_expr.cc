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
  switch (type->size_bytes) {
  case 8:
    println("  ld a0, 0(a0)");
    break;
  case 1:
    println("  lb a0, 0(a0)");
    break;
  }
}

auto Generator::load(Type *type) -> void {
  switch (type->kind) {
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
  }
}

auto Generator::value_expr(ValueExpr *expr) -> void {
  value_addr(expr);
  load(expr->type);
}

auto Generator::integer_expr(IntegerExpr *expr) -> void {
  println("  li a0, %ld", expr->value);
}

auto Generator::unary_expr(UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::negative:
    expr_proxy(expr->operand);
    println("  neg a0, a0");
    break;
  case UnaryKind::deref:
    expr_proxy(expr->operand);
    load(expr->type);
    break;
  case UnaryKind::refrence:
    addr_proxy(expr->operand);
    break;
  }
}

auto Generator::store_integer(IntegerType *type) -> void {
  switch (type->size_bytes) {
  case 8:
    println("  sd a1, 0(a0)");
    break;
  case 1:
    println("  sb a1, 0(a0)");
    break;
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
    std::abort();
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
    println("  add a0, a0, a1");
    break;
  case BinaryKind::subtract:
    println("  sub a0, a0, a1");
    break;
  case BinaryKind::multiply:
    println("  mul a0, a0, a1");
    break;
  case BinaryKind::divide:
    println("  div a0, a0, a1");
    break;
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