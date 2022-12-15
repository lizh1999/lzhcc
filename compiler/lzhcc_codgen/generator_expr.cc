#include "lzhcc_codegen.h"

namespace lzhcc {

auto Generator::value_addr(ValueExpr *expr) -> void {
  switch (expr->value->kind) {
  case ValueKind::local: {
    auto lvalue = cast<LValue>(expr->value);
    printf("  add a0, fp, %d\n", lvalue->offset);
    break;
  }
  case ValueKind::global: {
    auto gvalue = cast<GValue>(expr->value);
    auto name = gvalue->name;
    printf("  la a0, %.*s\n", (int)name.size(), name.data());
    break;
  }
  case ValueKind::function: {
    auto function = cast<Function>(expr->value);
    auto name = function->name;
    printf("  la a0, %.*s\n", (int)name.size(), name.data());
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

auto Generator::addr_proxy(Expr *expr) -> void {
  switch (expr->kind) {
  case ExperKind::value:
    return value_addr(cast<ValueExpr>(expr));
  case ExperKind::unary:
    return unary_addr(cast<UnaryExpr>(expr));
  default:
    expect_lvalue();
  }
}

auto Generator::load_integer(IntegerType *type) -> void {
  switch (type->size_bytes) {
  case 8:
    printf("  ld a0, 0(a0)\n");
    break;
  case 1:
    printf("  lb a0, 0(a0)\n");
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
    printf("  ld a0, 0(a0)\n");
    break;
  case TypeKind::function:
  case TypeKind::array:
    break;
  }
}

auto Generator::value_expr(ValueExpr *expr) -> void {
  value_addr(expr);
  load(expr->type);
}

auto Generator::integer_expr(IntegerExpr *expr) -> void {
  printf("  li a0, %ld\n", expr->value);
}

auto Generator::unary_expr(UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::negative:
    expr_proxy(expr->operand);
    printf("  neg a0, a0\n");
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
    printf("  sd a1, 0(a0)\n");
  case 1:
    printf("  sb a1, 0(a0)\n");
  }
}

auto Generator::store(Type *type) -> void {
  switch (type->kind) {
  case TypeKind::integer: {
    auto integer = cast<IntegerType>(type);
    return store_integer(integer);
  }
  case TypeKind::pointer:
    printf("  sd a1, 0(a0)\n");
    break;
  case TypeKind::array:
  case TypeKind::function:
    std::abort();
  }
}

auto Generator::binary_expr(BinaryExpr *expr) -> void {
  expr_proxy(expr->rhs);
  push("a0");
  if (expr->kind != BinaryKind::assign) {
    expr_proxy(expr->lhs);
  } else {
    addr_proxy(expr->lhs);
  }
  pop("a1");
  switch (expr->kind) {
  case BinaryKind::add:
    printf("  add a0, a0, a1\n");
    break;
  case BinaryKind::subtract:
    printf("  sub a0, a0, a1\n");
    break;
  case BinaryKind::multiply:
    printf("  mul a0, a0, a1\n");
    break;
  case BinaryKind::divide:
    printf("  div a0, a0, a1\n");
    break;
  case BinaryKind::less_than:
    printf("  slt a0, a0, a1\n");
    break;
  case BinaryKind::less_equal:
    printf("  slt a0, a1, a0\n");
    printf("  xori a0, a0, 1\n");
    break;
  case BinaryKind::equal:
    printf("  xor a0, a0, a1\n");
    printf("  seqz a0, a0\n");
    break;
  case BinaryKind::not_equal:
    printf("  xor a0, a0, a1\n");
    printf("  snez a0, a0\n");
    break;
  case BinaryKind::assign:
    store(expr->type);
    printf("  mv a0, a1\n");
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
  printf("  call %.*s\n", (int)expr->name.size(), expr->name.data());
  pop("ra");
}

auto Generator::push(const char *reg) -> void {
  printf("  addi sp, sp, -8\n");
  printf("  sd %s, 0(sp)\n", reg);
}

auto Generator::pop(const char *reg) -> void {
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
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
  }
}

} // namespace lzhcc