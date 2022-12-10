#include "lzhcc.h"
#include "lzhcc_codegen.h"
#include <cassert>
#include <cstdio>

namespace lzhcc {

auto ExprGenVisitor::visit(const IntegerExpr *expr) -> void {
  printf("  li a0, %ld\n", expr->value);
}

auto ExprGenVisitor::visit(const FloatingExpr *expr) -> void {
  assert(false && "todo");
}

auto ExprGenVisitor::visit(const UnaryExpr *expr) -> void {
  expr->operand->visit(this);
  switch (expr->kind) {
  case UnaryKind::negative:
    printf("  neg a0, a0\n");
    break;
  }
}

auto ExprGenVisitor::visit(const BinaryExpr *expr) -> void {
  expr->rhs->visit(this);
  push();
  expr->lhs->visit(this);
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
    break;
  }
}

auto ExprGenVisitor::push() -> void {
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
}

auto ExprGenVisitor::pop(const char *reg) -> void {
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
}

} // namespace lzhcc