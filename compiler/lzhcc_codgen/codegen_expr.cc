#include "lzhcc.h"
#include "lzhcc_codegen.h"
#include <cassert>
#include <cstdio>

namespace lzhcc {

auto LValueVisitor::visit(const VarRefExpr *expr) -> void {
  printf("  add a0, fp, %d\n", expr->var->offset);
}

auto RValueVisitor::visit(const IntegerExpr *expr) -> void {
  printf("  li a0, %ld\n", expr->value);
}

auto RValueVisitor::visit(const FloatingExpr *expr) -> void {
  assert(false && "todo");
}

auto RValueVisitor::visit(const VarRefExpr *expr) -> void {
  printf("  ld a0, %d(fp)\n", expr->var->offset);
}

auto RValueVisitor::visit(const UnaryExpr *expr) -> void {
  expr->operand->visit(this);
  switch (expr->kind) {
  case UnaryKind::negative:
    printf("  neg a0, a0\n");
    break;
  }
}

auto RValueVisitor::visit(const BinaryExpr *expr) -> void {
  expr->rhs->visit(this);
  push();
  if (expr->kind != BinaryKind::assign) {
    expr->lhs->visit(this);
  } else {
    LValueVisitor visitor;
    expr->lhs->visit(&visitor);
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
    printf("  sd a1, 0(a0)\n");
    printf("  mv a0, a1\n");
    break;
  }
}

auto RValueVisitor::push() -> void {
  printf("  addi sp, sp, -8\n");
  printf("  sd a0, 0(sp)\n");
}

auto RValueVisitor::pop(const char *reg) -> void {
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
}

} // namespace lzhcc