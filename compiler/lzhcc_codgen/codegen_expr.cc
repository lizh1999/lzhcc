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

auto ExprGenVisitor::visit(const BinaryExpr *expr) -> void {
  expr->rhs->visit(this);
  push();
  expr->lhs->visit(this);
  pop("a1");
  switch (expr->kind) {
  case BinaryKind::Add:
    printf("  add a0, a0, a1\n");
    break;
  case BinaryKind::Subtract:
    printf("  sub a0, a0, a1\n");
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