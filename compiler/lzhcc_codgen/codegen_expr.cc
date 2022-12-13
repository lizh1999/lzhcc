#include "lzhcc.h"
#include "lzhcc_codegen.h"
#include <cassert>
#include <cstdio>

namespace lzhcc {

auto LValueVisitor::visit(const VarRefExpr *expr) -> void {
  printf("  add a0, fp, %d\n", expr->var->offset);
}

auto LValueVisitor::visit(const UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::deref:
    expr->operand->visit(rvisitor_);
    break;
  default:
    expect_lvalue();
  }
}

auto RValueVisitor::visit(const IntegerExpr *expr) -> void {
  printf("  li a0, %ld\n", expr->value);
}

auto RValueVisitor::visit(const VarRefExpr *expr) -> void {
  printf("  ld a0, %d(fp)\n", expr->var->offset);
}

auto RValueVisitor::visit(const UnaryExpr *expr) -> void {
  switch (expr->kind) {
  case UnaryKind::negative:
    expr->operand->visit(this);
    printf("  neg a0, a0\n");
    break;
  case UnaryKind::deref:
    expr->operand->visit(this);
    printf("  ld a0, 0(a0)\n");
    break;
  case UnaryKind::refrence:
    expr->operand->visit(lvisitor_);
    break;
  }
}

auto RValueVisitor::visit(const BinaryExpr *expr) -> void {
  expr->rhs->visit(this);
  push();
  if (expr->kind != BinaryKind::assign) {
    expr->lhs->visit(this);
  } else {
    expr->lhs->visit(lvisitor_);
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

auto RValueVisitor::visit(const CallExpr *expr) -> void {
  auto str = context_->literal(expr->name->inner);
  push("ra");
  printf("  call %.*s\n", (int) str.size(), str.data());
  pop("ra");
}

auto RValueVisitor::push(const char *reg) -> void {
  printf("  addi sp, sp, -8\n");
  printf("  sd %s, 0(sp)\n", reg);
}

auto RValueVisitor::pop(const char *reg) -> void {
  printf("  ld %s, 0(sp)\n", reg);
  printf("  addi sp, sp, 8\n");
}

} // namespace lzhcc