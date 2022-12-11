#include "lzhcc.h"
#include "lzhcc_codegen.h"

namespace lzhcc {

auto StmtGenVisitor::visit(const ExpressionStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->expr->visit(&visitor);
}

auto StmtGenVisitor::visit(const ForStmt *stmt) -> void {
  if (stmt->init) {
    stmt->init->visit(this);
  }
  int label = counter++;
  printf(".L.begin.%d:", label);
  RValueVisitor visitor;
  if (stmt->cond) {
    stmt->cond->visit(&visitor);
    printf("  beqz a0, .L.end.%d\n", label);
  }
  stmt->then->visit(this);
  if (stmt->inc) {
    stmt->inc->visit(&visitor);
  }
  printf("  j .L.begin.%d\n", label);
  printf(".L.end.%d:", label);
}

auto StmtGenVisitor::visit(const IfStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->cond->visit(&visitor);
  int label = counter++;
  printf("  beqz a0, .L.else.%d\n", label);
  stmt->then->visit(this);
  printf("  j .L.end.%d\n", label);
  printf(".L.else.%d:\n", label);
  if (stmt->else_) {
    stmt->else_->visit(this);
  }
  printf(".L.end.%d:\n", label);
}

auto StmtGenVisitor::visit(const ReturnStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->expr->visit(&visitor);
  printf("  j .L.return\n");
}

auto StmtGenVisitor::visit(const BlockStmt *stmt) -> void {
  for (auto *stmt : stmt->stmts) {
    stmt->visit(this);
  }
}

}