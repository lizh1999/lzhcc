#include "lzhcc.h"
#include "lzhcc_codegen.h"

namespace lzhcc {

auto StmtGenVisitor::visit(const ExpressionStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->expr->visit(&visitor);
}

auto StmtGenVisitor::visit(const IfStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->cond->visit(&visitor);
  int lable = counter++;
  printf("  beqz a0, .L.else.%d\n", lable);
  stmt->than->visit(this);
  printf("  j .L.end.%d\n", lable);
  printf(".L.else.%d:\n", lable);
  if (stmt->else_) {
    stmt->else_->visit(this);
  }
  printf(".L.end.%d:\n", lable);
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