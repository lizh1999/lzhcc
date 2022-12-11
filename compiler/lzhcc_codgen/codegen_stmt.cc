#include "lzhcc.h"
#include "lzhcc_codegen.h"

namespace lzhcc {

auto StmtGenVisitor::visit(const ExpressionStmt *stmt) -> void {
  RValueVisitor visitor;
  stmt->expr->visit(&visitor);
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