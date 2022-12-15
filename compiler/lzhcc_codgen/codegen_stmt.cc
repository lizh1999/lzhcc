#include "lzhcc.h"
#include "lzhcc_codegen.h"

namespace lzhcc {

StmtGenVisitor::StmtGenVisitor(Context *context) {
  lvisitor_.rvisitor_ = &rvisitor_;
  rvisitor_.lvisitor_ = &lvisitor_;
  rvisitor_.context_ = context;
  lvisitor_.context_ = context;
}

auto StmtGenVisitor::visit(const ExpressionStmt *stmt) -> void {
  stmt->expr->visit(&rvisitor_);
}

auto StmtGenVisitor::visit(const ForStmt *stmt) -> void {
  if (stmt->init) {
    stmt->init->visit(this);
  }
  int label = counter++;
  printf(".L.begin.%d:", label);
  if (stmt->cond) {
    stmt->cond->visit(&rvisitor_);
    printf("  beqz a0, .L.end.%d\n", label);
  }
  stmt->then->visit(this);
  if (stmt->inc) {
    stmt->inc->visit(&rvisitor_);
  }
  printf("  j .L.begin.%d\n", label);
  printf(".L.end.%d:", label);
}

auto StmtGenVisitor::visit(const IfStmt *stmt) -> void {
  stmt->cond->visit(&rvisitor_);
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
  stmt->expr->visit(&rvisitor_);
  printf("  j .L.return.%d\n", return_label);
}

auto StmtGenVisitor::visit(const BlockStmt *stmt) -> void {
  for (auto *stmt : stmt->stmts) {
    stmt->visit(this);
  }
}

} // namespace lzhcc