#include "lzhcc_codegen.h"

namespace lzhcc {

auto Generator::expr_stmt(ExprStmt *stmt) -> void { expr_proxy(stmt->expr); }

auto Generator::for_stmt(ForStmt *stmt) -> void {
  if (stmt->init) {
    stmt_proxy(stmt->init);
  }
  int label = counter++;
  println(".L.begin.%d:", label);
  if (stmt->cond) {
    expr_proxy(stmt->cond);
    println("  beqz a0, .L.end.%d", label);
  }
  stmt_proxy(stmt->then);
  if (stmt->inc) {
    expr_proxy(stmt->inc);
  }
  println("  j .L.begin.%d", label);
  println(".L.end.%d:", label);
}

auto Generator::if_stmt(IfStmt *stmt) -> void {
  expr_proxy(stmt->cond);
  int label = counter++;
  println("  beqz a0, .L.else.%d", label);
  stmt_proxy(stmt->then);
  println("  j .L.end.%d", label);
  println(".L.else.%d:", label);
  if (stmt->else_) {
    stmt_proxy(stmt->else_);
  }
  println(".L.end.%d:", label);
}

auto Generator::return_stmt(ReturnStmt *stmt) -> void {
  expr_proxy(stmt->expr);
  println("  j .L.return.%d", return_label);
}

auto Generator::block_stmt(BlockStmt *stmt) -> void {
  for (auto *stmt : stmt->stmts) {
    stmt_proxy(stmt);
  }
}

auto Generator::stmt_proxy(Stmt *stmt) -> void {
  switch (stmt->kind) {
  case StmtKind::empty:
    return;
  case StmtKind::expr:
    return expr_stmt(cast<ExprStmt>(stmt));
  case StmtKind::kw_for:
    return for_stmt(cast<ForStmt>(stmt));
  case StmtKind::kw_if:
    return if_stmt(cast<IfStmt>(stmt));
  case StmtKind::kw_return:
    return return_stmt(cast<ReturnStmt>(stmt));
  case StmtKind::block:
    return block_stmt(cast<BlockStmt>(stmt));
  }
}

} // namespace lzhcc