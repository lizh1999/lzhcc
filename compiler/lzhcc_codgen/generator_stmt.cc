#include "lzhcc_codegen.h"

namespace lzhcc {

auto Generator::expr_stmt(ExprStmt *stmt) -> void { expr_proxy(stmt->expr); }

auto Generator::for_stmt(ForStmt *stmt) -> void {
  if (stmt->init) {
    stmt_proxy(stmt->init);
  }
  int label = counter++;
  auto break_name = stmt->break_label->name;
  auto continue_name = stmt->continue_label->name;
  println(".L.begin.%d:", label);
  if (stmt->cond) {
    expr_proxy(stmt->cond);
    println("  beqz a0, %.*s", (int)break_name.size(), break_name.data());
  }
  stmt_proxy(stmt->then);
  println("%.*s:", (int)continue_name.size(), continue_name.data());
  if (stmt->inc) {
    expr_proxy(stmt->inc);
  }
  println("  j .L.begin.%d", label);
  println("%.*s:", (int)break_name.size(), break_name.data());
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

auto Generator::goto_stmt(GotoStmt *stmt) -> void {
  auto name = stmt->label->name;
  println("  j %.*s", (int)name.size(), name.data());
}

auto Generator::label_stmt(LabelStmt *stmt) -> void {
  auto name = stmt->label->name;
  println("%.*s:", (int)name.size(), name.data());
}

auto Generator::switch_stmt(SwitchStmt *stmt) -> void {
  expr_proxy(stmt->expr);
  for (auto *case_stmt : stmt->case_lables) {
    println("  li a1, %ld", case_stmt->value);
    auto name = case_stmt->label->name;
    println("  beq a0, a1, %.*s", (int)name.size(), name.data());
  }
  if (stmt->default_label) {
    auto name = stmt->default_label->name;
    println("  j %.*s", (int)name.size(), name.data());
  } else {
    auto name = stmt->break_label->name;
    println("  j %.*s", (int)name.size(), name.data());
  }
  stmt_proxy(stmt->stmt);
  auto name = stmt->break_label->name;
  println("%.*s:", (int)name.size(), name.data());
}

auto Generator::case_stmt(CaseStmt *stmt) -> void {
  auto name = stmt->label->name;
  println("%.*s:", (int)name.size(), name.data());
  stmt_proxy(stmt->stmt);
}

auto Generator::default_stmt(DefaultStmt *stmt) -> void {
  auto name = stmt->label->name;
  println("%.*s:", (int)name.size(), name.data());
  stmt_proxy(stmt->stmt);
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
  case StmtKind::kw_goto:
    return goto_stmt(cast<GotoStmt>(stmt));
  case StmtKind::label:
    return label_stmt(cast<LabelStmt>(stmt));
  case StmtKind::kw_switch:
    return switch_stmt(cast<SwitchStmt>(stmt));
  case StmtKind::kw_case:
    return case_stmt(cast<CaseStmt>(stmt));
  case StmtKind::kw_default:
    return default_stmt(cast<DefaultStmt>(stmt));
  }
}

} // namespace lzhcc