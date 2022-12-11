#include "lzhcc_codegen.h"

namespace lzhcc {

auto StmtGenVisitor::visit(const ExpressionStmt *stmt) -> void {
  stmt->expr->visit(&expr_visitor);
}

}