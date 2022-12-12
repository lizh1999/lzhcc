#include "lzhcc.h"

namespace lzhcc {

auto VarRefExpr::visit(ExprVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto VarRefExpr::type() const -> const Type * { return var->type; }

auto IntegerExpr::visit(ExprVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto IntegerExpr::type() const -> const Type * { return type_; }

auto UnaryExpr::visit(ExprVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto UnaryExpr::type() const -> const Type * { return type_; }

auto BinaryExpr::visit(ExprVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto BinaryExpr::type() const -> const Type * { return type_; }

auto EmptyStmt::visit(StmtVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto ExpressionStmt::visit(StmtVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto ForStmt::visit(StmtVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto IfStmt::visit(StmtVisitor *visitor) const -> void { visitor->visit(this); }

auto ReturnStmt::visit(StmtVisitor *visitor) const -> void {
  visitor->visit(this);
}

auto BlockStmt::visit(StmtVisitor *visitor) const -> void {
  visitor->visit(this);
}

} // namespace lzhcc