#include "lzhcc.h"

namespace lzhcc {

struct RValueVisitor : ExprVisitor {
  void visit(const VarRefExpr *expr) override;
  void visit(const IntegerExpr *expr) override;
  void visit(const FloatingExpr *expr) override;
  void visit(const UnaryExpr *expr) override;
  void visit(const BinaryExpr *expr) override;
  void push();
  void pop(const char *reg);
};

struct LValueVisitor : ExprVisitor {
  void visit(const VarRefExpr *expr) override;
  void visit(const IntegerExpr *expr) override { expect_lvalue(); }
  void visit(const FloatingExpr *expr) override { expect_lvalue(); }
  void visit(const UnaryExpr *expr) override { expect_lvalue(); }
  void visit(const BinaryExpr *expr) override { expect_lvalue(); }
  void expect_lvalue() { std::abort(); }
};

struct StmtGenVisitor : StmtVisitor {
  void visit(const ExpressionStmt *stmt) override;
  void visit(const ReturnStmt *stmt) override;
  void visit(const BlockStmt *stmt) override;
};

} // namespace lzhcc