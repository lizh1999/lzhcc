#include "lzhcc.h"

namespace lzhcc {

struct ExprGenVisitor : ExprVisitor {
  void visit(const IntegerExpr *expr) override;
  void visit(const FloatingExpr *expr) override;
  void visit(const UnaryExpr *expr) override;
  void visit(const BinaryExpr *expr) override;
  void push();
  void pop(const char *reg);
};

}