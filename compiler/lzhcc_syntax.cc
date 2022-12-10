#include "lzhcc.h"

namespace lzhcc {

auto IntegerType::visit(TypeVisitor *visitor) const -> void { visitor->visit(this); }
auto FloatingType::visit(TypeVisitor *visitor) const -> void { visitor->visit(this); }
auto IntegerExpr::visit(ExprVisitor *visitor) const -> void { visitor->visit(this); }
auto FloatingExpr::visit(ExprVisitor *visitor) const -> void { visitor->visit(this); }
auto UnaryExpr::visit(ExprVisitor *visitor) const -> void { visitor->visit(this); }
auto BinaryExpr::visit(ExprVisitor *visitor) const -> void { visitor->visit(this); }
auto Expression::visit(AstVisitor *visitor) const -> void { visitor->visit(this); }

} // namespace lzhcc