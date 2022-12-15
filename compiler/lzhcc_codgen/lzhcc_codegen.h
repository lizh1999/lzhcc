#include "lzhcc.h"

namespace lzhcc {

class Generator {
public:
  Generator();

  auto codegen(Function *function) -> void;

private:
  auto store(Type *type) -> void;
  auto store_integer(IntegerType *type) -> void;
  auto load(Type *type) -> void;
  auto load_integer(IntegerType *type) -> void;
  auto value_expr(ValueExpr *expr) -> void;
  auto integer_expr(IntegerExpr *expr) -> void;
  auto unary_expr(UnaryExpr *expr) -> void;
  auto binary_expr(BinaryExpr *expr) -> void;
  auto call_expr(CallExpr *expr) -> void;
  auto expr_proxy(Expr *expr) -> void;

  auto value_addr(ValueExpr *expr) -> void;
  auto unary_addr(UnaryExpr *expr) -> void;
  auto addr_proxy(Expr *expr) -> void;

  auto expr_stmt(ExprStmt *stmt) -> void;
  auto for_stmt(ForStmt *stmt) -> void;
  auto if_stmt(IfStmt *stmt) -> void;
  auto return_stmt(ReturnStmt *stmt) -> void;
  auto block_stmt(BlockStmt *stmt) -> void;
  auto stmt_proxy(Stmt *stmt) -> void;

  auto push(const char *reg) -> void;
  auto pop(const char *reg) -> void;

  [[noreturn]] auto expect_lvalue() -> void { std::abort(); }

  int counter;
  int return_label;
};

} // namespace lzhcc