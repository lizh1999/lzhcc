#include "lzhcc.h"

namespace lzhcc {

enum class PassKind {
  gp,
  fp,
  sp,
  spsp,
  gpgp,
  gpsp,
  fpfp,
  fpgp,
  gpfp,
  refgp,
  refsp
};
struct Pass {
  PassKind kind;
  int inner0;
  int inner1;
  int inner2;
  int inner3;
  int inner4;
};

class Calling {
public:
  Calling(Context *ctx) : ctx_(ctx) {}
  static auto push(Type *&first, Type *&second, Type *in) -> bool;
  auto dump(RecordType *record, Type *&first, Type *&second, int *offset)
      -> bool;
  auto dump(Type *type, Type *&first, Type *&second, int *offset) -> bool;

  auto stack_bytes() -> int { return sp_ * 8; }
  auto ref_bytes() -> int { return ref_; }
  auto reg_bytes() -> int { return (gp_ + fp_) * 8; }
  auto gp() -> int { return gp_; }
  auto operator()(CallExpr *expr) -> std::vector<Pass>;
  auto operator()(std::span<LValue *> param) -> std::vector<Pass>;

private:
  enum { fp_max = 8, gp_max = 8 };
  static auto is_float(Type *type) -> bool;
  auto fpfp() -> bool;
  auto gpgp() -> bool;
  auto fpgp() -> bool;

  auto floating(Type *type) -> Pass;
  auto integer(Type *type) -> Pass;

  int gp_ = 0;
  int fp_ = 0;
  int sp_ = 0;
  int ref_ = 0;
  Context *ctx_;
};

class Generator {
public:
  Generator(Context *context);

  auto codegen(Function *function) -> void;
  auto codegen(GValue *gvalue) -> void;

private:
  auto store(Type *type) -> void;
  auto store_integer(IntegerType *type) -> void;
  auto store_floating(FloatingType *type) -> void;
  auto store_record(RecordType *type) -> void;
  auto load(Type *type) -> void;
  auto load_integer(IntegerType *type) -> void;
  auto load_floating(FloatingType *type) -> void;

  auto zero_expr(ZeroExpr *expr) -> void;
  auto value_expr(ValueExpr *expr) -> void;
  auto integer_expr(IntegerExpr *expr) -> void;
  auto floating_expr(FloatingExpr *expr) -> void;

  auto cast(Type *src, Type *dest) -> void;
  auto unary_expr(UnaryExpr *expr) -> void;

  auto add(BinaryExpr *expr) -> void;
  auto subtract(BinaryExpr *expr) -> void;
  auto multiply(BinaryExpr *expr) -> void;
  auto divide(BinaryExpr *expr) -> void;
  auto modulo(Type *type) -> void;
  auto shift_left(Type *type) -> void;
  auto shift_right(Type *type) -> void;
  auto less_than(BinaryExpr *expr) -> void;
  auto less_equal(BinaryExpr *expr) -> void;
  auto equal(BinaryExpr *expr) -> void;
  auto visit(BinaryExpr *expr) -> void;
  auto visitf(BinaryExpr *expr) -> void;

  auto negative(UnaryExpr *expr) -> void;

  auto binary_expr(BinaryExpr *expr) -> void;
  auto call_expr(CallExpr *expr) -> void;
  auto stmt_expr(StmtExpr *expr) -> void;
  auto member_expr(MemberExpr *expr) -> void;
  auto condition_expr(ConditionExpr *expr) -> void;
  auto expr_proxy(Expr *expr) -> void;

  auto value_addr(ValueExpr *expr) -> void;
  auto unary_addr(UnaryExpr *expr) -> void;
  auto binary_addr(BinaryExpr *expr) -> void;
  auto member_addr(MemberExpr *expr) -> void;
  auto addr_proxy(Expr *expr) -> void;

  auto cmp_zero(Type *type) -> void;

  auto expr_stmt(ExprStmt *stmt) -> void;
  auto for_stmt(ForStmt *stmt) -> void;
  auto if_stmt(IfStmt *stmt) -> void;
  auto return_stmt(ReturnStmt *stmt) -> void;
  auto block_stmt(BlockStmt *stmt) -> void;
  auto goto_stmt(GotoStmt *stmt) -> void;
  auto label_stmt(LabelStmt *stmt) -> void;
  auto switch_stmt(SwitchStmt *stmt) -> void;
  auto case_stmt(CaseStmt *stmt) -> void;
  auto default_stmt(DefaultStmt *stmt) -> void;
  auto do_stmt(DoStmt *stmt) -> void;
  auto stmt_proxy(Stmt *stmt) -> void;

  auto push(const char *reg) -> void;
  auto pop(const char *reg) -> void;
  auto pushf(const char *reg) -> void;
  auto popf(const char *reg) -> void;

  auto pop(int reg) -> void;
  auto popf(int reg) -> void;

  [[gnu::format(printf, 2, 3)]] auto println(const char *, ...) -> void;

  [[noreturn]] auto expect_lvalue() -> void { std::abort(); }

  int depth_;
  int counter_;
  int return_label_;

  FILE *out_;
  Context *context_;
};

} // namespace lzhcc