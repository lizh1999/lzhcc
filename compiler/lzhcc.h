#pragma once

#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lzhcc {

inline auto align_to(int x, int y) -> int { return (x + y - 1) / y * y; }

//
// lzhcc_diagnostic.cc
//

struct Diagnostic {
  std::string_view filename;
  std::string_view line;
  int line_number;
  int column_number;
};

[[noreturn]] auto fatal(Diagnostic loc, const char *message) -> void;

class Context;

enum class TokenKind : uint8_t {
  amp,                   // "&"
  amp_amp,               // "&&"
  amp_equal,             // "&="
  arrow,                 // "->"
  caret,                 // "^"
  caret_equal,           // "^="
  comma,                 // ","
  colon,                 // ":"
  dot,                   // "."
  dotdotdot,             // "..."
  equal,                 // "="
  equal_equal,           // "=="
  exclaim,               // "!"
  exclaim_equal,         // "!="
  greater,               // ">"
  greater_equal,         // ">="
  greater_greater,       // ">>"
  greater_greater_equal, // ">>="
  less,                  // "<"
  less_equal,            // "<="
  less_less,             // "<<"
  less_less_equal,       // "<<="
  minus,                 // "-"
  minus_equal,           // "-="
  minus_minus,           // "--"
  percent,               // "%"
  percent_equal,         // "%="
  pipe,                  // "|"
  pipe_equal,            // "|="
  pipe_pipe,             // "||"
  plus,                  // "+"
  plus_equal,            // "+="
  plus_plus,             // "++"
  question,              // "?"
  semi,                  // ";"
  slash,                 // "/"
  slash_equal,           // "/="
  star,                  // "*"
  star_equal,            // "*="
  tilde,                 // "~"
  open_paren,            // "("
  close_paren,           // ")"
  open_bracket,          // "["
  close_bracket,         // "]"
  open_brace,            // "{"
  close_brace,           // "}"
  string,                // string literal
  character,             // character literal
  numeric,               // numeric literal
  identifier,            // identifier
  eof,                   // eof
  kw_alignas,            // "_Alignas"
  kw_alignof,            // "_Alignof"
  kw_auto,               // "auto"
  kw_bool,               // "_Bool"
  kw_break,              // "break"
  kw_case,               // "case"
  kw_char,               // "char"
  kw_const,              // "const"
  kw_continue,           // "continue"
  kw_default,            // "default"
  kw_do,                 // "do"
  kw_double,             // "double"
  kw_else,               // "else"
  kw_enum,               // "enum"
  kw_extern,             // "extern"
  kw_float,              // "float"
  kw_for,                // "for"
  kw_goto,               // "goto"
  kw_if,                 // "if"
  kw_int,                // "int"
  kw_long,               // "long"
  kw_noreturn,           // "_Noreturn"
  kw_register,           // "register"
  kw_restrict,           // "restrict"
  kw_return,             // "return"
  kw_short,              // "short"
  kw_signed,             // "signed"
  kw_sizeof,             // "sizeof"
  kw_static,             // "static"
  kw_struct,             // "struct"
  kw_switch,             // "switch"
  kw_typedef,            // "typedef"
  kw_union,              // "union"
  kw_unsigned,           // "unsigned"
  kw_void,               // "void"
  kw_volatile,           // "volatile"
  kw_while,              // "while"
};

struct Token {
  TokenKind kind;
  bool leading_space;
  bool start_of_line;
  bool expand_disable;
  int location;
  int inner;
};

//
// lzhcc_lex/lzhcc_lex.cc
//

using CharCursorFn = std::function<std::pair<char, int>()>;
auto lex(CharCursorFn chars, Context &context) -> std::vector<Token>;

enum class TypeKind {
  kw_void,
  boolean,
  integer,
  floating,
  pointer,
  function,
  array,
  record,
};

struct Type {
  Type(const TypeKind kind) : kind(kind) {}
  const TypeKind kind;
};

struct VoidType : Type {
  VoidType() : Type(TypeKind::kw_void) {}
};

struct BoolType : Type {
  BoolType() : Type(TypeKind::boolean) {}
};

enum class IntegerKind {
  byte = 1,
  half = 2,
  word = 4,
  dword = 8,
};

enum class Sign {
  sign,
  unsign,
};

enum class Scalar {
  int8,
  int16,
  int32,
  int64,
  uint8,
  uint16,
  uint32,
  uint64,
};

inline constexpr auto pattern(IntegerKind kind, Sign sign) -> Scalar {
  using enum Scalar;
  switch (kind) {
  case IntegerKind::byte:
    return sign == Sign::sign ? int8 : uint8;
  case IntegerKind::half:
    return sign == Sign::sign ? int16 : uint16;
  case IntegerKind::word:
    return sign == Sign::sign ? int32 : uint32;
  case IntegerKind::dword:
    return sign == Sign::sign ? int64 : uint64;
  }
}

struct IntegerType : Type {
  IntegerType(IntegerKind kind, Sign sign)
      : Type(TypeKind::integer), kind(kind), sign(sign) {}
  IntegerKind kind;
  Sign sign;
};

enum class FloatingKind {
  float32 = 4,
  float64 = 8,
};

struct FloatingType : Type {
  FloatingType(FloatingKind kind) : Type(TypeKind::floating), kind(kind) {}
  FloatingKind kind;
};

struct PointerType : Type {
  PointerType(Type *base) : Type(TypeKind::pointer), base(base) {}
  Type *base;
};

struct FunctionType : Type {
  FunctionType(Type *ret, std::vector<Type *> params, bool is_variadic)
      : Type(TypeKind::function), ret(ret), params(params),
        is_variadic(is_variadic) {}
  Type *ret;
  std::vector<Type *> params;
  bool is_variadic;
};

struct ArrayType : Type {
  ArrayType(Type *base, int length)
      : Type(TypeKind::array), base(base), length(length) {}
  Type *base;
  int length;
};

struct Member {
  Type *type;
  int name;
  int offset;
};

struct RecordType : Type {
  RecordType(std::vector<Member> members, int size_bytes, int align_bytes)
      : Type(TypeKind::record), members(std::move(members)),
        size_bytes(size_bytes), align_bytes(align_bytes) {}
  static auto dummy() -> RecordType { return RecordType({}, -1, -1); }
  auto is_dummy() -> bool {
    return members.empty() && size_bytes == -1 && align_bytes == -1;
  }

  std::vector<Member> members;
  int size_bytes;
  int align_bytes;
};

enum class ValueKind {
  local,
  global,
  function,
  declaraion,
};

struct Value {
  Value(ValueKind kind, Type *type) : kind(kind), type(type) {}
  const ValueKind kind;
  Type *type;
};

struct LValue : Value {
  LValue(Type *type, int offset)
      : Value(ValueKind::local, type), offset(offset) {}
  int offset;
};

struct Relocation {
  int64_t index;
  std::string_view name;
  int64_t offset;
};

enum class Linkage {
  external,
  internal,
};

struct GValue : Value {
  GValue(Type *type, std::string_view name, uint8_t *init,
         std::vector<Relocation> relocations, int align_bytes, Linkage linkage)
      : Value(ValueKind::global, type), name(name), init(init),
        relocations(std::move(relocations)), align_bytes(align_bytes),
        linkage(linkage) {}
  std::string_view name;
  uint8_t *init;
  std::vector<Relocation> relocations;
  int align_bytes;
  Linkage linkage;
};

struct Function : Value {
  Function(Type *type, std::string_view name, int stack_size, struct Stmt *stmt,
           std::vector<LValue *> params, LValue *va_area, Linkage linkage)
      : Value(ValueKind::function, type), name(name), stack_size(stack_size),
        stmt(stmt), params(std::move(params)), va_area(va_area),
        linkage(linkage) {}
  std::string_view name;
  int stack_size;
  struct Stmt *stmt;
  std::vector<LValue *> params;
  LValue *va_area;
  Linkage linkage;
};

struct Declaration : Value {
  Declaration(Type *type, std::string_view name)
      : Value(ValueKind::declaraion, type), name(name) {}
  std::string_view name;
};

enum class ExperKind {
  zero,
  value,
  integer,
  floating,
  unary,
  binary,
  call,
  stmt,
  member,
  condition,
};

struct Expr {
  Expr(ExperKind kind, Type *type) : kind(kind), type(type) {}
  const ExperKind kind;
  Type *type;
};

struct ZeroExpr : Expr {
  ZeroExpr(Type *type, Expr *expr, int64_t size)
      : Expr(ExperKind::zero, type), expr(expr), size(size) {}
  Expr *expr;
  int64_t size;
};

struct ValueExpr : Expr {
  ValueExpr(Value *value) : Expr(ExperKind::value, value->type), value(value) {}
  Value *value;
};

struct IntegerExpr : Expr {
  IntegerExpr(Type *type, int64_t value)
      : Expr(ExperKind::integer, type), value(value) {}
  int64_t value;
};

struct FloatingExpr : Expr {
  FloatingExpr(Type *type, double value)
      : Expr(ExperKind::floating, type), value(value) {}
  double value;
};

struct StmtExpr : Expr {
  StmtExpr(Type *type, struct BlockStmt *stmt)
      : Expr(ExperKind::stmt, type), stmt(stmt) {}
  struct BlockStmt *stmt;
};

enum class UnaryKind {
  negative,
  refrence,
  deref,
  cast,
  logical_not,
  bitwise_not,
};

struct UnaryExpr : Expr {
  UnaryExpr(UnaryKind kind, Type *type, Expr *operand)
      : Expr(ExperKind::unary, type), kind(kind), operand(operand) {}
  UnaryKind kind;
  Expr *operand;
};

enum class BinaryKind {
  add,
  subtract,
  multiply,
  divide,
  modulo,
  less_than,
  less_equal,
  equal,
  not_equal,
  assign,
  comma,
  bitwise_or,
  bitwise_xor,
  bitwise_and,
  logical_and,
  logical_or,
  shift_left,
  shift_right,
};

struct BinaryExpr : Expr {
  BinaryExpr(BinaryKind kind, Type *type, Expr *lhs, Expr *rhs)
      : Expr(ExperKind::binary, type), kind(kind), lhs(lhs), rhs(rhs) {}
  BinaryKind kind;
  Expr *lhs;
  Expr *rhs;
};

struct CallExpr : Expr {
  CallExpr(Type *type, Expr *func, std::vector<Expr *> args, int arg_num)
      : Expr(ExperKind::call, type), func(func), args(std::move(args)),
        arg_num(arg_num) {}
  Expr *func;
  std::vector<Expr *> args;
  int arg_num;
};

struct MemberExpr : Expr {
  MemberExpr(Type *type, Expr *record, int offset)
      : Expr(ExperKind::member, type), record(record), offset(offset) {}
  Expr *record;
  int offset;
};

struct ConditionExpr : Expr {
  ConditionExpr(Type *type, Expr *cond, Expr *then, Expr *else_)
      : Expr(ExperKind::condition, type), cond(cond), then(then), else_(else_) {
  }
  Expr *cond;
  Expr *then;
  Expr *else_;
};

enum class StmtKind {
  empty,
  expr,
  kw_for,
  kw_if,
  kw_return,
  block,
  kw_goto,
  label,
  kw_switch,
  kw_case,
  kw_default,
  kw_do,
};

struct Stmt {
  Stmt(StmtKind kind) : kind(kind) {}
  const StmtKind kind;
};

struct EmptyStmt : Stmt {
  EmptyStmt() : Stmt(StmtKind::empty) {}
};

struct ExprStmt : Stmt {
  ExprStmt(Expr *expr) : Stmt(StmtKind::expr), expr(expr) {}
  Expr *expr;
};

struct Label {
  std::string_view name;
};

struct ForStmt : Stmt {
  ForStmt(Stmt *init, Expr *cond, Expr *inc, Stmt *then, Label *continue_label,
          Label *break_label)
      : Stmt(StmtKind::kw_for), init(init), cond(cond), inc(inc), then(then),
        continue_label(continue_label), break_label(break_label) {}
  Stmt *init;
  Expr *cond;
  Expr *inc;
  Stmt *then;
  Label *continue_label;
  Label *break_label;
};

struct IfStmt : Stmt {
  IfStmt(Expr *cond, Stmt *then, Stmt *else_)
      : Stmt(StmtKind::kw_if), cond(cond), then(then), else_(else_) {}
  Expr *cond;
  Stmt *then;
  Stmt *else_;
};

struct ReturnStmt : Stmt {
  ReturnStmt(Expr *expr) : Stmt(StmtKind::kw_return), expr(expr) {}
  Expr *expr;
};

struct BlockStmt : Stmt {
  BlockStmt(std::vector<Stmt *> stmts)
      : Stmt(StmtKind::block), stmts(std::move(stmts)) {}
  std::vector<Stmt *> stmts;
};

struct GotoStmt : Stmt {
  GotoStmt(Label *label) : Stmt(StmtKind::kw_goto), label(label) {}
  Label *label;
};

struct LabelStmt : Stmt {
  LabelStmt(Label *label) : Stmt(StmtKind::label), label(label) {}
  Label *label;
};

struct CaseStmt : Stmt {
  CaseStmt(Stmt *stmt, int64_t value, Label *label)
      : Stmt(StmtKind::kw_case), stmt(stmt), value(value), label(label) {}
  Stmt *stmt;
  int64_t value;
  Label *label;
};

struct SwitchStmt : Stmt {
  SwitchStmt(Expr *expr, Label *break_label)
      : Stmt(StmtKind::kw_switch), expr(expr), stmt(nullptr), case_lables(),
        default_label(nullptr), break_label(break_label) {}
  Expr *expr;
  Stmt *stmt;
  std::vector<CaseStmt *> case_lables;
  Label *default_label;
  Label *break_label;
};

struct DefaultStmt : Stmt {
  DefaultStmt(Stmt *stmt, Label *label)
      : Stmt(StmtKind::kw_default), stmt(stmt), label(label) {}
  Stmt *stmt;
  Label *label;
};

struct DoStmt : Stmt {
  DoStmt(Stmt *then, Expr *cond, Label *continue_label, Label *break_label)
      : Stmt(StmtKind::kw_do), then(then), cond(cond),
        continue_label(continue_label), break_label(break_label) {}
  Stmt *then;
  Expr *cond;
  Label *continue_label;
  Label *break_label;
};

enum class InitKind {
  array,
  record,
  scalar,
};

struct Init {
  Init(InitKind kind) : kind(kind) {}
  const InitKind kind;
};

struct ArrayInit : Init {
  ArrayInit(std::vector<Init *> children, Type *base)
      : Init(InitKind::array), children(std::move(children)), base(base) {}
  std::vector<Init *> children;
  Type *base;
};

struct RecordInit : Init {
  using Children = std::vector<std::pair<Member *, Init *>>;
  RecordInit(Children children)
      : Init(InitKind::record), children(std::move(children)) {}
  Children children;
};

struct ScalarInit : Init {
  ScalarInit(Expr *expr) : Init(InitKind::scalar), expr(expr) {}
  Expr *expr;
};

template <class T, class U> auto cast(U *origin) -> T * {
  return reinterpret_cast<T *>(origin);
}

//
// lzhcc_parse/lzhcc_parse.cc
//

struct Module {
  std::vector<GValue *> gvalues;
  std::vector<Function *> functions;
};

auto parse(std::span<Token> tokens, Context &context) -> Module;

//
// lzhcc_codgen/lzhcc_codgen.cc
//

auto codegen(Module &module, Context &context) -> void;

//
// lzhcc_context.cc
//

class Context {
public:
  Context();
  ~Context();
  auto append_file(std::string path) -> CharCursorFn;
  auto append_text(std::string text) -> CharCursorFn;
  auto push_literal(std::string literal) -> int;
  auto push_identifier(std::string literal) -> int;
  auto storage(int index) const -> std::string_view;
  auto into_keyword(int index) const -> TokenKind;

  // type
  auto void_type() -> Type *;
  auto boolean() -> Type *;
  auto int8() -> Type *;
  auto int16() -> Type *;
  auto int32() -> Type *;
  auto int64() -> Type *;

  auto uint8() -> Type *;
  auto uint16() -> Type *;
  auto uint32() -> Type *;
  auto uint64() -> Type *;

  auto float32() -> Type *;
  auto float64() -> Type *;

  auto pointer_to(Type *base) -> Type *;
  auto array_of(Type *base, int length) -> Type *;
  auto function_type(Type *ret, std::vector<Type *> params, bool) -> Type *;
  auto record_type() -> RecordType *;
  auto size_of(Type *type) -> int;
  auto align_of(Type *type) -> int;

  // value
  auto create_declaration(Type *type, std::string_view name) -> Declaration *;
  auto create_local(Type *type, int offset) -> LValue *;
  auto create_global(Type *type, std::string_view name, uint8_t *init,
                     std::vector<Relocation> relocations, int align_bytes,
                     Linkage linkage) -> GValue *;
  auto create_function(Type *type, std::string_view name, int stack_size,
                       Stmt *stmt, std::vector<LValue *> params,
                       LValue *va_area, Linkage linkage) -> Function *;

  // label
  auto create_label(std::string_view name = "") -> Label *;

  // expr
  auto zero(Expr *expr, int64_t size) -> Expr *;
  auto value(Value *value) -> Expr *;
  auto integer(int8_t value) -> Expr *;
  auto integer(int32_t value) -> Expr *;
  auto integer(int64_t value) -> Expr *;
  auto floating(Type *type, double value) -> Expr *;
  auto integer(Type *type, int64_t value) -> Expr *;

  auto negative(Type *type, Expr *operand) -> Expr *;
  auto refrence(Type *type, Expr *operand) -> Expr *;
  auto deref(Type *type, Expr *operand) -> Expr *;
  auto cast(Type *type, Expr *operand) -> Expr *;
  auto logical_not(Type *type, Expr *operand) -> Expr *;
  auto bitwise_not(Type *type, Expr *operand) -> Expr *;
  auto add(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto subtract(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto multiply(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto divide(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto modulo(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto less_than(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto less_equal(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto equal(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto not_equal(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto bitwise_or(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto bitwise_xor(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto bitwise_and(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto logical_and(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto logical_or(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto shift_left(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto shift_right(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto stmt_expr(Type *type, BlockStmt *stmt) -> Expr *;
  auto assign(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto comma(Type *type, Expr *lhs, Expr *rhs) -> Expr *;
  auto condition(Type *type, Expr *cond, Expr *then, Expr *else_) -> Expr *;

  auto call(Type *type, Expr *func, std::vector<Expr *> args, int arg_num)
      -> Expr *;

  auto member(Type *type, Expr *record, int offset) -> Expr *;

  // stmt
  auto empty_stmt() -> Stmt *;
  auto expr_stmt(Expr *expr) -> Stmt *;
  auto for_stmt(Stmt *init, Expr *cond, Expr *inc, Stmt *then,
                Label *continue_label, Label *break_label) -> Stmt *;
  auto if_stmt(Expr *cond, Stmt *then, Stmt *else_) -> Stmt *;
  auto return_stmt(Expr *expr) -> Stmt *;
  auto block_stmt(std::vector<Stmt *> stmts) -> Stmt *;
  auto goto_stmt(Label *label) -> Stmt *;
  auto label_stmt(Label *label) -> Stmt *;
  auto switch_stmt(Expr *expr, Label *break_label) -> SwitchStmt *;
  auto case_stmt(Stmt *stmt, int64_t value, Label *label) -> CaseStmt *;
  auto default_stmt(Stmt *stmt, Label *label) -> Stmt *;
  auto do_stmt(Stmt *stmt, Expr *cond, Label *continue_label,
               Label *break_label) -> Stmt *;

  // init
  auto array_init(std::vector<Init *> children, Type *base) -> Init *;
  auto record_init(RecordInit::Children children) -> Init *;
  auto scalar_init(Expr *expr) -> Init *;

  auto create_tmpfile() -> std::string;

  [[noreturn, gnu::format(printf, 3, 4)]] void fatal(int, const char *, ...);

  struct {
    const char *input = nullptr;
    const char *opt_o = nullptr;
    bool opt_cc1 = false;
    bool opt_hash_hash_hash = false;
    bool opt_S = false;
  } arg;

private:
  std::deque<std::string> storage_;
  std::deque<std::string> text_;
  std::unordered_map<std::string_view, int> identifier_map_;
  std::vector<TokenKind> keyword_map_;
  std::vector<std::string> tmpfile_;

  template <typename T, typename... Args> auto create(Args &&...args) -> T * {
    auto smart_ptr =
        std::make_unique<ArenaEntryTyped<T>>(std::forward<Args>(args)...);
    T *ptr = smart_ptr->instance();
    arena_.push_back(std::move(smart_ptr));
    return ptr;
  }

  struct ArenaEntry {
    virtual ~ArenaEntry() = default;
  };

  template <typename T> struct ArenaEntryTyped : ArenaEntry {
    template <typename... Args>
    ArenaEntryTyped(Args &&...args) : instance_(std::forward<Args>(args)...) {}
    auto instance() -> T * { return &instance_; }

  private:
    T instance_;
  };
  std::vector<std::unique_ptr<ArenaEntry>> arena_;
};

//
// lzhcc_driver.cc
//

auto main(std::span<char *> args) -> int;

} // namespace lzhcc