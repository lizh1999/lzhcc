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
  kw_bool,               // "_Bool"
  kw_break,              // "break"
  kw_case,               // "case"
  kw_char,               // "char"
  kw_continue,           // "continue"
  kw_default,            // "default"
  kw_else,               // "else"
  kw_enum,               // "enum"
  kw_for,                // "for"
  kw_goto,               // "goto"
  kw_if,                 // "if"
  kw_int,                // "int"
  kw_long,               // "long"
  kw_return,             // "return"
  kw_short,              // "short"
  kw_sizeof,             // "sizeof"
  kw_static,             // "static"
  kw_struct,             // "struct"
  kw_switch,             // "switch"
  kw_typedef,            // "typedef"
  kw_union,              // "union"
  kw_void,               // "void"
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

struct IntegerType : Type {
  IntegerType(IntegerKind kind, bool is_unsigned)
      : Type(TypeKind::integer), kind(kind), is_unsigned(is_unsigned) {}
  IntegerKind kind;
  bool is_unsigned;
};

struct PointerType : Type {
  PointerType(Type *base) : Type(TypeKind::pointer), base(base) {}
  Type *base;
};

struct FunctionType : Type {
  FunctionType(Type *ret, std::vector<Type *> params)
      : Type(TypeKind::function), ret(ret), params(params) {}
  Type *ret;
  std::vector<Type *> params;
};

struct ArrayType : Type {
  ArrayType(Type *base, int length)
      : Type(TypeKind::array), base(base), length(length) {}
  Type *base;
  int length;
};

struct Member {
  Type *type;
  int offset;
};

struct RecordType : Type {
  RecordType(std::unordered_map<int, Member> member_map, int size_bytes,
             int align_bytes)
      : Type(TypeKind::record), member_map(std::move(member_map)),
        size_bytes(size_bytes), align_bytes(align_bytes) {}
  static auto dummy() -> RecordType { return RecordType({}, -1, -1); }
  auto is_dummy() -> bool {
    return member_map.empty() && size_bytes == -1 && align_bytes == -1;
  }

  std::unordered_map<int, Member> member_map;
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

struct GValue : Value {
  GValue(Type *type, std::string_view name, uint8_t *init)
      : Value(ValueKind::global, type), name(name), init(init) {}
  std::string_view name;
  uint8_t *init;
};

enum class Linkage {
  external,
  internal,
};

struct Function : Value {
  Function(Type *type, std::string_view name, int stack_size, struct Stmt *stmt,
           std::vector<LValue *> params, Linkage linkage)
      : Value(ValueKind::function, type), name(name), stack_size(stack_size),
        stmt(stmt), params(std::move(params)), linkage(linkage) {}
  std::string_view name;
  int stack_size;
  struct Stmt *stmt;
  std::vector<LValue *> params;
  Linkage linkage;
};

struct Declaration : Value {
  Declaration(Type *type, std::string_view name)
      : Value(ValueKind::declaraion, type), name(name) {}
  std::string_view name;
};

enum class ExperKind {
  value,
  integer,
  unary,
  binary,
  call,
  stmt,
  member,
};

struct Expr {
  Expr(ExperKind kind, Type *type) : kind(kind), type(type) {}
  const ExperKind kind;
  Type *type;
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
  CallExpr(std::string_view name, Type *type, std::vector<Expr *> argus)
      : Expr(ExperKind::call, type), name(name), args(std::move(argus)) {}
  std::string_view name;
  std::vector<Expr *> args;
};

struct MemberExpr : Expr {
  MemberExpr(Type *type, Expr *record, int offset)
      : Expr(ExperKind::member, type), record(record), offset(offset) {}
  Expr *record;
  int offset;
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
  auto pointer_to(Type *base) -> Type *;
  auto array_of(Type *base, int length) -> Type *;
  auto function_type(Type *ret, std::vector<Type *> params) -> Type *;
  auto record_type() -> RecordType *;
  auto size_of(Type *type) -> int;
  auto align_of(Type *type) -> int;

  // value
  auto create_declaration(Type *type, std::string_view name) -> Declaration *;
  auto create_local(Type *type, int offset) -> LValue *;
  auto create_global(Type *type, std::string_view name, uint8_t *init)
      -> GValue *;
  auto create_function(Type *type, std::string_view name, int stack_size,
                       Stmt *stmt, std::vector<LValue *> params,
                       Linkage linkage) -> Function *;

  // label
  auto create_label(std::string_view name = "") -> Label *;

  // expr
  auto value(Value *value) -> Expr *;
  auto integer(int8_t value) -> Expr *;
  auto integer(int32_t value) -> Expr *;
  auto integer(int64_t value) -> Expr *;
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

  auto call(std::string_view name, Type *type, std::vector<Expr *> args)
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

  [[noreturn, gnu::format(printf, 3, 4)]] void fatal(int, const char *, ...);

  struct {
    const char *opt_o = nullptr;
  } arg;

private:
  std::deque<std::string> storage_;
  std::deque<std::string> text_;
  std::unordered_map<std::string_view, int> identifier_map_;
  std::vector<TokenKind> keyword_map_;

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

auto main(std::span<const char *> args) -> int;

} // namespace lzhcc