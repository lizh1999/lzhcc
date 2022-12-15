#pragma once

#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace lzhcc {

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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
  amp,           // "&"
  comma,         // ","
  equal,         // "="
  equal_equal,   // "=="
  exclaim,       // "!"
  exclaim_equal, // "!="
  greater,       // ">"
  greater_equal, // ">="
  less,          // "<"
  less_equal,    // "<="
  minus,         // "-"
  plus,          // "+"
  semi,          // ";"
  slash,         // "/"
  star,          // "*"
  open_paren,    // "("
  close_paren,   // ")"
  open_bracket,  // "["
  close_bracket, // "]"
  open_brace,    // "{"
  close_brace,   // "}"
  numeric,       // numeric value
  identifier,    // identifier
  eof,           // eof
  kw_char,       // "char"
  kw_else,       // "else"
  kw_for,        // "for"
  kw_if,         // "if"
  kw_int,        // "int"
  kw_return,     // "return"
  kw_sizeof,     // "sizeof"
  kw_while,      // "while"
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

//
// lzhcc_syntax.cc
//

using Type = std::variant<struct IntegerType, struct PointerType,
                          struct FunctionType, struct ArrayType>;

struct IntegerType {
  const int size_bytes;
  const bool is_signed;
};

struct FloatingType {
  const int size_bytes;
};

struct PointerType {
  const Type *base;
};

struct FunctionType {
  const Type *return_type;
  const std::vector<const Token *> names;
  const std::vector<Type *> paramters;
};

struct ArrayType {
  const Type *base;
  const int length;
};

struct Local {
  const int offset;
  const Type *type;
};

struct Global {
  std::string_view name;
  const Type *type;
};

struct Function {
  const Token *name;
  const int max_stack_size;
  const struct Statement *stmt;
  const Type *type;
  const std::vector<Local *> paramters;
};

using Variable = std::variant<Local *, Global *, Function *>;

struct ExprVisitor;

struct Expression {
  virtual void visit(ExprVisitor *visitor) const = 0;
  virtual const Type *type() const = 0;
  ~Expression() = default;
};

struct VarRefExpr : Expression {
  VarRefExpr(Variable var) : var(var) {}
  virtual void visit(ExprVisitor *visitor) const override;
  const Type *type() const override;
  Variable var;
};

struct IntegerExpr : Expression {
  IntegerExpr(const Type *type, int64_t value) : type_(type), value(value) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type() const override;
  const Type *type_;
  const int64_t value;
};

enum class UnaryKind {
  negative,
  refrence,
  deref,
};

struct UnaryExpr : Expression {
  UnaryExpr(UnaryKind kind, const Type *type, Expression *operand)
      : kind(kind), type_(type), operand(operand) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type() const override;
  const UnaryKind kind;
  const Type *type_;
  const Expression *operand;
};

enum class BinaryKind {
  add,
  subtract,
  multiply,
  divide,
  less_than,
  less_equal,
  equal,
  not_equal,
  assign,
};

struct BinaryExpr : Expression {
  BinaryExpr(BinaryKind kind, const Type *type, Expression *lhs,
             Expression *rhs)
      : kind(kind), type_(type), lhs(lhs), rhs(rhs) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type() const override;
  const BinaryKind kind;
  const Type *type_;
  const Expression *lhs;
  const Expression *rhs;
};

struct CallExpr : Expression {
  CallExpr(const Token *name, const Type *type,
           const std::vector<Expression *> arguments)
      : name(name), type_(type), arguments(arguments) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type() const override;
  const Token *name;
  const Type *type_;
  const std::vector<Expression *> arguments;
};

struct ExprVisitor {
  virtual void visit(const VarRefExpr *expr) = 0;
  virtual void visit(const IntegerExpr *expr) = 0;
  virtual void visit(const UnaryExpr *expr) = 0;
  virtual void visit(const BinaryExpr *expr) = 0;
  virtual void visit(const CallExpr *expr) = 0;
};

struct StmtVisitor;

struct Statement {
  virtual void visit(StmtVisitor *visitor) const = 0;
};

struct EmptyStmt : Statement {
  void visit(StmtVisitor *visitor) const override;
};

struct ExpressionStmt : Statement {
  ExpressionStmt(const Expression *expr) : expr(expr) {}
  void visit(StmtVisitor *visitor) const override;
  const Expression *expr;
};

struct ForStmt : Statement {
  ForStmt(Statement *init, Expression *cond, Expression *inc, Statement *then)
      : init(init), cond(cond), inc(inc), then(then) {}
  void visit(StmtVisitor *visitor) const override;
  const Statement *init;
  const Expression *cond;
  const Expression *inc;
  const Statement *then;
};

struct IfStmt : Statement {
  IfStmt(const Expression *cond, const Statement *then, const Statement *else_)
      : cond(cond), then(then), else_(else_) {}
  void visit(StmtVisitor *visitor) const override;
  const Expression *cond;
  const Statement *then;
  const Statement *else_;
};

struct ReturnStmt : Statement {
  ReturnStmt(const Expression *expr) : expr(expr) {}
  void visit(StmtVisitor *visitor) const override;
  const Expression *expr;
};

struct BlockStmt : Statement {
  BlockStmt(std::vector<Statement *> stmts) : stmts(std::move(stmts)) {}
  void visit(StmtVisitor *visitor) const override;
  const std::vector<Statement *> stmts;
};

struct StmtVisitor {
  virtual void visit(const EmptyStmt *stmt) = 0;
  virtual void visit(const ExpressionStmt *stmt) = 0;
  virtual void visit(const ForStmt *stmt) = 0;
  virtual void visit(const IfStmt *stmt) = 0;
  virtual void visit(const ReturnStmt *stmt) = 0;
  virtual void visit(const BlockStmt *stmt) = 0;
};

//
// lzhcc_parse/lzhcc_parse.cc
//

struct Ast {
  std::vector<Global *> globals;
  std::vector<Function *> functions;
};

auto parse(std::span<const Token> tokens, Context &context) -> Ast;

//
// lzhcc_codgen/lzhcc_codgen.cc
//

auto codegen(Ast &ast, Context &context) -> void;

//
// lzhcc_context.cc
//

struct Token;

class Context {
public:
  Context();
  auto append_text(std::string text) -> CharCursorFn;
  auto push_literal(std::string literal) -> int;
  auto literal(int index) const -> std::string_view;
  auto push_identifier(std::string literal) -> int;
  auto identifier(int index) const -> std::string_view;
  auto into_keyword(int index) const -> TokenKind;

  auto int8() -> Type *;
  auto int16() -> Type *;
  auto int32() -> Type *;
  auto int64() -> Type *;

  auto uint8() -> Type *;
  auto uint16() -> Type *;
  auto uint32() -> Type *;
  auto uint64() -> Type *;

  auto pointer_to(const Type *base) -> Type *;
  auto array_of(const Type *base, int length) -> Type *;

  auto size_of(const Type *type) -> int;
  auto integer(int64_t value) -> Expression *;
  auto add(const Type *type, Expression *lhs, Expression *rhs) -> Expression *;
  auto subtract(const Type *type, Expression *lhs, Expression *rhs)
      -> Expression *;
  auto multiply(const Type *type, Expression *lhs, Expression *rhs)
      -> Expression *;
  auto divide(const Type *type, Expression *lhs, Expression *rhs)
      -> Expression *;

  [[noreturn, gnu::format(printf, 3, 4)]] void fatal(int, const char *, ...);

  template <class T, class... Args> auto create(Args &&...args) -> T * {
    auto p = std::make_unique<ArenaEntryTyped<T>>(std::forward<Args>(args)...);
    auto instance = p->instance();
    arena_.push_back(std::move(p));
    return instance;
  }

private:
  struct ArenaEntry {
    virtual ~ArenaEntry() = default;
  };

  template <class T> struct ArenaEntryTyped : ArenaEntry {
    template <class... Args>
    ArenaEntryTyped(Args &&...args) : instance_(std::forward<Args>(args)...) {}
    auto instance() -> T * { return &instance_; }

  private:
    T instance_;
  };

  std::deque<std::string> storage_;
  std::deque<std::string> text_;
  std::vector<std::unique_ptr<ArenaEntry>> arena_;
  std::unordered_map<std::string_view, int> identifier_map_;
  std::vector<TokenKind> keyword_map_;
};

//
// lzhcc_driver.cc
//

auto main(std::span<const char *> args) -> int;

} // namespace lzhcc