#pragma once

#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

namespace lzhcc {

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
  minus,       // "-"
  plus,        // "+"
  slash,       // "/"
  star,        // "*"
  open_paren,  // "("
  close_paren, // ")"
  numeric,     // numeric value
  eof,         // eof
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

struct TypeVisitor;

struct Type {
  virtual void visit(TypeVisitor *visitor) const = 0;
  ~Type() = default;
};

struct IntegerType : Type {
  IntegerType(uint8_t size_bytes, bool is_signed)
      : size_bytes(size_bytes), is_signed(is_signed) {}
  void visit(TypeVisitor *visitor) const override;
  const int size_bytes;
  const bool is_signed;
};

struct FloatingType : Type {
  FloatingType(uint8_t size_bytes) : size_bytes(size_bytes) {}
  void visit(TypeVisitor *visitor) const override;
  const int size_bytes;
};

struct TypeVisitor {
  virtual void visit(const IntegerType *type) = 0;
  virtual void visit(const FloatingType *type) = 0;
};

struct AstVisitor;

struct AstNode {
  virtual void visit(AstVisitor *visitor) const = 0;
  virtual ~AstNode() = default;
};

struct ExprVisitor;

struct Expression : AstNode {
  void visit(AstVisitor *visitor) const override;
  virtual void visit(ExprVisitor *visitor) const = 0;
  ~Expression() = default;
};

struct AstVisitor {
  virtual void visit(const Expression *) = 0;
};

struct IntegerExpr : Expression {
  IntegerExpr(Type *type, int64_t value) : type(type), value(value) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type;
  const int64_t value;
};

struct FloatingExpr : Expression {
  FloatingExpr(Type *type, double value) : type(type), value(value) {}
  void visit(ExprVisitor *visitor) const override;
  const Type *type;
  const double value;
};

enum class UnaryKind {
  negative,
};

struct UnaryExpr : Expression {
  UnaryExpr(UnaryKind kind, Type *type, Expression *operand)
      : kind(kind), type(type), operand(operand) {}
  void visit(ExprVisitor *visitor) const override;
  const UnaryKind kind;
  const Type *type;
  const Expression *operand;
};

enum class BinaryKind {
  add,
  subtract,
  multiply,
  divide,
};

struct BinaryExpr : Expression {
  BinaryExpr(BinaryKind kind, Type *type, Expression *lhs, Expression *rhs)
      : kind(kind), type(type), lhs(lhs), rhs(rhs) {}
  void visit(ExprVisitor *visitor) const override;
  const BinaryKind kind;
  const Type *type;
  const Expression *lhs;
  const Expression *rhs;
};

struct ExprVisitor {
  virtual void visit(const IntegerExpr *) = 0;
  virtual void visit(const FloatingExpr *) = 0;
  virtual void visit(const UnaryExpr *) = 0;
  virtual void visit(const BinaryExpr *) = 0;
};

//
// lzhcc_parse/lzhcc_parse.cc
//

auto parse(std::span<const Token> tokens, Context &context) -> Expression *;

//
// lzhcc_codgen/lzhcc_codgen.cc
//

auto codegen(Expression *expression, Context &context) -> void;

//
// lzhcc_context.cc
//

struct Token;

class Context {
public:
  auto append_text(std::string text) -> CharCursorFn;
  auto push_literal(std::string literal) -> int;
  auto literal(int index) const -> std::string_view;

  auto int8() -> IntegerType *;
  auto int16() -> IntegerType *;
  auto int32() -> IntegerType *;
  auto int64() -> IntegerType *;

  auto uint8() -> IntegerType *;
  auto uint16() -> IntegerType *;
  auto uint32() -> IntegerType *;
  auto uint64() -> IntegerType *;

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
};

//
// lzhcc_driver.cc
//

auto main(std::span<const char *> args) -> int;

} // namespace lzhcc