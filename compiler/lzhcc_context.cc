#include "lzhcc.h"

#include <cassert>
#include <cstdarg>

namespace lzhcc {

class CharCursor {
public:
  CharCursor(const char *cursor, int position)
      : cursor_(cursor), position_(position) {}

  auto operator()() -> std::pair<char, int> {
    if (*cursor_ != 0 && *cursor_ != '\\') {
      return std::pair(*cursor_++, position_++);
    }
    while (*cursor_ == '\\' && cursor_[1] == '\n') [[unlikely]] {
      cursor_ += 2;
      position_ += 2;
    }
    if (*cursor_ == 0) {
      return std::pair(*cursor_, position_);
    } else [[likely]] {
      return std::pair(*cursor_++, position_++);
    }
  }

private:
  const char *cursor_;
  int position_;
};

Context::Context() {
  push_identifier("_Bool");
  push_identifier("break");
  push_identifier("case");
  push_identifier("char");
  push_identifier("continue");
  push_identifier("default");
  push_identifier("else");
  push_identifier("enum");
  push_identifier("for");
  push_identifier("goto");
  push_identifier("if");
  push_identifier("int");
  push_identifier("long");
  push_identifier("return");
  push_identifier("short");
  push_identifier("sizeof");
  push_identifier("static");
  push_identifier("struct");
  push_identifier("switch");
  push_identifier("typedef");
  push_identifier("union");
  push_identifier("void");
  push_identifier("while");
  keyword_map_.push_back(TokenKind::kw_bool);
  keyword_map_.push_back(TokenKind::kw_break);
  keyword_map_.push_back(TokenKind::kw_case);
  keyword_map_.push_back(TokenKind::kw_char);
  keyword_map_.push_back(TokenKind::kw_continue);
  keyword_map_.push_back(TokenKind::kw_default);
  keyword_map_.push_back(TokenKind::kw_else);
  keyword_map_.push_back(TokenKind::kw_enum);
  keyword_map_.push_back(TokenKind::kw_for);
  keyword_map_.push_back(TokenKind::kw_goto);
  keyword_map_.push_back(TokenKind::kw_if);
  keyword_map_.push_back(TokenKind::kw_int);
  keyword_map_.push_back(TokenKind::kw_long);
  keyword_map_.push_back(TokenKind::kw_return);
  keyword_map_.push_back(TokenKind::kw_short);
  keyword_map_.push_back(TokenKind::kw_sizeof);
  keyword_map_.push_back(TokenKind::kw_static);
  keyword_map_.push_back(TokenKind::kw_struct);
  keyword_map_.push_back(TokenKind::kw_switch);
  keyword_map_.push_back(TokenKind::kw_typedef);
  keyword_map_.push_back(TokenKind::kw_union);
  keyword_map_.push_back(TokenKind::kw_void);
  keyword_map_.push_back(TokenKind::kw_while);
}

auto Context::append_file(std::string path) -> CharCursorFn {
  FILE *in = nullptr;
  if (path == "-") {
    in = stdin;
  } else {
    in = fopen(path.c_str(), "r");
  }
  std::string text;
  enum { buffer_size = 4096 };
  size_t n = 0;
  do {
    char buffer[buffer_size];
    n = fread(buffer, 1, buffer_size, in);
    text.append(buffer, n);
  } while (n == buffer_size);
  if (in != stdin) {
    fclose(in);
  }
  return append_text(std::move(text));
}

auto Context::append_text(std::string text) -> CharCursorFn {
  int location = 0;
  for (int i = 0; i < text_.size(); i++) {
    location += text_[i].size();
  }
  text_.push_back(std::move(text));
  return CharCursor(text_.back().c_str(), location);
}

auto Context::push_literal(std::string literal) -> int {
  int index = storage_.size();
  storage_.push_back(std::move(literal));
  return index;
}

auto Context::storage(int index) const -> std::string_view {
  return storage_[index];
}

auto Context::push_identifier(std::string literal) -> int {
  if (auto it = identifier_map_.find(literal); it != identifier_map_.end()) {
    return it->second;
  } else {
    int index = storage_.size();
    storage_.push_back(std::move(literal));
    identifier_map_.emplace(storage_.back(), index);
    return index;
  }
}

auto Context::into_keyword(int index) const -> TokenKind {
  if (index < keyword_map_.size()) {
    return keyword_map_[index];
  } else {
    return TokenKind::identifier;
  }
}

auto Context::void_type() -> Type * { return create<VoidType>(); }

auto Context::boolean() -> Type * { return create<BoolType>(); }

auto Context::int8() -> Type * {
  return create<IntegerType>(IntegerKind::byte, /*is_unsigned=*/false);
}

auto Context::int16() -> Type * {
  return create<IntegerType>(IntegerKind::half, /*is_unsigned=*/false);
}

auto Context::int32() -> Type * {
  return create<IntegerType>(IntegerKind::word, /*is_unsigned=*/false);
}

auto Context::int64() -> Type * {
  return create<IntegerType>(IntegerKind::dword, /*is_unsigned=*/false);
}

auto Context::pointer_to(Type *base) -> Type * {
  return create<PointerType>(base);
}

auto Context::array_of(Type *base, int length) -> Type * {
  return create<ArrayType>(base, length);
}

auto Context::function_type(Type *ret, std::vector<Type *> params) -> Type * {
  return create<FunctionType>(ret, std::move(params));
}

auto Context::record_type() -> RecordType * {
  return create<RecordType>(RecordType::dummy());
}

auto Context::size_of(Type *type) -> int {
  switch (type->kind) {
  case TypeKind::kw_void:
  case TypeKind::boolean:
    return 1;
  case TypeKind::pointer:
  case TypeKind::function:
    return 8;
  case TypeKind::integer: {
    auto integer = cast<IntegerType>(type);
    return static_cast<int>(integer->kind);
  }
  case TypeKind::array: {
    auto array = cast<ArrayType>(type);
    return array->length == -1 ? 8 : array->length * size_of(array->base);
  }
  case TypeKind::record:
    auto record = cast<RecordType>(type);
    return record->size_bytes;
  }
}

auto Context::align_of(Type *type) -> int {
  switch (type->kind) {
  case TypeKind::kw_void:
    std::abort();
  case TypeKind::boolean:
    return 1;
  case TypeKind::pointer:
  case TypeKind::function:
    return 8;
  case TypeKind::integer: {
    auto integer = cast<IntegerType>(type);
    return static_cast<int>(integer->kind);
  }
  case TypeKind::array:
    return align_of(cast<ArrayType>(type)->base);
  case TypeKind::record:
    return cast<RecordType>(type)->align_bytes;
  }
}

auto Context::create_declaration(Type *type, std::string_view name)
    -> Declaration * {
  return create<Declaration>(type, name);
}

auto Context::create_local(Type *type, int offset) -> LValue * {
  return create<LValue>(type, offset);
}

auto Context::create_global(Type *type, std::string_view name, uint8_t *init)
    -> GValue * {
  return create<GValue>(type, name, init);
}

auto Context::create_function(Type *type, std::string_view name, int stack_size,
                              Stmt *stmt, std::vector<LValue *> params,
                              Linkage linkage) -> Function * {
  return create<Function>(type, name, stack_size, stmt, std::move(params),
                          linkage);
}

auto Context::create_label(std::string_view name) -> Label * {
  return create<Label>(Label{name});
}

auto Context::value(Value *value) -> Expr * { return create<ValueExpr>(value); }

auto Context::integer(int8_t value) -> Expr * {
  return create<IntegerExpr>(int8(), value);
}

auto Context::integer(int32_t value) -> Expr * {
  return create<IntegerExpr>(int32(), value);
}

auto Context::integer(int64_t value) -> Expr * {
  return create<IntegerExpr>(int64(), value);
}

auto Context::negative(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::negative, type, operand);
}

auto Context::refrence(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::refrence, type, operand);
}

auto Context::deref(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::deref, type, operand);
}

auto Context::cast(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::cast, type, operand);
}

auto Context::logical_not(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::logical_not, type, operand);
}

auto Context::bitwise_not(Type *type, Expr *operand) -> Expr * {
  return create<UnaryExpr>(UnaryKind::bitwise_not, type, operand);
}

auto Context::add(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::add, type, lhs, rhs);
}

auto Context::subtract(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::subtract, type, lhs, rhs);
}

auto Context::multiply(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::multiply, type, lhs, rhs);
}

auto Context::divide(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::divide, type, lhs, rhs);
}

auto Context::modulo(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::modulo, type, lhs, rhs);
}

auto Context::less_than(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::less_than, type, lhs, rhs);
}

auto Context::less_equal(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::less_equal, type, lhs, rhs);
}

auto Context::equal(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::equal, type, lhs, rhs);
}

auto Context::not_equal(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::not_equal, type, lhs, rhs);
}

auto Context::assign(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::assign, type, lhs, rhs);
}

auto Context::comma(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::comma, type, lhs, rhs);
}

auto Context::condition(Type *type, Expr *cond, Expr *then, Expr *else_)
    -> Expr * {
  return create<ConditionExpr>(type, cond, then, else_);
}

auto Context::bitwise_or(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::bitwise_or, type, lhs, rhs);
}

auto Context::bitwise_xor(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::bitwise_xor, type, lhs, rhs);
}

auto Context::bitwise_and(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::bitwise_and, type, lhs, rhs);
}

auto Context::logical_and(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::logical_and, type, lhs, rhs);
}

auto Context::logical_or(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::logical_or, type, lhs, rhs);
}

auto Context::shift_left(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::shift_left, type, lhs, rhs);
}

auto Context::shift_right(Type *type, Expr *lhs, Expr *rhs) -> Expr * {
  return create<BinaryExpr>(BinaryKind::shift_right, type, lhs, rhs);
}

auto Context::call(std::string_view name, Type *type, std::vector<Expr *> args)
    -> Expr * {
  return create<CallExpr>(name, type, std::move(args));
}

auto Context::member(Type *type, Expr *record, int offset) -> Expr * {
  return create<MemberExpr>(type, record, offset);
}

auto Context::stmt_expr(Type *type, BlockStmt *stmt) -> Expr * {
  return create<StmtExpr>(type, stmt);
}

auto Context::empty_stmt() -> Stmt * { return create<EmptyStmt>(); }

auto Context::expr_stmt(Expr *expr) -> Stmt * { return create<ExprStmt>(expr); }

auto Context::for_stmt(Stmt *init, Expr *cond, Expr *inc, Stmt *then,
                       Label *continue_label, Label *break_label) -> Stmt * {
  return create<ForStmt>(init, cond, inc, then, continue_label, break_label);
}

auto Context::if_stmt(Expr *cond, Stmt *then, Stmt *else_) -> Stmt * {
  return create<IfStmt>(cond, then, else_);
}

auto Context::return_stmt(Expr *expr) -> Stmt * {
  return create<ReturnStmt>(expr);
}

auto Context::block_stmt(std::vector<Stmt *> stmts) -> Stmt * {
  return create<BlockStmt>(std::move(stmts));
}

auto Context::goto_stmt(Label *label) -> Stmt * {
  return create<GotoStmt>(label);
}

auto Context::label_stmt(Label *label) -> Stmt * {
  return create<LabelStmt>(label);
}

auto Context::switch_stmt(Expr *expr, Label *break_label) -> SwitchStmt * {
  return create<SwitchStmt>(expr, break_label);
}

auto Context::case_stmt(Stmt *stmt, int64_t value, Label *label) -> CaseStmt * {
  return create<CaseStmt>(stmt, value, label);
}

auto Context::default_stmt(Stmt *stmt, Label *label) -> Stmt * {
  return create<DefaultStmt>(stmt, label);
}

auto Context::fatal(int loc, const char *fmt, ...) -> void {
  int file_id = 0;
  while (file_id < text_.size() && text_[file_id].size() <= loc) {
    loc -= text_[file_id++].size();
  }
  assert(file_id != text_.size());
  // std::string_view filename = filename_[file_id];
  std::string_view line = text_[file_id];
  int line_number = 0;
  auto pos = line.find('\n');
  while (pos < loc) {
    line.remove_prefix(pos + 1);
    loc -= pos + 1;
    line_number++;
    pos = line.find('\n');
  }
  if (pos != std::string_view::npos) {
    line = line.substr(0, pos);
  }

  char *message;
  size_t length;
  FILE *out = open_memstream(&message, &length);

  va_list args;
  va_start(args, fmt);
  vfprintf(out, fmt, args);
  va_end(args);

  fclose(out);
  lzhcc::fatal({"", line, line_number, loc}, message);
}

} // namespace lzhcc