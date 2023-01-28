#include "lzhcc.h"

#include <algorithm>
#include <cassert>
#include <cerrno>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

namespace lzhcc {

CharCursor::CharCursor(const char *cursor, int position)
    : cursor_(cursor), position_(position) {}

auto CharCursor::operator()() -> std::pair<char, int> {
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

Context::Context() {
  push_identifier("_Alignas");
  push_identifier("_Alignof");
  push_identifier("asm");
  push_identifier("auto");
  push_identifier("_Bool");
  push_identifier("break");
  push_identifier("case");
  push_identifier("char");
  push_identifier("const");
  push_identifier("continue");
  push_identifier("default");
  push_identifier("do");
  push_identifier("double");
  push_identifier("else");
  push_identifier("enum");
  push_identifier("extern");
  push_identifier("float");
  push_identifier("for");
  push_identifier("goto");
  push_identifier("if");
  push_identifier("inline");
  push_identifier("int");
  push_identifier("long");
  push_identifier("_Noreturn");
  push_identifier("register");
  push_identifier("restrict");
  push_identifier("__restrict");
  push_identifier("__restrict__");
  push_identifier("return");
  push_identifier("short");
  push_identifier("signed");
  push_identifier("sizeof");
  push_identifier("static");
  push_identifier("struct");
  push_identifier("switch");
  push_identifier("typedef");
  push_identifier("union");
  push_identifier("unsigned");
  push_identifier("void");
  push_identifier("volatile");
  push_identifier("while");
  keyword_map_.push_back(TokenKind::kw_alignas);
  keyword_map_.push_back(TokenKind::kw_alignof);
  keyword_map_.push_back(TokenKind::kw_asm);
  keyword_map_.push_back(TokenKind::kw_auto);
  keyword_map_.push_back(TokenKind::kw_bool);
  keyword_map_.push_back(TokenKind::kw_break);
  keyword_map_.push_back(TokenKind::kw_case);
  keyword_map_.push_back(TokenKind::kw_char);
  keyword_map_.push_back(TokenKind::kw_const);
  keyword_map_.push_back(TokenKind::kw_continue);
  keyword_map_.push_back(TokenKind::kw_default);
  keyword_map_.push_back(TokenKind::kw_do);
  keyword_map_.push_back(TokenKind::kw_double);
  keyword_map_.push_back(TokenKind::kw_else);
  keyword_map_.push_back(TokenKind::kw_enum);
  keyword_map_.push_back(TokenKind::kw_extern);
  keyword_map_.push_back(TokenKind::kw_float);
  keyword_map_.push_back(TokenKind::kw_for);
  keyword_map_.push_back(TokenKind::kw_goto);
  keyword_map_.push_back(TokenKind::kw_if);
  keyword_map_.push_back(TokenKind::kw_inline);
  keyword_map_.push_back(TokenKind::kw_int);
  keyword_map_.push_back(TokenKind::kw_long);
  keyword_map_.push_back(TokenKind::kw_noreturn);
  keyword_map_.push_back(TokenKind::kw_register);
  keyword_map_.push_back(TokenKind::kw_restrict);
  keyword_map_.push_back(TokenKind::kw_restrict);
  keyword_map_.push_back(TokenKind::kw_restrict);
  keyword_map_.push_back(TokenKind::kw_return);
  keyword_map_.push_back(TokenKind::kw_short);
  keyword_map_.push_back(TokenKind::kw_signed);
  keyword_map_.push_back(TokenKind::kw_sizeof);
  keyword_map_.push_back(TokenKind::kw_static);
  keyword_map_.push_back(TokenKind::kw_struct);
  keyword_map_.push_back(TokenKind::kw_switch);
  keyword_map_.push_back(TokenKind::kw_typedef);
  keyword_map_.push_back(TokenKind::kw_union);
  keyword_map_.push_back(TokenKind::kw_unsigned);
  keyword_map_.push_back(TokenKind::kw_void);
  keyword_map_.push_back(TokenKind::kw_volatile);
  keyword_map_.push_back(TokenKind::kw_while);
}

Context::~Context() {
  for (auto &tmp : tmpfile_) {
    unlink(tmp.c_str());
  }
}

auto Context::to_string(Token &token) -> std::string_view {
  switch (token.kind) {
  case TokenKind::amp:
    return "&";
  case TokenKind::amp_amp:
    return "&&";
  case TokenKind::amp_equal:
    return "&=";
  case TokenKind::arrow:
    return "->";
  case TokenKind::caret:
    return "^";
  case TokenKind::caret_equal:
    return "^=";
  case TokenKind::comma:
    return ",";
  case TokenKind::colon:
    return ":";
  case TokenKind::dot:
    return ".";
  case TokenKind::dotdotdot:
    return "...";
  case TokenKind::equal:
    return "=";
  case TokenKind::equal_equal:
    return "==";
  case TokenKind::exclaim:
    return "!";
  case TokenKind::exclaim_equal:
    return "!=";
  case TokenKind::greater:
    return ">";
  case TokenKind::greater_equal:
    return ">=";
  case TokenKind::greater_greater:
    return ">>";
  case TokenKind::greater_greater_equal:
    return ">>>";
  case TokenKind::hash:
    return "#";
  case TokenKind::hash_hash:
    return "##";
  case TokenKind::less:
    return "<";
  case TokenKind::less_equal:
    return "<=";
  case TokenKind::less_less:
    return "<<";
  case TokenKind::less_less_equal:
    return "<<<";
  case TokenKind::minus:
    return "-";
  case TokenKind::minus_equal:
    return "-=";
  case TokenKind::minus_minus:
    return "--";
  case TokenKind::percent:
    return "%";
  case TokenKind::percent_equal:
    return "%=";
  case TokenKind::pipe:
    return "|";
  case TokenKind::pipe_equal:
    return "|=";
  case TokenKind::pipe_pipe:
    return "||";
  case TokenKind::plus:
    return "+";
  case TokenKind::plus_equal:
    return "+=";
  case TokenKind::plus_plus:
    return "++";
  case TokenKind::question:
    return "?";
  case TokenKind::semi:
    return ";";
  case TokenKind::slash:
    return "/";
  case TokenKind::slash_equal:
    return "/=";
  case TokenKind::star:
    return "*";
  case TokenKind::star_equal:
    return "*=";
  case TokenKind::tilde:
    return "~";
  case TokenKind::open_paren:
    return "(";
  case TokenKind::close_paren:
    return ")";
  case TokenKind::open_bracket:
    return "[";
  case TokenKind::close_bracket:
    return "]";
  case TokenKind::open_brace:
    return "{";
  case TokenKind::close_brace:
    return "}";
  case TokenKind::unknown:
    return {reinterpret_cast<const char *>(&token.inner), 1};
  case TokenKind::eof:
    return "\n";
  default:
    return storage_[token.inner];
  }
}

auto Context::remove_macro(int name) -> void { macro_map_.erase(name); }

auto Context::remove_macro(const char *name) -> void {
  auto ident = push_identifier(name);
  macro_map_.erase(ident);
}

auto Context::find_macro(int name) -> Macro * {
  auto it = macro_map_.find(name);
  return it == macro_map_.end() ? nullptr : it->second;
}

auto Context::object_macro(int name, std::vector<Token> replace) -> void {
  auto macro = create<ObjectMacro>(std::move(replace));
  macro_map_[name] = macro;
}

auto Context::function_macro(int name, std::vector<ParamKind> param,
                             std::vector<Token> replace, bool is_variadic)
    -> void {
  auto macro =
      create<FunctionMacro>(std::move(param), std::move(replace), is_variadic);
  macro_map_[name] = macro;
}

auto Context::builtin_macro(const char *name,
                            std::function<Token(Token)> handle) -> void {
  auto macro = create<BuiltinMacro>(std::move(handle));
  macro_map_[push_identifier(name)] = macro;
}

auto Context::append_file(std::string path) -> CharCursor {
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
  return append_text(std::move(text), path == "-" ? "<stdin>" : path);
}

auto Context::append_text(std::string text, std::string name) -> CharCursor {
  filename_.push_back(name);
  int location = 0;
  if (!file_location_.empty()) {
    location = file_location_.back() + text_.back().size();
  }
  std::vector<int> line = {0};
  for (int i = 0; i < text.size(); i++) {
    if (text[i] == '\n') {
      line.push_back(i + 1);
    }
  }
  line.push_back(text.size());
  file_location_.push_back(location);
  line_location_.push_back(std::move(line));
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

auto Context::c_str(int index) const -> const char * {
  return storage_[index].c_str();
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
  return create<IntegerType>(IntegerKind::byte, Sign::sign);
}

auto Context::int16() -> Type * {
  return create<IntegerType>(IntegerKind::half, Sign::sign);
}

auto Context::int32() -> Type * {
  return create<IntegerType>(IntegerKind::word, Sign::sign);
}

auto Context::int64() -> Type * {
  return create<IntegerType>(IntegerKind::dword, Sign::sign);
}

auto Context::uint8() -> Type * {
  return create<IntegerType>(IntegerKind::byte, Sign::unsign);
}

auto Context::uint16() -> Type * {
  return create<IntegerType>(IntegerKind::half, Sign::unsign);
}

auto Context::uint32() -> Type * {
  return create<IntegerType>(IntegerKind::word, Sign::unsign);
}

auto Context::uint64() -> Type * {
  return create<IntegerType>(IntegerKind::dword, Sign::unsign);
}

auto Context::float32() -> Type * {
  return create<FloatingType>(FloatingKind::float32);
}

auto Context::float64() -> Type * {
  return create<FloatingType>(FloatingKind::float64);
}

auto Context::pointer_to(Type *base) -> Type * {
  return create<PointerType>(base);
}

auto Context::array_of(Type *base, int length) -> Type * {
  return create<ArrayType>(base, length);
}

auto Context::function_type(Type *ret, std::vector<Type *> params,
                            bool is_variadic) -> Type * {
  return create<FunctionType>(ret, std::move(params), is_variadic);
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
  case TypeKind::floating: {
    auto floating = cast<FloatingType>(type);
    return static_cast<int>(floating->kind);
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
  case TypeKind::floating: {
    auto floating = cast<FloatingType>(type);
    return static_cast<int>(floating->kind);
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

auto Context::create_global(Type *type, std::string_view name, uint8_t *init,
                            std::vector<Relocation> relocations,
                            int align_bytes, Linkage linkage) -> GValue * {
  return create<GValue>(type, name, init, std::move(relocations), align_bytes,
                        linkage);
}

auto Context::create_function(Type *type, std::string_view name, int stack_size,
                              Stmt *stmt, std::vector<LValue *> params,
                              LValue *va_area, Linkage linkage) -> Function * {
  return create<Function>(type, name, stack_size, stmt, std::move(params),
                          va_area, linkage);
}

auto Context::create_label(std::string_view name) -> Label * {
  return create<Label>(Label{name});
}

auto Context::zero(Expr *expr, int64_t size) -> Expr * {
  return create<ZeroExpr>(void_type(), expr, size);
}

auto Context::value(Value *value) -> Expr * { return create<ValueExpr>(value); }

auto Context::integer(int8_t value) -> Expr * {
  return create<IntegerExpr>(int8(), value);
}

auto Context::integer(uint16_t value) -> Expr * {
  return create<IntegerExpr>(uint16(), value);
}

auto Context::integer(int32_t value) -> Expr * {
  return create<IntegerExpr>(int32(), value);
}

auto Context::integer(uint32_t value) -> Expr * {
  return create<IntegerExpr>(uint32(), value);
}

auto Context::integer(int64_t value) -> Expr * {
  return create<IntegerExpr>(int64(), value);
}

auto Context::integer(Type *type, int64_t value) -> Expr * {
  return create<IntegerExpr>(type, value);
}

auto Context::floating(Type *type, double value) -> Expr * {
  return create<FloatingExpr>(type, value);
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

auto Context::call(Type *type, Expr *func, std::vector<Expr *> args,
                   int arg_num, LValue *ret_buffer) -> Expr * {
  return create<CallExpr>(type, func, std::move(args), arg_num, ret_buffer);
}

auto Context::member(Type *type, Expr *record, Member *member) -> Expr * {
  return create<MemberExpr>(type, record, member);
}

auto Context::stmt_expr(Type *type, BlockStmt *stmt) -> Expr * {
  return create<StmtExpr>(type, stmt);
}

auto Context::empty_stmt() -> Stmt * { return create<EmptyStmt>(); }

auto Context::asm_stmt(std::string_view code) -> Stmt * {
  return create<AsmStmt>(code);
}

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

auto Context::do_stmt(Stmt *stmt, Expr *cond, Label *continue_label,
                      Label *break_label) -> Stmt * {
  return create<DoStmt>(stmt, cond, continue_label, break_label);
}

auto Context::array_init(std::vector<Init *> children, Type *base) -> Init * {
  return create<ArrayInit>(std::move(children), base);
}

auto Context::record_init(RecordInit::Children children) -> Init * {
  return create<RecordInit>(std::move(children));
}

auto Context::scalar_init(Expr *expr) -> Init * {
  return create<ScalarInit>(expr);
}

auto Context::create_tmpfile() -> std::string {
  std::string path = "/tmp/lzhcc-XXXXXX";
  int fd = mkstemp(const_cast<char *>(path.c_str()));
  if (fd == -1) {
    fprintf(stderr, "mkstemp failed %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }
  close(fd);
  tmpfile_.push_back(path);
  return path;
}

auto Context::filename(int loc) -> std::string_view {
  int file_id =
      std::upper_bound(file_location_.begin(), file_location_.end(), loc) -
      file_location_.begin() - 1;
  return filename_[file_id];
}

auto Context::line_number(int loc) -> int {
  int file_id =
      std::upper_bound(file_location_.begin(), file_location_.end(), loc) -
      file_location_.begin() - 1;
  loc -= file_location_[file_id];
  return std::upper_bound(line_location_[file_id].begin(),
                          line_location_[file_id].end(), loc) -
         line_location_[file_id].begin() - 1;
}

auto Context::fatal(int loc, const char *fmt, ...) -> void {
  int file_id =
      std::upper_bound(file_location_.begin(), file_location_.end(), loc) -
      file_location_.begin() - 1;
  loc -= file_location_[file_id];
  std::string_view filename = filename_[file_id];
  int line_number = std::upper_bound(line_location_[file_id].begin(),
                                     line_location_[file_id].end(), loc) -
                    line_location_[file_id].begin() - 1;
  loc -= line_location_[file_id][line_number];
  std::string_view line(
      text_[file_id].data() + line_location_[file_id][line_number],
      text_[file_id].data() + line_location_[file_id][line_number + 1]);
  if (line.back() == '\n') {
    line.remove_suffix(1);
  }
  char *message;
  size_t length;
  FILE *out = open_memstream(&message, &length);

  va_list args;
  va_start(args, fmt);
  vfprintf(out, fmt, args);
  va_end(args);

  fclose(out);
  lzhcc::fatal({filename, line, line_number, loc}, message);
}

} // namespace lzhcc