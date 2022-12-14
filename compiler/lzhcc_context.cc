#include "lzhcc.h"

#include <cassert>
#include <cstdarg>
#include <cstdint>
#include <cstdio>

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
  push_identifier("else");
  push_identifier("for");
  push_identifier("if");
  push_identifier("int");
  push_identifier("return");
  push_identifier("sizeof");
  push_identifier("while");
  keyword_map_.push_back(TokenKind::kw_else);
  keyword_map_.push_back(TokenKind::kw_for);
  keyword_map_.push_back(TokenKind::kw_if);
  keyword_map_.push_back(TokenKind::kw_int);
  keyword_map_.push_back(TokenKind::kw_return);
  keyword_map_.push_back(TokenKind::kw_sizeof);
  keyword_map_.push_back(TokenKind::kw_while);
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

auto Context::literal(int index) const -> std::string_view {
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

auto Context::identifier(int index) const -> std::string_view {
  return storage_[index];
}

auto Context::into_keyword(int index) const -> TokenKind {
  if (index < keyword_map_.size()) {
    return keyword_map_[index];
  } else {
    return TokenKind::identifier;
  }
}

auto Context::int8() -> Type * { return create<Type>(IntegerType{1, true}); }

auto Context::int16() -> Type * { return create<Type>(IntegerType{2, true}); }

auto Context::int32() -> Type * { return create<Type>(IntegerType{4, true}); }

auto Context::int64() -> Type * { return create<Type>(IntegerType{8, true}); }

auto Context::uint8() -> Type * { return create<Type>(IntegerType{1, false}); }

auto Context::uint16() -> Type * { return create<Type>(IntegerType{2, false}); }

auto Context::uint32() -> Type * { return create<Type>(IntegerType{4, false}); }

auto Context::uint64() -> Type * { return create<Type>(IntegerType{8, false}); }

auto Context::pointer_to(const Type *base) -> Type * {
  return create<Type>(PointerType{base});
}

auto Context::array_of(const Type *base, int length) -> Type * {
  return create<Type>(ArrayType{base, length});
}

inline struct {
  auto operator()(const IntegerType &type) -> const int {
    return type.size_bytes;
  }
  auto operator()(const PointerType &) -> const int { return 8; }
  auto operator()(const FunctionType &) -> const int { return 0; }
  auto operator()(const ArrayType &type) -> const int {
    return type.length < 0 ? 8 : type.length * std::visit(*this, *type.base);
  }
} size_of;

auto Context::size_of(const Type *type) -> int {
  return std::visit(lzhcc::size_of, *type);
}

auto Context::integer(int64_t value) -> Expression * {
  return create<IntegerExpr>(int64(), value);
}

auto Context::add(const Type *type, Expression *lhs, Expression *rhs)
    -> Expression * {
  return create<BinaryExpr>(BinaryKind::add, type, lhs, rhs);
}

auto Context::subtract(const Type *type, Expression *lhs, Expression *rhs)
    -> Expression * {
  return create<BinaryExpr>(BinaryKind::subtract, type, lhs, rhs);
}

auto Context::multiply(const Type *type, Expression *lhs, Expression *rhs)
    -> Expression * {
  return create<BinaryExpr>(BinaryKind::multiply, type, lhs, rhs);
}

auto Context::divide(const Type *type, Expression *lhs, Expression *rhs)
    -> Expression * {
  return create<BinaryExpr>(BinaryKind::divide, type, lhs, rhs);
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