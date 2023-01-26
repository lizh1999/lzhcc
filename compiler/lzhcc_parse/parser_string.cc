#include "lzhcc_parse.h"

#include <cassert>

namespace lzhcc {

static auto from_hex(char c) -> int {
  if ('0' <= c && c <= '9') {
    return c - '0';
  } else if ('a' <= c && c <= 'f') {
    return c - 'a' + 10;
  } else {
    return c - 'A' + 10;
  }
}

static auto isodigit(char ch) -> bool { return '0' <= ch && ch <= '7'; }

static auto from_escape(const char *&ptr) -> int {
#define case_return(value, target)                                             \
  case value:                                                                  \
    ptr++;                                                                     \
    return target
  assert(*ptr == '\\');
  switch (int c = 0; *++ptr) {
    case_return('a', '\a');
    case_return('b', '\b');
    case_return('t', '\t');
    case_return('n', '\n');
    case_return('v', '\v');
    case_return('f', '\f');
    case_return('r', '\r');
    case_return('e', '\e');
  case '0' ... '7':
    c = *ptr++ - '0';
    if (isodigit(*ptr)) {
      c = c * 8 + *ptr++ - '0';
      if (isodigit(*ptr))
        c = c * 8 + *ptr++ - '0';
    }
    return c;

  case 'x':
    while (std::isxdigit(*++ptr)) {
      c = c * 16 + from_hex(*ptr);
    }
    return c;
  default:
    return *ptr++;
  }
#undef case_return
}

auto Parser::cook_string() -> std::string {
  auto token = consume(TokenKind::string);
  auto raw = context_->storage(token->inner);
  assert(raw.front() == '"' && raw.back() == '"');
  std::string init;
  const char *ptr = raw.data() + 1;
  while (*ptr != '"') {
    if (*ptr == '\\' && ptr[1] != 'u' && ptr[1] != 'U') {
      init.push_back(from_escape(ptr));
    } else if (*ptr == '\\') {
      ptr += 2;
      uint32_t c = 0;
      while (std::isxdigit(*ptr)) {
        c = c * 16 + from_hex(*ptr++);
      }
      init.append(encode_utf8(c));
    } else [[likely]] {
      init.push_back(*ptr++);
    }
  }
  init.push_back('\0');
  return init;
}

auto Parser::string() -> Expr * {
  auto init = cook_string();
  while (next_is(TokenKind::string)) {
    init.pop_back();
    init.append(cook_string());
  }
  auto type = context_->array_of(context_->int8(), init.size());
  int index = context_->push_literal(std::move(init));
  auto init_view = context_->storage(index);
  auto var = create_anon_global(type, (uint8_t *)init_view.data(), {});
  return context_->value(var);
}

auto Parser::character() -> Expr * {
  auto token = consume(TokenKind::character);
  auto raw = context_->storage(token->inner);

  bool is_width = false;
  bool is_utf16 = false;
  bool is_uft32 = false;
  if (raw[0] == 'L') {
    is_width = true;
    raw.remove_prefix(1);
  } else if (raw[0] == 'u') {
    is_utf16 = true;
    raw.remove_prefix(1);
  } else if (raw[0] == 'U') {
    is_uft32 = true;
    raw.remove_prefix(1);
  }
  assert(raw.front() == '\'' && raw.back() == '\'');
  const char *ptr = raw.data() + 1;
  int value;
  if (*ptr == '\\') {
    value = from_escape(ptr);
  } else {
    value = decode_utf8(ptr);
  }
  if (is_width) {
    return context_->integer(value);
  } else if (is_utf16) {
    return context_->integer((uint16_t)value);
  } else if (is_uft32) {
    return context_->integer((uint32_t) value);
  }  else {
    return context_->integer((int8_t)value);
  }
}

} // namespace lzhcc