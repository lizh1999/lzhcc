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

static auto from_escape(const char *&ptr) -> char {
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

auto Parser::string() -> Expr * {
  auto token = consume();
  auto raw = context_->storage(token->inner);
  assert(raw.front() == '"' && raw.back() == '"');
  std::string init;
  const char *ptr = raw.data() + 1;
  while (*ptr != '"') {
    if (*ptr == '\\') {
      init.push_back(from_escape(ptr));
    } else [[likely]] {
      init.push_back(*ptr++);
    }
  }
  init.push_back('\0');
  auto type = context_->array_of(context_->int8(), init.size());
  int index = context_->push_literal(std::move(init));
  auto init_view = context_->storage(index);
  auto var = create_anon(type, (uint8_t *)init_view.data());
  return context_->value(var);
}

} // namespace lzhcc