#include "lzhcc_parse.h"
#include <cassert>
#include <type_traits>

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

template <class T> static auto cook_string(const char *ptr) -> std::vector<T> {
  std::vector<T> data;
  while (*ptr != '"') {
    if (*ptr == '\\' && ptr[1] != 'u' && ptr[1] != 'U') {
      data.push_back(from_escape(ptr));
    } else {
      uint32_t c = 0;
      if (*ptr != '\\') {
        c = decode_utf8(ptr);
      } else {
        for (ptr += 2; std::isxdigit(*ptr);) {
          c = c * 16 + from_hex(*ptr++);
        }
      }
      if constexpr (std::is_same_v<T, uint16_t>) {
        if (c < 0x10000) {
          data.push_back(c);
        } else {
          c -= 0x10000;
          data.push_back(0xd800 + ((c >> 10) & 0x3ff));
          data.push_back(0xdc00 + (c & 0x3ff));
        }
      }
      if constexpr (std::is_same_v<T, char>) {
        auto str = encode_utf8(c);
        data.insert(data.end(), str.begin(), str.end());
      }

      if constexpr (std::is_same_v<T, uint32_t>) {
        data.push_back(c);
      }
    }
  }
  return data;
}

auto Parser::cook_string(IntegerType *&type) -> std::string {
  auto token = consume(TokenKind::string);
  auto raw = context_->storage(token->inner);
  std::string init;
  if (raw[0] == 'u' && raw[1] == '8') {
    raw.remove_prefix(2);
    type = type ?: (IntegerType *)context_->int8();
  } else if (raw[0] == 'u') {
    raw.remove_prefix(1);
    type = type ?: (IntegerType *)context_->uint16();
  } else if (raw[0] == 'U') {
    raw.remove_prefix(1);
    type = type ?: (IntegerType *)context_->uint32();
  } else {
    type = type ?: (IntegerType *)context_->int8();
  }
  if (type->kind == IntegerKind::byte) {
    auto data = lzhcc::cook_string<char>(raw.data() + 1);
    init.append(data.data(), data.size());
  } else if (type->kind == IntegerKind::half) {
    auto data = lzhcc::cook_string<uint16_t>(raw.data() + 1);
    auto ptr = reinterpret_cast<char *>(data.data());
    init.append(ptr, data.size() * 2);
  } else {
    auto data = lzhcc::cook_string<uint32_t>(raw.data() + 1);
    auto ptr = reinterpret_cast<char *>(data.data());
    init.append(ptr, data.size() * 4);
  }
  return init;
}

auto Parser::string() -> Expr * {
  IntegerType *base = 0;
  auto init = cook_string(base);
  while (next_is(TokenKind::string)) {
    init.append(cook_string(base));
  }
  int size;
  if (base->kind == IntegerKind::byte) {
    init.push_back(0);
    size = init.size();
  } else if (base->kind == IntegerKind::half) {
    init.push_back(0);
    init.push_back(0);
    size = init.size() / 2;
  } else {
    init.push_back(0);
    init.push_back(0);
    init.push_back(0);
    init.push_back(0);
    size = init.size() / 4;
  }
  auto type = context_->array_of(base, size);
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
    return context_->integer((uint32_t)value);
  } else {
    return context_->integer((int8_t)value);
  }
}

} // namespace lzhcc