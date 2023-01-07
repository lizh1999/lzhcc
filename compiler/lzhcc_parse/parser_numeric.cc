#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <charconv>

namespace lzhcc {

auto Parser::numeric() -> Expr * {
  auto token = consume();
  return integer(token) ?: floating(token);
}

auto Parser::floating(Token *token) -> Expr * {
  auto literal = context_->storage(token->inner);
  auto fmt = std::chars_format::general;
  if (literal.starts_with("0x") || literal.starts_with("0X")) {
    literal.remove_prefix(2);
    fmt = std::chars_format::hex;
  }
  double value;
  auto [end, _] = std::from_chars(literal.begin(), literal.end(), value, fmt);

  std::string_view suffix(end, literal.end());

  Type *type = nullptr;
  if (suffix == "f" || suffix == "F") {
    type = context_->float32();
  } else if (suffix == "l" || suffix == "L") {
    type = context_->float64();
  } else if (!suffix.empty()) {
    context_->fatal(token->location, "");
  } else {
    type = context_->float64();
  }
  return context_->floating(type, value);
}

auto Parser::integer(Token *token) -> Expr * {
  auto literal = context_->storage(token->inner);
  int base;
  if (literal[0] != '0') {
    base = 10;
  } else if (literal.starts_with("0x") || literal.starts_with("0X")) {
    literal.remove_prefix(2);
    base = 16;
  } else if (literal.starts_with("0b") || literal.starts_with("0B")) {
    literal.remove_prefix(2);
    base = 2;
  } else {
    base = 8;
  }

  uint64_t value;
  auto [end, _] = std::from_chars(literal.begin(), literal.end(), value, base);

  std::string_view suffix(end, literal.end());
  bool l = false, u = false;
  for (const char *s :
       {"llu", "LLu", "llU", "LLU", "ull", "uLL", "Ull", "ULL"}) {
    if (suffix.starts_with(s)) {
      suffix.remove_prefix(3);
      l = u = true;
      goto check;
    }
  }

  for (const char *s : {"lu", "Lu", "lU", "LU", "ul", "Ul", "uL", "UL"}) {
    if (suffix.starts_with(s)) {
      suffix.remove_prefix(2);
      l = u = true;
      goto check;
    }
  }

  if (suffix.starts_with("LL") || suffix.starts_with("ll")) {
    suffix.remove_prefix(2);
    l = true;
    goto check;
  }

  if (suffix.starts_with("L") || suffix.starts_with("l")) {
    suffix.remove_prefix(1);
    l = true;
    goto check;
  }

  if (suffix.starts_with("U") || suffix.starts_with("u")) {
    suffix.remove_prefix(1);
    u = true;
    goto check;
  }
check:
  if (!suffix.empty()) {
    return nullptr;
  }

  Type *type = [&]() -> Type * {
    if (l && u)
      return context_->uint64();
    if (base == 10) {
      if (l)
        return context_->int64();
      if (u)
        return value >> 32 ? context_->uint64() : context_->uint32();
      return value >> 31 ? context_->int64() : context_->int32();
    }
    if (l)
      return value >> 63 ? context_->uint64() : context_->int64();
    if (u)
      return value >> 32 ? context_->uint64() : context_->uint32();
    if (value >> 63)
      return context_->uint64();
    if (value >> 32)
      return context_->int64();
    if (value >> 31)
      return context_->uint32();
    return context_->int32();
  }();
  return context_->integer(type, value);
}

} // namespace lzhcc