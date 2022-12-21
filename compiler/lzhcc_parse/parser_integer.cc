#include "lzhcc_parse.h"
#include <charconv>

namespace lzhcc {

auto Parser::integer() -> Expr * {
  auto token = consume(TokenKind::numeric);
  auto literal = context_->storage(token->inner);
  int64_t value;
  if (literal[0] != '0') {
    std::from_chars(literal.begin(), literal.end(), value);
  } else if (literal.starts_with("0x") || literal.starts_with("0X")) {
    std::from_chars(literal.begin() + 2, literal.end(), value, 16);
  } else if (literal.starts_with("0b") || literal.starts_with("0B")) {
    std::from_chars(literal.begin() + 2, literal.end(), value, 2);
  } else {
    std::from_chars(literal.begin(), literal.end(), value, 8);
  }
  if (INT_MIN <= value && value <= INT_MAX) {
    auto i32 = static_cast<int32_t>(value);
    return context_->integer(i32);
  } else {
    return context_->integer(value);
  }
}

} // namespace lzhcc