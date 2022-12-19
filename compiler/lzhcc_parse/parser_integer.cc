#include "lzhcc_parse.h"
#include <charconv>

namespace lzhcc {

auto Parser::integer() -> Expr * {
  auto token = consume(TokenKind::numeric);
  auto literal = context_->storage(token->inner);
  int64_t value;
  std::from_chars(literal.begin(), literal.end(), value);
  if (INT_MIN <= value && value <= INT_MAX) {
    auto i32 = static_cast<int32_t>(value);
    return context_->integer(i32);
  } else {
    return context_->integer(value);
  }
}

} // namespace lzhcc