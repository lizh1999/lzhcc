#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> Expression * {
  auto ret = expression();
  assert(position_->kind == TokenKind::eof);
  return ret;
}

auto Parser::next_kind() const -> TokenKind { return position_->kind; }

auto Parser::consume() -> Token {
  assert(position_->kind != TokenKind::eof);
  return *position_++;
}

auto Parser::consume(TokenKind kind) -> Token {
  if (position_->kind != kind) {
    context_->fatal(position_->location, "");
  }
  return *position_++;
}

} // namespace lzhcc