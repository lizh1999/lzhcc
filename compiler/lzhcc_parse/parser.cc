#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> Expression * {
  auto ret = expression();
  assert(position_->kind == TokenKind::eof);
  return ret;
}

} // namespace lzhcc