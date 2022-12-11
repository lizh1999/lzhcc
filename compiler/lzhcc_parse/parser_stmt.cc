#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::statement() -> Statement * {
  auto expr = expression();
  consume(TokenKind::semi);
  return create<ExpressionStmt>(expr);
}

}