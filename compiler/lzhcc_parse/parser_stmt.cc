#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Statement * {
  std::vector<Statement *> stmts;
  while (next_kind() != TokenKind::eof) {
    auto expr = expression();
    consume(TokenKind::semi);
    stmts.push_back(create<ExpressionStmt>(expr));
  }
  return create<BlockStmt>(std::move(stmts));
}

auto Parser::statement() -> Statement * {
  return block_stmt();
}

}