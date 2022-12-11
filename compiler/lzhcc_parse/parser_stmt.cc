#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Statement * {
  std::vector<Statement *> stmts;
loop:
  switch (next_kind()) {
  case TokenKind::kw_return:
    stmts.push_back(return_stmt());
    goto loop;
  case TokenKind::eof:
    break;
  default:
    auto expr = expression();
    consume(TokenKind::semi);
    stmts.push_back(create<ExpressionStmt>(expr));
    goto loop;
  }
  return create<BlockStmt>(std::move(stmts));
}

auto Parser::return_stmt() -> Statement * {
  consume(TokenKind::kw_return);
  auto expr = expression();
  consume(TokenKind::semi);
  return create<ReturnStmt>(expr);
}

auto Parser::statement() -> Statement * { return block_stmt(); }

} // namespace lzhcc