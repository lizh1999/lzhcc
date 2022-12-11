#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Statement * {
  consume(TokenKind::open_brace);
  std::vector<Statement *> stmts;
loop:
  switch (next_kind()) {
  case TokenKind::kw_return:
    stmts.push_back(return_stmt());
    goto loop;
  case TokenKind::open_brace:
    stmts.push_back(block_stmt());
    goto loop;
  case TokenKind::semi:
    consume();
    stmts.push_back(create<EmptyStmt>());
    goto loop;
  case TokenKind::close_brace:
    consume();
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