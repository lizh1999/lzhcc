#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Statement * {
  consume(TokenKind::open_brace);
  std::vector<Statement *> stmts;
  while (!consume_if(TokenKind::close_brace)) {
    stmts.push_back(statement());
  }
  return create<BlockStmt>(std::move(stmts));
}

auto Parser::if_stmt() -> Statement * {
  consume(TokenKind::kw_if);
  consume(TokenKind::open_paren);
  auto cond = expression();
  consume(TokenKind::close_paren);
  auto than = statement();
  Statement *else_ = nullptr;
  if (consume_if(TokenKind::kw_else)) {
    else_ = statement();
  }
  return create<IfStmt>(cond, than, else_);
}

auto Parser::return_stmt() -> Statement * {
  consume(TokenKind::kw_return);
  auto expr = expression();
  consume(TokenKind::semi);
  return create<ReturnStmt>(expr);
}

auto Parser::statement() -> Statement * {
  switch (next_kind()) {
  case TokenKind::kw_if:
    return if_stmt();
  case TokenKind::kw_return:
    return return_stmt();
  case TokenKind::open_brace:
    return block_stmt();
  case TokenKind::semi:
    consume();
    return create<EmptyStmt>();
  default:
    auto expr = expression();
    consume(TokenKind::semi);
    return create<ExpressionStmt>(expr);
  }
}

} // namespace lzhcc