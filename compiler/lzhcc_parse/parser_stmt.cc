#include "lzhcc.h"
#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Statement * {
  entry_scope();
  consume(TokenKind::open_brace);
  std::vector<Statement *> stmts;
  while (!consume_if(TokenKind::close_brace)) {
    switch (next_kind()) {
    case TokenKind::kw_int: {
      auto init = declaration();
      stmts.insert(stmts.end(), init.begin(), init.end());
      break;
    }
    default:
      stmts.push_back(statement());
    }
  }
  leave_scope();
  return create<BlockStmt>(std::move(stmts));
}

auto Parser::expr_stmt() -> Statement * {
  if (consume_if(TokenKind::semi)) {
    return create<EmptyStmt>();
  } else {
    auto expr = expression();
    consume(TokenKind::semi);
    return create<ExpressionStmt>(expr);
  }
}

auto Parser::for_stmt() -> Statement * {
  consume(TokenKind::kw_for);
  consume(TokenKind::open_paren);
  auto init = expr_stmt();
  Expression *cond = nullptr;
  if (!consume_if(TokenKind::semi)) {
    cond = expression();
    consume(TokenKind::semi);
  }
  Expression *inc = nullptr;
  if (!consume_if(TokenKind::close_paren)) {
    inc = expression();
    consume(TokenKind::close_paren);
  }
  auto then = statement();
  return create<ForStmt>(init, cond, inc, then);
}

auto Parser::if_stmt() -> Statement * {
  consume(TokenKind::kw_if);
  consume(TokenKind::open_paren);
  auto cond = expression();
  consume(TokenKind::close_paren);
  auto then = statement();
  Statement *else_ = nullptr;
  if (consume_if(TokenKind::kw_else)) {
    else_ = statement();
  }
  return create<IfStmt>(cond, then, else_);
}

auto Parser::return_stmt() -> Statement * {
  consume(TokenKind::kw_return);
  auto expr = expression();
  consume(TokenKind::semi);
  return create<ReturnStmt>(expr);
}

auto Parser::while_stmt() -> Statement * {
  consume(TokenKind::kw_while);
  consume(TokenKind::open_paren);
  auto cond = expression();
  consume(TokenKind::close_paren);
  auto then = statement();
  return create<ForStmt>(nullptr, cond, nullptr, then);
}

auto Parser::statement() -> Statement * {
  switch (next_kind()) {
  case TokenKind::kw_for:
    return for_stmt();
  case TokenKind::kw_if:
    return if_stmt();
  case TokenKind::kw_return:
    return return_stmt();
  case TokenKind::kw_while:
    return while_stmt();
  case TokenKind::open_brace:
    return block_stmt();
  default:
    return expr_stmt();
  }
}

} // namespace lzhcc