#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt(bool is_top) -> Stmt * {
  if (!is_top) {
    entry_scope();
  }
  consume(TokenKind::open_brace);
  std::vector<Stmt *> stmts;
  while (!consume_if(TokenKind::close_brace)) {
    switch (next_kind()) {
    case TokenKind::kw_char:
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
  return context_->block_stmt(std::move(stmts));
}

auto Parser::expr_stmt() -> Stmt * {
  if (consume_if(TokenKind::semi)) {
    return context_->empty_stmt();
  } else {
    auto expr = expression();
    consume(TokenKind::semi);
    return context_->expr_stmt(expr);
  }
}

auto Parser::for_stmt() -> Stmt * {
  consume(TokenKind::kw_for);
  consume(TokenKind::open_paren);
  auto init = expr_stmt();
  Expr *cond = nullptr;
  if (!consume_if(TokenKind::semi)) {
    cond = expression();
    consume(TokenKind::semi);
  }
  Expr *inc = nullptr;
  if (!consume_if(TokenKind::close_paren)) {
    inc = expression();
    consume(TokenKind::close_paren);
  }
  auto then = statement();
  return context_->for_stmt(init, cond, inc, then);
}

auto Parser::if_stmt() -> Stmt * {
  consume(TokenKind::kw_if);
  consume(TokenKind::open_paren);
  auto cond = expression();
  consume(TokenKind::close_paren);
  auto then = statement();
  Stmt *else_ = nullptr;
  if (consume_if(TokenKind::kw_else)) {
    else_ = statement();
  }
  return context_->if_stmt(cond, then, else_);
}

auto Parser::return_stmt() -> Stmt * {
  consume(TokenKind::kw_return);
  auto expr = expression();
  consume(TokenKind::semi);
  return context_->return_stmt(expr);
}

auto Parser::while_stmt() -> Stmt * {
  consume(TokenKind::kw_while);
  consume(TokenKind::open_paren);
  auto cond = expression();
  consume(TokenKind::close_paren);
  auto then = statement();
  return context_->for_stmt(nullptr, cond, nullptr, then);
}

auto Parser::statement() -> Stmt * {
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