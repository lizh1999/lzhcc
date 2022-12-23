#include "lzhcc_parse.h"

namespace lzhcc {

auto Parser::block_stmt() -> Stmt * {
  consume(TokenKind::open_brace);
  std::vector<Stmt *> stmts;
  while (!consume_if(TokenKind::close_brace)) {
    if (is_typename(position_) && position_[1].kind != TokenKind::colon) {
      stmts.push_back(declaration());
    } else {
      stmts.push_back(statement());
    }
  }
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

  auto continue_name = unique_name().first;
  auto continue_label = context_->create_label(continue_name);
  continues_.push(continue_label);

  auto break_name = unique_name().first;
  auto break_label = context_->create_label(break_name);
  breaks_.push(break_label);
  entry_scope();
  Stmt *init = nullptr;
  if (is_typename(position_)) {
    init = declaration();
  } else {
    init = expr_stmt();
  }
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
  Stmt *then = nullptr;
  if (next_is(TokenKind::open_brace)) {
    then = block_stmt();
  } else {
    then = statement();
  }
  leave_scope();
  continues_.pop();
  breaks_.pop();
  return context_->for_stmt(init, cond, inc, then, continue_label, break_label);
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
  expr = context_->cast(ret_, expr);
  consume(TokenKind::semi);
  return context_->return_stmt(expr);
}

auto Parser::while_stmt() -> Stmt * {
  consume(TokenKind::kw_while);
  consume(TokenKind::open_paren);

  auto continue_name = unique_name().first;
  auto continue_label = context_->create_label(continue_name);
  continues_.push(continue_label);

  auto break_name = unique_name().first;
  auto break_label = context_->create_label(break_name);
  breaks_.push(break_label);

  auto cond = expression();
  consume(TokenKind::close_paren);
  auto then = statement();

  continues_.pop();
  breaks_.pop();
  return context_->for_stmt(nullptr, cond, nullptr, then, continue_label,
                            break_label);
}

auto Parser::goto_stmt() -> Stmt * {
  consume(TokenKind::kw_goto);
  auto token = consume(TokenKind::identifier);
  Label *label = nullptr;
  auto it = lable_map_.find(token->inner);
  if (it != lable_map_.end()) {
    label = it->second;
  } else {
    label = context_->create_label();
    lable_map_.emplace(token->inner, label);
  }
  return context_->goto_stmt(label);
}

auto Parser::label_stmt() -> Stmt * {
  auto token = consume(TokenKind::identifier);
  consume(TokenKind::colon);
  Label *label = nullptr;
  auto it = lable_map_.find(token->inner);
  if (it != lable_map_.end()) {
    label = it->second;
  } else {
    label = context_->create_label();
    lable_map_.emplace(token->inner, label);
  }
  if (!label->name.empty()) {
    context_->fatal(token->location, "");
  }
  label->name = unique_name().first;
  return context_->label_stmt(label);
}

auto Parser::switch_stmt() -> Stmt * {
  consume(TokenKind::kw_switch);
  consume(TokenKind::open_paren);
  auto expr = expression();
  consume(TokenKind::close_paren);
  auto break_name = unique_name().first;
  auto break_label = context_->create_label(break_name);
  auto switch_stmt = context_->switch_stmt(expr, break_label);
  switchs_.push(switch_stmt);
  breaks_.push(break_label);
  auto stmt = statement();
  switch_stmt->stmt = stmt;
  switchs_.pop();
  breaks_.pop();
  return switch_stmt;
}

auto Parser::case_stmt() -> Stmt * {
  auto token = consume(TokenKind::kw_case);
  if (int64_t value; !const_int(&value)) {
    context_->fatal(token->location, "");
  } else {
    consume(TokenKind::colon);
    auto stmt = statement();
    auto label_name = unique_name().first;
    auto case_label = context_->create_label(label_name);
    auto current_switch = switchs_.top();
    if (current_switch->expr->type->kind == TypeKind::integer) {
      switch (cast<IntegerType>(current_switch->expr->type)->kind) {
      case IntegerKind::byte:
        value = static_cast<int8_t>(value);
        break;
      case IntegerKind::half:
        value = static_cast<int16_t>(value);
        break;
      case IntegerKind::word:
        value = static_cast<int32_t>(value);
        break;
      case IntegerKind::dword:
        value = static_cast<int64_t>(value);
        break;
      }
    }
    auto case_stmt = context_->case_stmt(stmt, value, case_label);
    current_switch->case_lables.push_back(case_stmt);
    return case_stmt;
  }
}

auto Parser::default_stmt() -> Stmt * {
  consume(TokenKind::kw_default);
  consume(TokenKind::colon);
  auto label_name = unique_name().first;
  auto default_label = context_->create_label(label_name);
  switchs_.top()->default_label = default_label;
  auto stmt = statement();
  return context_->default_stmt(stmt, default_label);
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
  case TokenKind::kw_goto:
    return goto_stmt();
  case TokenKind::kw_break:
    consume();
    consume(TokenKind::semi);
    return context_->goto_stmt(breaks_.top());
  case TokenKind::kw_continue:
    consume();
    consume(TokenKind::semi);
    return context_->goto_stmt(continues_.top());
  case TokenKind::kw_switch:
    return switch_stmt();
  case TokenKind::kw_case:
    return case_stmt();
  case TokenKind::kw_default:
    return default_stmt();
  case TokenKind::open_brace: {
    entry_scope();
    auto result = block_stmt();
    leave_scope();
    return result;
  }
  default:
    return position_[1].kind == TokenKind::colon ? label_stmt() : expr_stmt();
  }
}

} // namespace lzhcc