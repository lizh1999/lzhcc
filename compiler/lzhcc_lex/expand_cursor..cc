#include "lzhcc_lex.h"
#include <cassert>

namespace lzhcc {

auto ExpandCursor::into(std::vector<Token> tokens) -> Cursor {
  return [i = 0, tokens = std::move(tokens)]() mutable -> Token {
    if (i != tokens.size()) {
      return tokens[i++];
    } else {
      Token token;
      token.kind = TokenKind::eof;
      token.start_of_line = true;
      return token;
    }
  };
}

ExpandCursor::ExpandCursor(Cursor cursor, Context *context)
    : top_token_(cursor()), top_cursor_(std::move(cursor)), context_(context) {}

auto ExpandCursor::operator()() -> Token { return advance(); }

auto ExpandCursor::advance() -> Token {
  if (top_token_.kind == TokenKind::eof) {
    return top_token_;
  }
  auto consume = top_token_;
  if (consume.kind != TokenKind::identifier || consume.expand_disable) {
    advance_top_token();
    return consume;
  }
  auto macro = context_->find_macro(consume.inner);
  if (!macro || macro->expand_disable) {
    advance_top_token();
    consume.expand_disable = true;
    return consume;
  }

  advance_top_token();

  if (macro->kind == MacroKind::object) {
    expand(cast<ObjectMacro>(macro), consume);
    return advance();
  }

  if (top_token_.leading_space || top_token_.kind != TokenKind::open_paren) {
    return consume;
  }

  expand(cast<FunctionMacro>(macro), consume);
  return advance();
}

auto ExpandCursor::expand(ObjectMacro *macro, Token origin) -> void {
  assert(!macro->expand_disable);
  auto replace = macro->replace;
  if (!replace.empty()) {
    auto &first = replace.front();
    first.leading_space = origin.leading_space;
    first.start_of_line = origin.start_of_line;
  }
  return push(macro, into(std::move(replace)));
}

auto ExpandCursor::expand(FunctionMacro *macro, Token origin) -> void {
  assert(!macro->expand_disable);
  advance_top_token();

  std::vector<std::vector<Token>> args;
  if (macro->arg_num == 0) {
    if (top_token_.kind != TokenKind::close_paren) {
      context_->fatal(origin.location, "");
    }
    advance_top_token();
  } else {
    int nested_level = 1;
    std::vector<Token> arg;
    while (true) {
      auto consume = top_token_;
      advance_top_token();
      if (consume.kind == TokenKind::eof) {
        context_->fatal(origin.location, "");
      }
      if (consume.kind == TokenKind::open_paren) {
        ++nested_level;
      } else if (consume.kind == TokenKind::close_paren) {
        --nested_level;
      }
      if (nested_level == 0) {
        args.push_back(std::move(arg));
        break;
      }
      if (1 < nested_level || consume.kind != TokenKind::comma) {
        arg.push_back(consume);
        continue;
      }
      args.push_back(std::move(arg));
    }
  }

  if (args.size() != macro->arg_num) {
    context_->fatal(origin.location, "");
  }

  std::vector<std::vector<Token>> expand(macro->arg_num);
  for (int i = 0; i < args.size(); i++) {
    ExpandCursor cursor(into(args[i]), context_);
    do {
      auto token = cursor();
      expand[i].push_back(token);
    } while (expand[i].back().kind != TokenKind::eof);
  }

  std::vector<Token> result;
  for (auto token : macro->replace) {
    if (token.kind == TokenKind::argument) {
      auto &arg = expand[token.inner];
      int first = result.size();
      result.insert(result.end(), arg.begin(), --arg.end());
      if (arg.size() > 1) {
        result[first].start_of_line = token.start_of_line;
        result[first].leading_space = token.leading_space;
      }
    } else {
      result.push_back(token);
    }
  }
  if (!result.empty()) {
    auto &first = result.front();
    first.leading_space = origin.leading_space;
    first.start_of_line = origin.start_of_line;
  }

  push(macro, into(std::move(result)));
}

auto ExpandCursor::advance_top_token() -> void {
  assert(top_token_.kind != TokenKind::eof);
  top_token_ = top_cursor_();
  while (top_token_.kind == TokenKind::eof && !token_stack_.empty()) {
    assert(macro_stack_.top()->expand_disable);
    macro_stack_.top()->expand_disable = false;
    top_token_ = token_stack_.top();
    top_cursor_ = std::move(cursor_stack_.top());

    macro_stack_.pop();
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

auto ExpandCursor::push(Macro *macro, Cursor cursor) -> void {
  auto cache = top_token_;
  top_token_ = cursor();
  if (top_token_.kind == TokenKind::eof) {
    top_token_ = cache;
    return;
  }
  assert(!macro->expand_disable);
  macro->expand_disable = true;
  macro_stack_.push(macro);
  token_stack_.push(cache);
  cursor_stack_.push(std::move(top_cursor_));
  top_cursor_ = std::move(cursor);
}

} // namespace lzhcc