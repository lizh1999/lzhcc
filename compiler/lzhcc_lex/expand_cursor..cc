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
  expand(cast<ObjectMacro>(macro), consume);
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