#include "lzhcc_lex.h"
#include <cassert>
#include <filesystem>

namespace fs = std::filesystem;

namespace lzhcc {

TokenCursor::TokenCursor(CharCursorFn cursor, Context *context)
    : sb_include(context->push_identifier("include")),
      top_cursor_(cursor, context), context_(context) {
  top_token_ = top_cursor_();
}

auto TokenCursor::text_fn() -> std::function<Token()> {
  return std::bind(&TokenCursor::text, this);
}

auto TokenCursor::text() -> Token {
  if (top_token_.kind == TokenKind::eof) {
    top_token_.start_of_line = true;
    return top_token_;
  }
  auto token = top_token_;
  advance_top_token();
  if (!token.start_of_line || token.kind != TokenKind::hash) {
    return token;
  }
  if (top_token_.start_of_line) {
    return text();
  } else {
    auto token = top_token_;
    advance_top_token();
    if (token.kind != TokenKind::identifier) {
      context_->fatal(token.location, "");
    }
    auto directive = token.inner;
    if (directive == sb_include) {
      include_file();
      return text();
    }
    assert(false);
  }
}

auto TokenCursor::advance_top_token() -> void {
  assert(top_token_.kind != TokenKind::eof);
  top_token_ = top_cursor_();
  while (top_token_.kind == TokenKind::eof && !token_stack_.empty()) {
    top_token_ = token_stack_.top();
    top_cursor_ = cursor_stack_.top();
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

auto TokenCursor::skip_line() -> void {
  if (top_token_.start_of_line) {
    return;
  }
  while (!top_token_.start_of_line) {
    advance_top_token();
  }
}

auto TokenCursor::include_file() -> void {
  auto token = top_token_;
  advance_top_token();
  if (token.kind != TokenKind::string) {
    context_->fatal(token.location, "");
  }
  fs::path path = context_->filename(token.location);
  auto name = context_->storage(token.inner);
  skip_line();
  name.remove_prefix(1);
  name.remove_suffix(1);
  path = path.parent_path() / name;
  if (!fs::exists(path)) {
    context_->fatal(token.location, "");
  }
  cursor_stack_.push(std::move(top_cursor_));
  token_stack_.push(top_token_);
  auto chars = context_->append_file(path);
  SourceCursor cursor(std::move(chars), context_);

  top_cursor_ = std::move(cursor);
  top_token_ = top_cursor_();
  if (top_token_.kind == TokenKind::eof) {
    top_token_ = token_stack_.top();
    top_cursor_ = std::move(cursor_stack_.top());
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

} // namespace lzhcc