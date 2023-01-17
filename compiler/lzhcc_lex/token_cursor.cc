#include "lzhcc_lex.h"
#include <cassert>
#include <filesystem>

namespace fs = std::filesystem;

namespace lzhcc {

TokenCursor::TokenCursor(CharCursorFn cursor, Context *context)
    : sb_include(context->push_identifier("include")),
      sb_if(context->push_identifier("if")),
      sb_else(context->push_identifier("else")),
      sb_endif(context->push_identifier("endif")), top_cursor_(cursor, context),
      context_(context) {
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
    if (directive == sb_if) {
      cond_stack_.push(token.location);
      handle_if();
      return text();
    }
    if (directive == sb_else) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
      skip_cond();
      return text();
    }
    if (directive == sb_endif) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
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

auto TokenCursor::skip_cond() -> void {
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
    if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if) {
      depth++;
    }
    if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        break;
      }
    }
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

auto TokenCursor::handle_if() -> void {
  std::vector<Token> tokens;
  while (!top_token_.start_of_line) {
    tokens.push_back(top_token_);
    advance_top_token();
  }
  tokens.push_back(Token{
      .kind = TokenKind::eof,
      .start_of_line = true,
  });
  int64_t value;
  if (!const_int(tokens, *context_, &value)) {
    context_->fatal(cond_stack_.top(), "");
  }
  if (value) {
    return;
  }
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
     if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if) {
      ++depth;
      advance_top_token();
      continue;
    }
    if (top_token_.inner == sb_else) {
      advance_top_token();
      if (depth == 1) {
        break;
      }
    } else if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        cond_stack_.pop();
        break;
      }
    }
  }
}

} // namespace lzhcc