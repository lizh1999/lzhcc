#pragma once

#include "lzhcc.h"
#include <stack>

namespace lzhcc {

class SourceCursor {
  using Cursor = CharCursorFn;

public:
  SourceCursor(Cursor cursor, Context *context);
  auto operator()() -> Token;

private:
  auto white_space() -> void;
  auto new_line() -> void;
  auto line_comment() -> void;
  auto block_comment() -> void;
  auto numeric(std::string text = "") -> Token;
  auto punctuator() -> Token;
  auto identifier() -> Token;
  auto string() -> Token;
  auto character() -> Token;

  auto token(TokenKind kind, int location, int inner = -1) -> Token;

  template <class Pred> auto eat_while(Pred &&pred) -> void;
  auto advance_current() -> void;

  bool leading_space_;
  bool start_of_line_;
  char current_;
  int location_;
  Cursor cursor_;
  Context *context_;
};

class TokenCursor {
  using Cursor = SourceCursor;

public:
  TokenCursor(CharCursorFn cursor, Context *context);

  auto text_fn() -> std::function<Token()>;
  auto text() -> Token;

private:
  const int sb_include;
  const int sb_if;
  const int sb_else;
  const int sb_endif;

  auto advance_top_token() -> void;
  auto skip_line() -> void;
  auto skip_cond() -> void;
  auto include_file() -> void;
  auto handle_if() -> void;

  Token top_token_;
  Cursor top_cursor_;
  std::stack<Token> token_stack_;
  std::stack<Cursor> cursor_stack_;
  std::stack<int> cond_stack_;
  Context *context_;
};

} // namespace lzhcc