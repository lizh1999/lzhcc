#pragma once

#include "lzhcc.h"

namespace lzhcc {

class SourceCursor {
  using Cursor = CharCursorFn;

public:
  SourceCursor(Cursor cursor, Context *context);
  auto operator()() -> Token;

private:
  auto white_space() -> void;
  auto numeric() -> Token;
  auto punctuator() -> Token;
  auto identifier() -> Token;
  auto string() -> Token;

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

} // namespace lzhcc