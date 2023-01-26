#pragma once

#include "lzhcc.h"

namespace lzhcc {

class SourceCursor {
  using Cursor = CharCursor;

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
  auto character(std::string text = "") -> Token;

  auto token(TokenKind kind, int location, int inner = -1) -> Token;

  auto first() -> char;
  auto second() -> char;
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
  TokenCursor(CharCursor cursor, Context *context);

  auto text_fn() -> std::function<Token()>;
  auto text() -> Token;

private:
  const int sb_defined;
  const int sb_define;
  const int sb_undef;
  const int sb_include;
  const int sb_if;
  const int sb_ifdef;
  const int sb_ifndef;
  const int sb_else;
  const int sb_elif;
  const int sb_endif;
  const int sb_error;

  auto advance_top_token() -> void;
  auto skip_line() -> void;
  auto skip_cond() -> void;
  auto include_file(int loc) -> void;
  auto const_int() -> bool;
  auto handle_if() -> void;
  auto handle_ifdef() -> void;
  auto handle_ifndef() -> void;
  auto if_group() -> void;
  auto collect_param(bool *is_variadic) -> std::unordered_map<int, int>;
  auto define_macro() -> void;
  auto remove_macro() -> void;

  Token top_token_;
  Cursor top_cursor_;
  std::stack<Token> token_stack_;
  std::stack<Cursor> cursor_stack_;
  std::stack<int> cond_stack_;
  std::stack<std::string> file_;
  std::stack<int> line_;
  int counter_;
  Context *context_;
};

class ExpandCursor {
  using Cursor = std::function<Token()>;
  static auto into(std::vector<Token> tokens) -> Cursor;

public:
  ExpandCursor(Cursor cursor, Context *context);
  ExpandCursor(std::vector<Token> tokens, Context *context);

  auto operator()() -> Token;

private:
  auto advance() -> Token;
  auto advance_top_token() -> void;
  auto push(Macro *macro, Cursor cursor) -> void;

  auto stringize(std::span<Token> in, Token hash) -> Token;
  auto paste(Token lhs, Token rhs) -> Token;
  auto expand(ObjectMacro *macro, Token origin) -> void;
  auto expand(FunctionMacro *macro, Token origin) -> void;

  Token top_token_;
  Cursor top_cursor_;
  std::stack<Macro *> macro_stack_;
  std::stack<Token> token_stack_;
  std::stack<Cursor> cursor_stack_;
  Context *context_;
};

} // namespace lzhcc