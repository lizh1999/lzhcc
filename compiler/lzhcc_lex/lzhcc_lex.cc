#include "lzhcc_lex.h"

namespace lzhcc {

auto lex(CharCursorFn chars, Context &context) -> std::vector<Token> {
  std::vector<Token> tokens;
  SourceCursor source(std::move(chars), &context);
  do {
    auto token = source();
    if (token.kind == TokenKind::identifier) {
      token.kind = context.into_keyword(token.inner);
    }
    tokens.push_back(token);
  } while (tokens.back().kind != TokenKind::eof);
  return tokens;
}

} // namespace lzhcc