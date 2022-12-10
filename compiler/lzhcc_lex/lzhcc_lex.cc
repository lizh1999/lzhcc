#include "lzhcc_lex.h"

namespace lzhcc {

auto lex(CharCursorFn chars, Context &context) -> std::vector<Token> {
  std::vector<Token> tokens;
  SourceCursor source(std::move(chars), &context);
  do {
    tokens.push_back(source());
  } while (tokens.back().kind != TokenKind::eof);
  return tokens;
}

} // namespace lzhcc