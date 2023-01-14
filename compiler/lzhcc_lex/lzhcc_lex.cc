#include "lzhcc_lex.h"
#include <stack>

namespace lzhcc {

auto lex(CharCursorFn chars, Context &context) -> std::vector<Token> {
  std::vector<Token> tokens;
  TokenCursor cursor(std::move(chars), &context);
  std::stack<int> stack;
  do {
    auto token = cursor.text();
    switch (token.kind) {
    case TokenKind::identifier:
      token.kind = context.into_keyword(token.inner);
      break;
    case TokenKind::open_brace:
    case TokenKind::open_bracket:
    case TokenKind::open_paren:
      stack.push(tokens.size());
      break;
    case TokenKind::close_bracket:
    case TokenKind::close_brace:
    case TokenKind::close_paren:
      if (stack.empty()) {
        context.fatal(token.location, "");
      }
      tokens[stack.top()].inner = tokens.size() - stack.top();
      stack.pop();
      break;
    default:
      break;
    }
    tokens.push_back(token);
  } while (tokens.back().kind != TokenKind::eof);
  return tokens;
}

} // namespace lzhcc