#include "lzhcc_lex.h"
#include <stack>

namespace lzhcc {

auto lex(CharCursorFn chars, Context &context) -> std::vector<Token> {
  context.define_macro("_LP64", "1");
  context.define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
  context.define_macro("__ELF__", "1");
  context.define_macro("__LP64__", "1");
  context.define_macro("__SIZEOF_DOUBLE__", "8");
  context.define_macro("__SIZEOF_FLOAT__", "4");
  context.define_macro("__SIZEOF_INT__", "4");
  context.define_macro("__SIZEOF_LONG_DOUBLE__", "8");
  context.define_macro("__SIZEOF_LONG_LONG__", "8");
  context.define_macro("__SIZEOF_LONG__", "8");
  context.define_macro("__SIZEOF_POINTER__", "8");
  context.define_macro("__SIZEOF_PTRDIFF_T__", "8");
  context.define_macro("__SIZEOF_SHORT__", "2");
  context.define_macro("__SIZEOF_SIZE_T__", "8");
  context.define_macro("__SIZE_TYPE__", "unsigned long");
  context.define_macro("__STDC_HOSTED__", "1");
  context.define_macro("__STDC_NO_ATOMICS__", "1");
  context.define_macro("__STDC_NO_COMPLEX__", "1");
  context.define_macro("__STDC_NO_THREADS__", "1");
  context.define_macro("__STDC_NO_VLA__", "1");
  context.define_macro("__STDC_VERSION__", "201112L");
  context.define_macro("__STDC__", "1");
  context.define_macro("__USER_LABEL_PREFIX__", "");
  context.define_macro("__alignof__", "_Alignof");
  context.define_macro("__lzhcc__", "1");
  context.define_macro("__const__", "const");
  context.define_macro("__gnu_linux__", "1");
  context.define_macro("__inline__", "inline");
  context.define_macro("__linux", "1");
  context.define_macro("__linux__", "1");
  context.define_macro("__signed__", "signed");
  context.define_macro("__typeof__", "typeof");
  context.define_macro("__unix", "1");
  context.define_macro("__unix__", "1");
  context.define_macro("__volatile__", "volatile");
  context.define_macro("linux", "1");
  context.define_macro("unix", "1");
  context.define_macro("__riscv_mul", "1");
  context.define_macro("__riscv_muldiv", "1");
  context.define_macro("__riscv_fdiv", "1");
  context.define_macro("__riscv_xlen", "64");
  context.define_macro("__riscv", "1");
  context.define_macro("__riscv64", "1");
  context.define_macro("__riscv_div", "1");
  context.define_macro("__riscv_float_abi_double", "1");
  context.define_macro("__riscv_flen", "64");

  std::vector<Token> tokens;
  TokenCursor base(std::move(chars), &context);
  ExpandCursor cursor(base.text_fn(), &context);
  std::stack<int> stack;
  do {
    auto token = cursor();
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

auto Context::define_macro(const char *name, const char *text) -> void {
  auto chars = append_text(text, "");
  SourceCursor cursor(chars, this);
  std::vector<Token> replace;
  for (auto token = cursor(); token.kind != TokenKind::eof;) {
    replace.push_back(token);
    token = cursor();
  }
  return object_macro(push_identifier(name), std::move(replace));
}

} // namespace lzhcc