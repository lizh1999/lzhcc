#include "lzhcc_lex.h"
#include <time.h>

namespace lzhcc {

static auto format_date(struct tm *tm) -> const char * {
  static char mon[][4] = {
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
  };
  static char buffer[32];
  sprintf(buffer, "\"%s %2d %d\"", mon[tm->tm_mon], tm->tm_mday,
          tm->tm_year + 1900);
  return buffer;
}

static auto format_time(struct tm *tm) -> const char * {
  static char buffer[32];
  sprintf(buffer, "\"%02d:%02d:%02d\"", tm->tm_hour, tm->tm_min, tm->tm_sec);
  return buffer;
}

auto lex(CharCursor chars, Context &context) -> std::vector<Token> {
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
  context.define_macro("__STDC_UTF_16__", "1");
  context.define_macro("__STDC_UTF_32__", "1");
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

  time_t now = time(nullptr);
  struct tm *tm = localtime(&now);
  context.define_macro("__DATE__", format_date(tm));
  context.define_macro("__TIME__", format_time(tm));

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

auto Context::define_macro(const char *text) -> void {
  int equal = 0;
  while (text[equal] && text[equal] != '=') {
    equal++;
  }
  if (text[equal]) {
    std::string name(text, text + equal);
    define_macro(name.c_str(), text + equal + 1);
  } else {
    define_macro(text, "1");
  }
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