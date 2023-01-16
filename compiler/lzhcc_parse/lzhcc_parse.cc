#include "lzhcc_parse.h"

namespace lzhcc {

auto parse(std::span<Token> tokens, Context &context) -> Module {
  Parser parser(tokens.data(), &context);
  return parser();
}

auto const_int(std::span<Token> tokens, Context &context, int64_t *value)
    -> bool {
  Parser parser(tokens.data(), &context);
  return parser.const_int(value);
}

} // namespace lzhcc