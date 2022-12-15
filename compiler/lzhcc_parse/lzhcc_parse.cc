#include "lzhcc_parse.h"

namespace lzhcc {

auto parse(std::span<Token> tokens, Context &context) -> Module {
  Parser parser(tokens.data(), &context);
  return parser();
}

} // namespace lzhcc