#include "lzhcc_parse.h"
#include "lzhcc.h"

namespace lzhcc {

auto parse(std::span<const Token> tokens, Context &context) -> Function * {
  Parser parser(tokens.data(), &context);
  return parser();
}

} // namespace lzhcc