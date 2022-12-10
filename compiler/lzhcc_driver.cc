#include "lzhcc.h"
#include <cassert>

namespace lzhcc {

auto main(std::span<const char *> args) -> int {
  assert(args.size() == 2);
  Context context;
  auto chars = context.append_text(args[1]);
  auto tokens = lex(std::move(chars), context);
  auto tree = parse(tokens, context);
  codegen(tree, context);
  return 0;
}

}