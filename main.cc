#include "lzhcc.h"

auto main(int argc, const char *argv[]) -> int {
  return lzhcc::main(std::span(argv, argc));
}