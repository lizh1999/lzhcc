#include "lzhcc.h"

auto main(int argc, char *argv[]) -> int {
  return lzhcc::main(std::span(argv, argc));
}