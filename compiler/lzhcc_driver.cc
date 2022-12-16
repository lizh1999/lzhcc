#include "lzhcc.h"

namespace lzhcc {

[[noreturn]] static void usage(int status) {
  fprintf(stderr, "lzhcc [ -o <path> ] <file>\n");
  exit(status);
}

auto main(std::span<const char *> args) -> int {
  Context context;
  const char *input = nullptr;
  for (int i = 0; i < args.size(); i++) {
    std::string_view arg = args[i];
    if (arg == "--help") {
      usage(EXIT_SUCCESS);
    }
    if (arg == "-o") {
      if (i + 1 == args.size()) {
        usage(EXIT_FAILURE);
      }
      context.arg.opt_o = args[++i];
      continue;
    }

    if (arg.starts_with("-o")) {
      context.arg.opt_o = args[i] + 2;
      continue;
    }

    if (arg[0] == '-' && arg.size() > 1) {
      fprintf(stderr, "unknow argument: %s\n", args[i]);
      exit(EXIT_FAILURE);
    }
    input = args[i];
  }
  if (!input) {
    fprintf(stderr, "no input files.\n");
    exit(EXIT_FAILURE);
  }

  auto chars = context.append_file(input);
  auto tokens = lex(std::move(chars), context);
  auto tree = parse(tokens, context);
  codegen(tree, context);
  return 0;
}

} // namespace lzhcc