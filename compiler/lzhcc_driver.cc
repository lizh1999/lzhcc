#include "lzhcc.h"
#include <cstdlib>
#include <cstring>
#include <sys/wait.h>
#include <unistd.h>

namespace lzhcc {

[[noreturn]] static void usage(int status) {
  fprintf(stderr, "lzhcc [ -o <path> ] <file>\n");
  exit(status);
}

static auto run_subprocess(std::span<char *> args, Context &context) {
  if (context.arg.opt_hash_hash_hash) {
    for (auto arg : args) {
      fprintf(stderr, "%s ", arg ?: "\n");
    }
  }

  if (fork() == 0) {
    execvp(args[0], args.data());
    fprintf(stderr, "exec failed: %s: %s\n", args[0], strerror(errno));
    _exit(EXIT_FAILURE);
  }
  int status;
  while (wait(&status) > 1)
    ;
  if (status != 0) {
    exit(EXIT_FAILURE);
  }
}

static auto parse_args(std::span<char *> args) -> Context {
  Context context;
  for (int i = 0; i < args.size(); i++) {
    std::string_view arg = args[i];
    if (arg == "--help") {
      usage(EXIT_SUCCESS);
    }
    if (arg == "-###") {
      context.arg.opt_hash_hash_hash = true;
      continue;
    }
    if (arg == "-cc1") {
      context.arg.opt_cc1 = true;
      continue;
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
    context.arg.input = args[i];
  }
  if (!context.arg.input) {
    fprintf(stderr, "no input files.\n");
    exit(EXIT_FAILURE);
  }
  return context;
}

static auto cc1(Context &context) {
  auto chars = context.append_file(context.arg.input);
  auto tokens = lex(std::move(chars), context);
  auto tree = parse(tokens, context);
  codegen(tree, context);
}

static auto run_cc1(std::vector<char *> args, Context &context) {
  char cc1[] = "-cc1";
  args.push_back(cc1);
  args.push_back(0);
  run_subprocess(args, context);
}

auto main(std::span<char *> args) -> int {
  Context context = parse_args(args);
  if (context.arg.opt_cc1) {
    cc1(context);
  } else {
    run_cc1({args.begin(), args.end()}, context);
  }
  return 0;
}

} // namespace lzhcc