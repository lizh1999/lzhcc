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

static auto replace_extn(std::string tmpl, const char *extn) {
  if (auto pos = tmpl.find_last_of('/'); pos != std::string::npos) {
    tmpl.erase(0, pos + 1);
  }
  if (auto pos = tmpl.find('.'); pos != std::string::npos) {
    tmpl.erase(pos);
  }
  return tmpl + extn;
}

static auto parse_args(std::span<char *> args, Context *context) {
  auto &result = context->arg;
  for (int i = 0; i < args.size(); i++) {
    std::string_view arg = args[i];
    if (arg == "--help") {
      usage(EXIT_SUCCESS);
    }
    if (arg == "-###") {
      result.opt_hash_hash_hash = true;
      continue;
    }
    if (arg == "-cc1") {
      result.opt_cc1 = true;
      continue;
    }
    if (arg == "-S") {
      result.opt_S = true;
      continue;
    }
    if (arg == "-o") {
      if (i + 1 == args.size()) {
        usage(EXIT_FAILURE);
      }
      result.opt_o = args[++i];
      continue;
    }

    if (arg.starts_with("-o")) {
      result.opt_o = args[i] + 2;
      continue;
    }

    if (arg[0] == '-' && arg.size() > 1) {
      fprintf(stderr, "unknow argument: %s\n", args[i]);
      exit(EXIT_FAILURE);
    }
    result.input = args[i];
  }
  if (!result.input) {
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

static auto run_cc1(std::vector<char *> args, Context &context,
                    const char *input, const char *output) {
  static char cc1[] = "-cc1";
  args.push_back(cc1);

  if (input) {
    args.push_back(const_cast<char *>(input));
  }

  if (output) {
    static char o[] = "-o";
    args.push_back(o);
    args.push_back(const_cast<char *>(output));
  }

  args.push_back(0);
  run_subprocess(args, context);
}

static auto assemble(Context &context, const char *input, const char *output) {
  char as[] = "riscv64-unknown-linux-gnu-as", c[] = "-c", o[] = "-o";
  char *cmd[] = {
      as, c, const_cast<char *>(input), o, const_cast<char *>(output), 0,
  };
  run_subprocess(cmd, context);
}

auto main(std::span<char *> args) -> int {
  Context context;
  parse_args(args, &context);
  if (context.arg.opt_cc1) {
    cc1(context);
    return 0;
  }

  std::string output;
  if (context.arg.opt_o) {
    output = context.arg.opt_o;
  } else if (context.arg.opt_S) {
    output = replace_extn(context.arg.input, ".s");
  } else {
    output = replace_extn(context.arg.input, ".o");
  }

  if (context.arg.opt_S) {
    run_cc1({args.begin(), args.end()}, context, context.arg.input,
            output.c_str());
    return 0;
  }

  std::string tmpfile = context.create_tmpfile();
  run_cc1({args.begin(), args.end()}, context, context.arg.input,
          tmpfile.c_str());
  assemble(context, tmpfile.c_str(), output.c_str());
  return 0;
}

} // namespace lzhcc