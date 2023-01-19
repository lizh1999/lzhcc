#include "lzhcc.h"
#include <cstdlib>
#include <cstring>
#include <sys/wait.h>
#include <unistd.h>

#define push(args, str) args.push_back(const_cast<char *>(str))
#define gcc_lib_path "/home/lizh/riscv/lib/gcc/riscv64-unknown-linux-gnu/11.1.0"
#define lib_path "/home/lizh/riscv/sysroot/usr/lib"
#define sysroot "/home/lizh/riscv/sysroot"

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
  for (int i = 1; i < args.size(); i++) {
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
    if (arg == "-c") {
      result.opt_c = true;
      continue;
    }
    if (arg == "-E") {
      result.opt_E = true;
      continue;
    }
    if (arg == "-o") {
      if (i + 1 == args.size()) {
        usage(EXIT_FAILURE);
      }
      result.opt_o = args[++i];
      continue;
    }

    if (arg == "-I") {
      if (i + 1 == args.size()) {
        usage(EXIT_FAILURE);
      }
      result.include_paths.push_back(args[++i]);
    }

    if (arg.starts_with("-o")) {
      result.opt_o = args[i] + 2;
      continue;
    }

    if (arg.starts_with("-I")) {
      result.include_paths.push_back(args[i] + 2);
      continue;
    }

    if (arg == "-cc1-input") {
      result.base_file = args[++i];
      continue;
    }

    if (arg == "-cc1-output") {
      result.output_file = args[++i];
      continue;
    }

    if (arg[0] == '-' && arg.size() > 1) {
      fprintf(stderr, "unknow argument: %s\n", args[i]);
      exit(EXIT_FAILURE);
    }
    result.input_paths.push_back(args[i]);
  }
  if (result.input_paths.empty()) {
    fprintf(stderr, "no input files.\n");
    exit(EXIT_FAILURE);
  }
  return context;
}

static auto print_token(std::span<Token> tokens, Context &context) {
  FILE *out = stdout;
  using namespace std::literals;
  if (context.arg.opt_o && context.arg.opt_o != "-"sv) {
    out = fopen(context.arg.opt_o, "w");
  }
  for (auto &token : tokens.first(tokens.size() - 1)) {
    auto str = context.to_string(token);
    if (token.start_of_line) {
      fprintf(out, "\n");
    }
    if (token.leading_space) {
      fprintf(out, " ");
    }
    fprintf(out, "%.*s", (int)str.size(), str.data());
  }
  fprintf(out, "\n");
}

static auto cc1(Context &context) {
  auto chars = context.append_file(context.arg.base_file);
  auto tokens = lex(std::move(chars), context);
  if (context.arg.opt_E) {
    return print_token(tokens, context);
  }
  auto tree = parse(tokens, context);
  codegen(tree, context);
}

static auto run_cc1(std::vector<char *> args, Context &context,
                    const char *input, const char *output) {
  push(args, "-cc1");

  if (input) {
    push(args, "-cc1-input");
    args.push_back(const_cast<char *>(input));
  }

  if (output) {
    push(args, "-cc1-output");
    args.push_back(const_cast<char *>(output));
  }

  args.push_back(0);
  run_subprocess(args, context);
}

static auto run_linker(Context &context, std::span<std::string> inputs,
                       const char *output) {
  std::vector<char *> args;
  push(args, "riscv64-unknown-linux-gnu-ld");
  push(args, "-o");
  push(args, output);
  push(args, "-m");
  push(args, "elf64lriscv");
  push(args, "-dynamic-linker");
  push(args, sysroot "/lib/ld-linux-riscv64-lp64d.so.1");
  push(args, lib_path "/crt1.o");
  push(args, gcc_lib_path "/crti.o");
  push(args, gcc_lib_path "/crtbegin.o");
  push(args, "-L" gcc_lib_path);
  push(args, "-L" lib_path);
  push(args, "-L/home/lizh/riscv/riscv64-unknown-linux-gnu/lib");
  push(args, "-L" sysroot "/lib");
  for (auto &input : inputs) {
    push(args, input.c_str());
  }
  push(args, "-lc");
  push(args, "-lgcc");
  push(args, "--as-need");
  push(args, "-lgcc_s");
  push(args, "--no-as-need");
  push(args, gcc_lib_path "/crtend.o");
  push(args, lib_path "/crtn.o");
  args.push_back(0);
  run_subprocess(args, context);
}

static auto assemble(Context &context, const char *input, const char *output) {
  std::vector<char *> args;
  push(args, "riscv64-unknown-linux-gnu-as");
  push(args, "-c");
  push(args, input);
  push(args, "-o");
  push(args, output);
  args.push_back(0);
  run_subprocess(args, context);
}

auto main(std::span<char *> args) -> int {
  static Context context;
  parse_args(args, &context);
  if (context.arg.opt_cc1) {
    context.arg.include_paths.push_back("/home/lizh/riscv/sysroot/usr/include");
    cc1(context);
    return 0;
  }

  if (1 < context.arg.input_paths.size() && context.arg.opt_o &&
      (context.arg.opt_c || context.arg.opt_S || context.arg.opt_E)) {
    fprintf(stderr, "cannot specify '-o' with multiple files\n");
    exit(EXIT_FAILURE);
  }

  std::vector<std::string> ld_args;
  for (auto input : context.arg.input_paths) {
    std::string output;
    if (context.arg.opt_o) {
      output = context.arg.opt_o;
    } else if (context.arg.opt_S) {
      output = replace_extn(input, ".s");
    } else {
      output = replace_extn(input, ".o");
    }

    std::string_view view = input;
    if (view.ends_with(".o")) {
      ld_args.push_back(input);
      continue;
    }

    if (view.ends_with(".s")) {
      if (!context.arg.opt_S) {
        assemble(context, input, output.c_str());
      }
      continue;
    }

    if (!view.ends_with(".c") && view != "-") {
      fprintf(stderr, "unknown file extension: %s\n", input);
      exit(EXIT_FAILURE);
    }

    if (context.arg.opt_S) {
      run_cc1({args.begin(), args.end()}, context, input, output.c_str());
    } else if (context.arg.opt_c) {
      std::string tmpfile = context.create_tmpfile();
      run_cc1({args.begin(), args.end()}, context, input, tmpfile.c_str());
      assemble(context, tmpfile.c_str(), output.c_str());
    } else if (context.arg.opt_E) {
      run_cc1({args.begin(), args.end()}, context, input, 0);
    } else {
      auto tmp1 = context.create_tmpfile();
      auto tmp2 = context.create_tmpfile();
      run_cc1({args.begin(), args.end()}, context, input, tmp1.c_str());
      assemble(context, tmp1.c_str(), tmp2.c_str());
      ld_args.push_back(tmp2);
    }
  }
  if (!ld_args.empty()) {
    run_linker(context, ld_args, context.arg.opt_o ?: "a.out");
  }
  return 0;
}

} // namespace lzhcc