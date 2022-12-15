#include "lzhcc_codegen.h"

namespace lzhcc {

Generator::Generator() : counter(0), return_label(0) {}

auto Generator::codegen(Function *function) -> void {
  auto name = function->name;
  return_label = counter++;
  printf("  .text\n");
  printf("  .globl %.*s\n", (int)name.size(), name.data());
  printf("%.*s:\n", (int)name.size(), name.data());
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  printf("  addi sp, sp, -%d\n", function->stack_size);
  printf("  mv fp, sp\n");

  auto &params = function->params;
  for (int i = 0; i < params.size(); i++) {
    printf("  sd a%d, %d(sp)\n", i, params[i]->offset);
  }

  stmt_proxy(function->stmt);
  printf(".L.return.%d:\n", return_label);
  printf("  addi sp, sp, %d\n", function->stack_size);
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");
  printf("  ret\n");
}

} // namespace lzhcc