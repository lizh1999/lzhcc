#include "lzhcc_codegen.h"

namespace lzhcc {

auto codegen(Module &module, Context &context) -> void {
  for (GValue *gvalue : module.gvalues) {
    auto name = gvalue->name;
    printf("  .data\n");

    if (gvalue->init == 0) {
      printf("%.*s:\n", (int) name.size(), name.data());
      printf("  .zero %d\n", context.size_of(gvalue->type));
    } else {
      printf("  .globl %.*s\n", (int)name.size(), name.data());
      printf("%.*s:\n", (int) name.size(), name.data());
      int size = context.size_of(gvalue->type);
      for (int i = 0; i < size; i++) {
        printf("  .byte %d\n", (int) gvalue->init[i]);
      }
    }
  }
  Generator generator;
  for (Function *function : module.functions) {
    generator.codegen(function);

  }
}

} // namespace lzhcc