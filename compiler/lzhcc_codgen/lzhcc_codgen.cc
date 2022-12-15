#include "lzhcc.h"
#include "lzhcc_codegen.h"
#include <cstdio>

namespace lzhcc {

auto codegen(Ast &ast, Context &context) -> void {
  StmtGenVisitor gen(&context);
  for (Global *global : ast.globals) {
    auto name = global->name;
    printf("  .data\n");

    if (global->init == -1) {
      printf("%.*s:\n", (int) name.size(), name.data());
      printf("  .zero %d\n", context.size_of(global->type));
    } else {
      printf("  .globl %.*s\n", (int)name.size(), name.data());
      printf("%.*s:\n", (int) name.size(), name.data());
      auto text = context.literal(global->init);
      for (int byte : text) {
        printf("  .byte %d\n", byte);
      }
    }
  }
  for (Function *func : ast.functions) {
    auto name = context.literal(func->name->inner);
    int label = gen.counter++;
    gen.return_label = label;
    printf("  .text\n");
    printf("  .globl %.*s\n", (int)name.size(), name.data());
    printf("%.*s:\n", (int)name.size(), name.data());
    printf("  addi sp, sp, -8\n");
    printf("  sd fp, 0(sp)\n");
    printf("  addi sp, sp, -%d\n", func->max_stack_size);
    printf("  mv fp, sp\n");

    for (int i = 0; i < func->paramters.size(); i++) {
      printf("  sd a%d, %d(sp)\n", i, func->paramters[i]->offset);
    }

    func->stmt->visit(&gen);
    printf(".L.return.%d:\n", label);
    printf("  addi sp, sp, %d\n", func->max_stack_size);
    printf("  ld fp, 0(sp)\n");
    printf("  addi sp, sp, 8\n");
    printf("  ret\n");
  }
}

} // namespace lzhcc