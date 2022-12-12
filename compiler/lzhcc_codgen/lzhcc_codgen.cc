#include "lzhcc.h"
#include "lzhcc_codegen.h"
#include <cstdio>

namespace lzhcc {

auto codegen(Function *func, Context &context) -> void {
  printf("  .globl main\n");
  printf("main:\n");
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  printf("  addi sp, sp, -%d\n", func->max_stack_size);
  printf("  mv fp, sp\n");
  StmtGenVisitor gen;
  func->stmt->visit(&gen);
  printf(".L.return:\n");
  printf("  mv sp, fp\n");
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");
  printf("  ret\n");
}

}