#include "lzhcc_codegen.h"
#include <cstdio>

namespace lzhcc {

auto codegen(Statement *stmt, Context &context) -> void {
  printf("  .globl main\n");
  printf("main:\n");
  printf("  addi sp, sp, -8\n");
  printf("  sd fp, 0(sp)\n");
  printf("  addi sp, sp, -128\n");
  printf("  mv fp, sp\n");
  StmtGenVisitor gen;
  stmt->visit(&gen);
  printf("  mv sp, fp\n");
  printf("  ld fp, 0(sp)\n");
  printf("  addi sp, sp, 8\n");
  printf("  ret\n");
}

}