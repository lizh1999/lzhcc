#include "lzhcc_codegen.h"
#include <cstdio>

namespace lzhcc {

auto codegen(Statement *stmt, Context &context) -> void {
  printf("  .globl main\n");
  printf("main:\n");
  StmtGenVisitor gen;
  stmt->visit(&gen);
  printf("  ret\n");
}

}