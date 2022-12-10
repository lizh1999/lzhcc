#include "lzhcc_codegen.h"
#include <cstdio>

namespace lzhcc {

auto codegen(Expression *expression, Context &context) -> void {
  printf("  .globl main\n");
  printf("main:\n");
  ExprGenVisitor gen;
  expression->visit(&gen);
  printf("  ret\n");
}

}