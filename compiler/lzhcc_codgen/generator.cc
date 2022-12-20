#include "lzhcc_codegen.h"

#include <cstdarg>
#include <cstring>

namespace lzhcc {

Generator::Generator(Context *context)
    : counter(0), return_label(0), context_(context) {
  using namespace std::string_view_literals;
  const char *path = context_->arg.opt_o;
  if (!path || path == "-"sv) {
    out_ = stdout;
  } else {
    out_ = fopen(path, "w");
    if (!out_) {
      fprintf(stderr, "cannot open output file: %s: %s", path, strerror(errno));
    }
  }
}

auto Generator::codegen(GValue *gvalue) -> void {
  auto name = gvalue->name;
  println("  .data");
  int size = context_->size_of(gvalue->type);
  if (gvalue->init == 0) {
    println("%.*s:", (int)name.size(), name.data());
    println("  .zero %d", size);
  } else {
    println("  .globl %.*s", (int)name.size(), name.data());
    println("%.*s:", (int)name.size(), name.data());
    for (int i = 0; i < size; i++) {
      println("  .byte %d", (int)gvalue->init[i]);
    }
  }
}

auto Generator::store_integer(IntegerType *type, int src, int offset) -> void {
  switch (type->kind) {
  case IntegerKind::byte:
    return println("  sb a%d, %d(sp)", src, offset);
  case IntegerKind::half:
    return println("  sh a%d, %d(sp)", src, offset);
  case IntegerKind::word:
    return println("  sw a%d, %d(sp)", src, offset);
  case IntegerKind::dword:
    return println("  sd a%d, %d(sp)", src, offset);
  }
}

auto Generator::store(Type *type, int src, int offset) -> void {
  switch (type->kind) {
  case TypeKind::integer:
    return store_integer(cast<IntegerType>(type), src, offset);
  case TypeKind::boolean:
    return println("  sb a%d, %d(sp)", src, offset);
  case TypeKind::pointer:
    return println("  sd a%d, %d(sp)", src, offset);
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
  case TypeKind::kw_void:
    std::abort();
  }
}

auto Generator::codegen(Function *function) -> void {
  auto name = function->name;
  return_label = counter++;
  println("  .text");
  println("  .globl %.*s", (int)name.size(), name.data());
  println("%.*s:", (int)name.size(), name.data());
  println("  addi sp, sp, -8");
  println("  sd fp, 0(sp)");
  println("  addi sp, sp, -%d", function->stack_size);
  println("  mv fp, sp");

  auto &params = function->params;
  for (int i = 0; i < params.size(); i++) {
    store(params[i]->type, i, params[i]->offset);
  }

  stmt_proxy(function->stmt);
  println(".L.return.%d:", return_label);
  println("  addi sp, sp, %d", function->stack_size);
  println("  ld fp, 0(sp)");
  println("  addi sp, sp, 8");
  println("  ret");
}

auto Generator::println(const char *fmt, ...) -> void {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(out_, fmt, ap);
  va_end(ap);
  fputc('\n', out_);
}

} // namespace lzhcc