#include "lzhcc_codegen.h"

#include <cstdarg>
#include <cstring>

namespace lzhcc {

Generator::Generator(Context *context)
    : depth_(0), counter_(0), return_label_(0), context_(context) {
  using namespace std::string_view_literals;
  const char *path = context_->arg.output_file;
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
  int size = context_->size_of(gvalue->type);
  if (gvalue->linkage == Linkage::external) {
    println("  .globl %.*s", (int)name.size(), name.data());
  } else {
    println("  .local %.*s", (int)name.size(), name.data());
  }
  println("  .balign %d", gvalue->align_bytes);
  if (gvalue->init == 0) {
    println("  .data");
    println("%.*s:", (int)name.size(), name.data());
    println("  .zero %d", size);
  } else {
    println("  .data");
    println("%.*s:", (int)name.size(), name.data());

    auto &rel = gvalue->relocations;
    for (int i = 0, j = 0; i < size;) {
      if (j < rel.size() && rel[j].index == i) {
        auto name = rel[j].name;
        println("  .dword %.*s + %ld", (int)name.size(), name.data(),
                rel[j].offset);
        i += 8;
        j++;
      } else {
        println("  .byte %d", (int)gvalue->init[i++]);
      }
    }
  }
}

auto Generator::store(Type *type, int &gp, int &fp, int offset) -> void {
  auto integer = [&](IntegerType *type) -> void {
    switch (type->kind) {
    case IntegerKind::byte:
      return println("  sb a%d, %d(sp)", gp++, offset);
    case IntegerKind::half:
      return println("  sh a%d, %d(sp)", gp++, offset);
    case IntegerKind::word:
      return println("  sw a%d, %d(sp)", gp++, offset);
    case IntegerKind::dword:
      return println("  sd a%d, %d(sp)", gp++, offset);
    }
  };
  auto floating = [&](FloatingType *type) -> void {
    IntegerType int32(IntegerKind::word, Sign::sign);
    IntegerType int64(IntegerKind::dword, Sign::sign);
    switch (type->kind) {
    case FloatingKind::float32:
      return fp == 8 ? integer(&int32)
                     : println("  fsw fa%d, %d(sp)", fp++, offset);
    case FloatingKind::float64:
      return fp == 8 ? integer(&int64)
                     : println("  fsd fa%d, %d(sp)", fp++, offset);
    }
  };
  switch (type->kind) {
  case TypeKind::integer:
    return integer(cast<IntegerType>(type));
  case TypeKind::boolean:
    return println("  sb a%d, %d(sp)", gp++, offset);
  case TypeKind::pointer:
    return println("  sd a%d, %d(sp)", gp++, offset);
  case TypeKind::floating:
    return floating(cast<FloatingType>(type));
  case TypeKind::function:
  case TypeKind::array:
  case TypeKind::record:
  case TypeKind::kw_void:
    std::abort();
  }
}

auto Generator::codegen(Function *function) -> void {
  auto name = function->name;
  return_label_ = counter_++;
  println("  .text");
  switch (function->linkage) {
  case Linkage::external:
    println("  .globl %.*s", (int)name.size(), name.data());
    break;
  case Linkage::internal:
    println("  .local %.*s", (int)name.size(), name.data());
    break;
  }

  println("%.*s:", (int)name.size(), name.data());
  push("fp");
  push("ra");
  println("  li t0, -%d", function->stack_size);
  println("  add sp, sp, t0");
  println("  mv fp, sp");

  int gp = 0, fp = 0;
  for (auto param : function->params) {
    store(param->type, gp, fp, param->offset);
  }
  if (function->va_area) {
    int i = function->params.size();
    for (int j = 0; i < 8; i++, j += 8) {
      println("  sd a%d, %d(sp)", i, function->va_area->offset + j);
    }
  }

  stmt_proxy(function->stmt);
  println(".L.return.%d:", return_label_);
  println("  li t0, %d", function->stack_size);
  println("  add sp, sp, t0");
  pop("ra");
  pop("fp");
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