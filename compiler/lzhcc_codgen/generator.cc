#include "lzhcc_codegen.h"

#include <cassert>
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
  println("  mv t4, sp");
  push("fp");
  push("ra");
  println("  li t0, -%d", align_to(function->stack_size, 16) + 16);
  println("  add sp, sp, t0");
  println("  mv fp, sp");

  Calling calling(context_);
  auto pass = calling(function->params);
  auto &params = function->params;

  auto store_gp = [&](int bytes, int src, int dest) {
    switch (bytes) {
    case 1:
      return println("  sb a%d, %d(sp)", src, dest);
    case 2:
      return println("  sh a%d, %d(sp)", src, dest);
    case 4:
      return println("  sw a%d, %d(sp)", src, dest);
    case 8:
      return println("  sd a%d, %d(sp)", src, dest);
    default:
      assert(false);
    }
  };

  auto store_sp = [&](int bytes, int src, int dest) {
    println("  ld t0, %d(t4)", src * 8);
    switch (bytes) {
    case 1:
      return println("  sb t0, %d(sp)", dest);
    case 2:
      return println("  sh t0, %d(sp)", dest);
    case 4:
      return println("  sw t0, %d(sp)", dest);
    case 8:
      return println("  sd t0, %d(sp)", dest);
    default:
      assert(false);
    }
  };

  auto store_fp = [&](int bytes, int src, int dest) {
    switch (bytes) {
    case 4:
      return println("  fsw fa%d, %d(sp)", src, dest);
    case 8:
      return println("  fsd fa%d, %d(sp)", src, dest);
    default:
      assert(false);
    }
  };

  auto memcpy = [&](int dest, int bytes) {
    println("  li t0, %d", dest);
    println("  li t1, %d", dest + bytes);
    println("  add t1, sp, t1");
    println("  add t0, sp, t0");
    int label = counter_++;
    println(".L.%d:", label);
    println("  lb t2, 0(t3)");
    println("  sb t2, 0(t0)");
    println("  addi t3, t3, 1");
    println("  addi t0, t0, 1");
    println("  bne t0, t1, .L.%d", label);
  };

  for (int i = 0; i < params.size(); i++) {
    auto [kind, inner0, inner1, inner2, inner3, inner4] = pass[i];
    int size = context_->size_of(params[i]->type);
    int offset = params[i]->offset;
    switch (kind) {
    case PassKind::gp:
      store_gp(size, inner0, offset);
      break;
    case PassKind::fp:
      store_fp(size, inner0, offset);
      break;
    case PassKind::sp:
      store_sp(size, inner0, offset);
      break;
    case PassKind::spsp:
      println("  ld t0, %d(t4)", inner0 * 8);
      println("  sd t0, %d(sp)", offset);
      store_sp(size - 8, inner1, offset + 8);
      break;
    case PassKind::gpgp: {
      println("  sd a%d, %d(sp)", inner0, offset);
      store_gp(size - 8, inner1, offset + 8);
      break;
    }
    case PassKind::gpsp: {
      println("  sd a%d, %d(sp)", inner0, offset);
      store_sp(size - 8, inner1, offset + 8);
      break;
    }
    case PassKind::fpfp:
      store_fp(inner2, inner0, offset);
      store_fp(inner3, inner1, offset + inner4);
      break;
    case PassKind::fpgp:
      store_fp(inner2, inner0, offset);
      store_gp(inner3, inner1, offset + inner4);
      break;
    case PassKind::gpfp:
      store_gp(inner2, inner0, offset);
      store_fp(inner3, inner1, offset + inner4);
      break;
    case PassKind::refgp:
      println("  mv t3, a%d", inner1);
      memcpy(offset, size);
      break;
    case PassKind::refsp:
      println("  ld t3, (%d)t4", inner1);
      memcpy(offset, size);
      break;
    }
  }

  if (function->va_area) {
    int i = calling.gp();
    for (int j = 0; i < 8; i++, j += 8) {
      println("  sd a%d, %d(sp)", i, function->va_area->offset + j);
    }
  }

  stmt_proxy(function->stmt);
  println(".L.return.%d:", return_label_);
  println("  li t0, %d", align_to(function->stack_size, 16) + 16);
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