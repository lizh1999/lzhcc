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
  Calling calling(context_);
  auto pass = calling(function);
  auto &params = function->params;
  println("  mv t4, sp");
  if (function->va_area) {
    int num = Calling::gp_max - calling.gp();
    if (num != 0) {
      println("  addi sp, sp, -%d", 8 * num);
      for (int i = 0, j = calling.gp(); i < num; i++) {
        println("  sd a%d, %d(sp)", j++, i * 8);
      }
      println("  mv t5, sp");
      if (num % 2 == 1) {
        println("  addi sp, sp, -8");
      }
    } else {
      println("  li t5, %d", calling.stack_bytes());
      println("  add t5, sp, t5");
    }
  }

  println("  sd fp, -8(sp)");
  println("  sd ra, -16(sp)");
  println("  addi sp, sp, -16");

  println("  li t0, -%d", align_to(function->stack_size, 16));
  println("  add sp, sp, t0");
  println("  mv fp, sp");

  if (function->va_area) {
    println("  sd t5, %d(sp)", function->va_area->offset);
  }

  auto func_type = cast<FunctionType>(function->type);
  bool huge_object = 16 < context_->size_of(func_type->ret);
  if (huge_object) {
    push("a0");
  }

  auto store_gp = [&](int bytes, int src, int dest) {
    println("  mv t0, a%d", src);
    switch (bytes) {
    case 8:
      return println("  sd t0, %d(sp)", dest);
    case 7:
      println("  slli t1, t0, 48");
      println("  sb t1, %d(sp)", dest + 6);
    case 6:
      println("  slli t1, a0, 32");
      println("  sh t1, %d(sp)", dest + 4);
      return println("  sw t0, %d(sp)", dest);
    case 5:
      println("  slli t1, a0, 32");
      println("  sb t1, %d(sp)", dest + 4);
    case 4:
      return println("  sw t0, %d(sp)", dest);
    case 3:
      println("  slliw t1, t0, 16");
      println("  sb t1, %d(sp)", dest + 1);
    case 2:
      return println("  sh t0, %d(sp)", dest);
    case 1:
      return println("  sb t0, %d(sp)", dest);
    }
  };

  auto store_sp = [&](int bytes, int src, int dest) {
    println("  ld t0, %d(t4)", src * 8);
    switch (bytes) {
    case 8:
      return println("  sd t0, %d(sp)", dest);
    case 7:
      println("  slli t1, t0, 48");
      println("  sb t1, %d(sp)", dest + 6);
    case 6:
      println("  slli t1, a0, 32");
      println("  sh t1, %d(sp)", dest + 4);
      return println("  sw t0, %d(sp)", dest);
    case 5:
      println("  slli t1, a0, 32");
      println("  sb t1, %d(sp)", dest + 4);
    case 4:
      return println("  sw t0, %d(sp)", dest);
    case 3:
      println("  slliw t1, t0, 16");
      println("  sb t1, %d(sp)", dest + 1);
    case 2:
      return println("  sh t0, %d(sp)", dest);
    case 1:
      return println("  sb t0, %d(sp)", dest);
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

  stmt_proxy(function->stmt);
  println(".L.return.%d:", return_label_);

  auto load_gp = [&](IntegerType *type, int src, int dest) {
    switch (type->kind) {
    case IntegerKind::byte:
      return println("  lb a%d, %d(t0)", dest, src);
    case IntegerKind::half:
      return println("  lh a%d, %d(t0)", dest, src);
    case IntegerKind::word:
      return println("  lw a%d, %d(t0)", dest, src);
    case IntegerKind::dword:
      return println("  ld a%d, %d(t0)", dest, src);
    }
  };
  auto load_fp = [&](FloatingType *type, int src, int dest) {
    switch (type->kind) {
    case FloatingKind::float32:
      return println("  flw fa%d, %d(t0)", dest, src);
    case FloatingKind::float64:
      return println("  fld fa%d, %d(t0)", dest, src);
    }
  };
  auto load = [&](Type *type, int src, int dest) {
    assert(dest < 8);
    switch (type->kind) {
    case TypeKind::boolean:
      return println("  lb a%d, %d(t0)", dest, src);
    case TypeKind::integer:
      return load_gp(cast<IntegerType>(type), src, dest);
    case TypeKind::floating:
      return load_fp(cast<FloatingType>(type), src, dest);
    case TypeKind::pointer:
      return println("  ld a%d, %d(t0)", dest, src);
    case TypeKind::function:
    case TypeKind::array:
    case TypeKind::record:
    case TypeKind::kw_void:
      assert(false);
    }
  };

  auto load_size = [&](int size, int src, int dest) {
    assert(dest < 8);
    switch (size) {
    case 5 ... 8:
      return println("  ld a%d, %d(t0)", dest, src);
    case 3 ... 4:
      return println("  lw a%d, %d(t0)", dest, src);
    case 2:
      return println("  lh a%d, %d(t0)", dest, src);
    case 1:
      return println("  lb a%d, %d(t0)", dest, src);
    }
  };

  auto is_float = [](Type *type) { return type->kind == TypeKind::floating; };

  do {
    if (func_type->ret->kind != TypeKind::record) {
      break;
    }
    if (huge_object) {
      println("# pop huge page");
      pop("t1");
    }
    println("  mv t0, a0");
    Type *first = 0, *second = 0;
    int offset = 0, size = context_->size_of(func_type->ret);
    if (dump(context_, func_type->ret, first, second, &offset)) {
      if (!second) {
        load(first, 0, 0);
        break;
      } else if (is_float(first) && is_float(second)) {
        load(first, 0, 0);
        load(second, offset, 1);
        break;
      } else if (!is_float(first) && is_float(second)) {
        load(first, 0, 0);
        load(second, offset, 0);
        break;
      } else if (is_float(first) && !is_float(second)) {
        load(first, 0, 0);
        load(second, offset, 0);
        break;
      }
    }
    if (size <= 8) {
      load_size(size, 0, 0);
    } else if (size <= 16) {
      load_size(8, 0, 0);
      load_size(size - 8, 8, 1);
    } else {
      println("  li t2, %d", size);
      println("  add t2, t2, t0");
      int label = counter_++;
      println(".L.%d:", label);
      println("  lb t3, 0(t0)");
      println("  sb t3, 0(t1)");
      println("  addi t0, t0, 1");
      println("  addi t1, t1, 1");
      println("  bne t0, t2, .L.%d", label);
    }
  } while (false);

  assert(depth_ == 0);
  // sp: 0x40007ffe40
  // sp[-8]: 0x007ffe90

  println("  li t0, %d", align_to(function->stack_size, 16));
  println("  add sp, sp, t0");
  println("  addi sp, sp, 16");
  println("  ld fp, -8(sp)");
  println("  ld ra, -16(sp)");
  if (function->va_area) {
    int num = align_to(Calling::gp_max - calling.gp(), 2);
    if (num != 0) {
      println("  addi sp, sp, %d", num * 8);
    }
  }
  println("  ret");
}

auto Generator::println(const char *fmt, ...) -> void {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(out_, fmt, ap);
  va_end(ap);
  fputc('\n', out_);
}

auto Generator::expect_lvalue() -> void { assert(false); }

} // namespace lzhcc