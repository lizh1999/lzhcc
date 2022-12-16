#include "lzhcc_codegen.h"

namespace lzhcc {

auto codegen(Module &module, Context &context) -> void {
  Generator generator(&context);
  for (GValue *gvalue : module.gvalues) {
    generator.codegen(gvalue);
  }
  for (Function *function : module.functions) {
    generator.codegen(function);

  }
}

} // namespace lzhcc