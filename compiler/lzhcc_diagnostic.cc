#include "lzhcc.h"

#include <cstdio>
#include <cstdlib>

#define RED "\033[1;31m"
#define GREEN "\033[1;32m"
#define NONE "\033[0m"

namespace lzhcc {

void fatal(Diagnostic loc, const char *message) {
  fprintf(stderr, "%.*s:%d:%d: " RED "error: " NONE "%s\n",
          (int)loc.filename.size(), loc.filename.data(), loc.line_number + 1,
          loc.column_number + 1, message);
  fprintf(stderr, "%.*s\n", (int)loc.line.size(), loc.line.data());
  fprintf(stderr, GREEN "%*c" NONE "\n", loc.column_number + 1, '^');
  std::exit(EXIT_FAILURE);
}

} // namespace lzhcc
