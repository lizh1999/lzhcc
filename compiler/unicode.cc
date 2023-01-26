#include "lzhcc.h"

namespace lzhcc {

auto encode_utf8(uint32_t c) -> std::string {
  char buf[8]{};
  if (c <= 0x7F) {
    buf[0] = c;
  } else if (c <= 0x7FF) {
    buf[0] = 0b11000000 | (c >> 6);
    buf[1] = 0b10000000 | (c & 0b00111111);
  } else if (c <= 0xFFFF) {
    buf[0] = 0b11100000 | (c >> 12);
    buf[1] = 0b10000000 | ((c >> 6) & 0b00111111);
    buf[2] = 0b10000000 | (c & 0b00111111);
  } else {
    buf[0] = 0b11110000 | (c >> 18);
    buf[1] = 0b10000000 | ((c >> 12) & 0b00111111);
    buf[2] = 0b10000000 | ((c >> 6) & 0b00111111);
    buf[3] = 0b10000000 | (c & 0b00111111);
  }
  return buf;
}

auto decode_utf8(const char *&ptr) -> uint32_t {
  if (0 <= *ptr) [[likely]] {
    return *ptr++;
  }
  int length = 0;
  uint32_t result = 0;
  if (uint8_t c = *ptr; 0b11110000 <= c) {
    length = 4;
    result = c & 0b1111;
  } else if (0b11100000 <= c) {
    length = 3;
    result = c & 0b11111;
  } else {
    length = 2;
    result = c & 0b11111;
  }
  for (int i = 1; i < length; i++) {
    result = result << 6 | (ptr[i] & 0b111111);
  }
  ptr += length;
  return result;
}

} // namespace lzhcc