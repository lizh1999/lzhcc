#include "test.h"

#define STR(x) #x

int main() {
  ASSERT(4, sizeof(L'\0'));
  ASSERT(97, L'a');

  ASSERT(0, strcmp("αβγ", "\u03B1\u03B2\u03B3"));
  ASSERT(0, strcmp("中文", "\u4E2D\u6587"));
  ASSERT(0, strcmp("中文", "\U00004E2D\U00006587"));
  ASSERT(0, strcmp("🌮", "\U0001F32E"));

  ASSERT(-1, L'\xffffffff'>>31);
  ASSERT(946, L'β');
  ASSERT(21834, L'啊');
  ASSERT(127843, L'🍣');

  ASSERT(2, sizeof(u'\0'));
  ASSERT(1, u'\xffff'>>15);
  ASSERT(97, u'a');
  ASSERT(946, u'β');
  ASSERT(21834, u'啊');
  ASSERT(62307, u'🍣');

  ASSERT(0, strcmp(STR(u'a'), "u'a'"));

  ASSERT(4, sizeof(U'\0'));
  ASSERT(1, U'\xffffffff'>>31);
  ASSERT(97, U'a');
  ASSERT(946, U'β');
  ASSERT(21834, U'啊');
  ASSERT(127843, U'🍣');

  ASSERT(0, strcmp(STR(U'a'), "U'a'"));

  ASSERT(4, sizeof(u8"abc"));
  ASSERT(0, strcmp(u8"abc", "abc"));

  ASSERT(0, strcmp(STR(u8"a"), "u8\"a\""));

  ASSERT(2, sizeof(u""));
  ASSERT(10, sizeof(u"\xffzzz"));
  ASSERT(0, memcmp(u"", "\0\0", 2));
  ASSERT(0, memcmp(u"abc", "a\0b\0c\0\0\0", 8));
  ASSERT(0, memcmp(u"日本語", "\345e,g\236\212\0\0", 8));
  ASSERT(0, memcmp(u"🍣", "<\330c\337\0\0", 6));
  ASSERT(u'β', u"βb"[0]);
  ASSERT(u'b', u"βb"[1]);
  ASSERT(0, u"βb"[2]);

  printf("OK\n");
  return 0;
}