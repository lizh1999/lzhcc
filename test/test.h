#ifdef __x86_64__
void assert(int, int, const char *);
int printf(const char *, ...);
#endif

#define ASSERT(x, y) assert(x, y, #y)