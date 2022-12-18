#ifdef __x86_64__
void assert(int, int, const char *);
int printf(const char *, ...);
#else
void assert(int, int, char *);
int printf(char *);
#endif

#define ASSERT(x, y) assert(x, y, #y)