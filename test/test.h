#ifdef __x86_64__
void assert(int, int, const char *);
int printf(const char *, ...);
int strcmp(char *, char *);
int memcmp(char *, char *, long);
#else
void assert(int, int, char *);
int printf(char *);
int strcmp(char *, char *);
int memcmp(char *, char *, long);
#endif

#define ASSERT(x, y) assert(x, y, #y)