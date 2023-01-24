void assert(int, int, const char *);
int printf(const char *, ...);
int sprintf(char *, const char *, ...);
int vsprintf(char *buf, char *fmt, void *ap);
int strcmp(const char *, const char *);
int memcmp(const char *, const char *, long);
void exit(int n);

#define ASSERT(x, y) assert(x, y, #y)