void assert(int, int, const char *);
int printf(const char *, ...);
int sprintf(char *, const char *, ...);
int vsprintf(char *buf, const char *fmt, void *ap);
int strcmp(const char *, const char *);
int memcmp(const void *, const void *, long);
void exit(int n);
long strlen (const char *);

#define ASSERT(x, y) assert(x, y, #y)