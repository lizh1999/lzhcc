void assert(int, int, const char *);
int printf(const char *, ...);
int sprintf(char *, const char *, ...);
int strcmp(char *, char *);
int memcmp(char *, char *, long);
void exit(int n);

#define ASSERT(x, y) assert(x, y, #y)