void assert(int, int, char *);
int printf(char *, ...);
int sprintf(char *, char *, ...);
int strcmp(char *, char *);
int memcmp(char *, char *, long);

#define ASSERT(x, y) assert(x, y, #y)