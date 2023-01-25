#include "test.h"

int ret3(void) {
  return 3;
  return 5;
}

int add2(int x, int y) { return x + y; }

int sub2(int x, int y) { return x - y; }

int add6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}

int addx(int *x, int y) { return *x + y; }

int sub_char(char a, char b, char c) { return a - b - c; }

int sub_long(long a, long b, long c) { return a - b - c; }

int sub_short(short a, short b, short c) { return a - b - c; }

int fib(int x) {
  if (x <= 1)
    return 1;
  return fib(x - 1) + fib(x - 2);
}

int abs(int);

int g1;

int *g1_ptr(void) { return &g1; }
char int_to_char(int x) { return x; }

int div_long(long a, long b) {
  return a / b;
}

_Bool bool_fn_add(_Bool x) { return x + 1; }
_Bool bool_fn_sub(_Bool x) { return x - 1; }

static int static_fn() { return 3; }

int param_decay(int x[]) { return x[0]; }

int counter() {
  static int i;
  static int j = 1 + 1;
  return i++ + j++;
}

void ret_none() {
  return;
}

int add_all(int n, ...);

typedef void *va_list;

int sprintf(char *buf, const char *fmt, ...);
int vsprintf(char *buf, const char *fmt, va_list ap);

char *fmt(char *buf, const char *fmt, ...) {
  va_list ap = __va_area__;
  vsprintf(buf, fmt, ap);
}

double add_double(double x, double y);
float add_float(float x, float y);

float add_float3(float x, float y, float z) {
  return x + y + z;
}

double add_double3(double x, double y, double z) {
  return x + y + z;
}

int (*fnptr(int (*fn)(int n, ...)))(int, ...) {
  return fn;
}

int param_decay2(int x()) { return x(); }

const char *func_fn(void) {
  return __func__;
}

const char *function_fn(void) {
  return __FUNCTION__;
}

int add10_int(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9, int x10);
float add10_float(float x1, float x2, float x3, float x4, float x5, float x6, float x7, float x8, float x9, float x10);
double add10_double(double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10);

int many_args1(int a, int b, int c, int d, int e, int f, int g, int h) {
  return g / h;
}

double many_args2(double a, double b, double c, double d, double e, double f,
                  double g, double h, double i, double j) {
  return i / j;
}

int many_args3(int a, double b, int c, int d, double e, int f, double g, int h,
               double i, double j, double k, double l, double m, int n, int o,
               double p) {
  return o / p;
}

double many_args4(int a0, int a1, int a2, int a3, int a4, int a5, int a6,
                  int a7, int s1, int s2, double fa0, double fa1, double fa2,
                  double fa3, double fa4, double fa5, double fa6, double fa7,
                  double fs3, double fs4) {
  return s2 / fs4;
}

typedef struct {_Bool a;} StTy1_1;
typedef struct {short a;} StTy1_2;
typedef struct {unsigned a;} StTy1_3;
typedef struct {long a;} StTy1_4;

int struct_type_1_1_test(StTy1_1 x);
int struct_type_1_2_test(StTy1_2 x);
int struct_type_1_3_test(StTy1_3 x);
int struct_type_1_4_test(StTy1_4 x);

typedef struct {char a;char b;char c;char d;char e;char f;char g;char h;} StTy2_1;
typedef struct {int a;int b;} StTy2_2;
typedef struct {unsigned a;unsigned b;} StTy2_3;

int struct_type_2_1_test(StTy2_1 x, int n);
int struct_type_2_2_test(StTy2_2 x, int n);
int struct_type_2_3_test(StTy2_3 x, int n);

typedef struct {char a;char b;char c;char d;int e;} StTy3_1;
typedef struct {char a;char b;char c;int d;} StTy3_2;
typedef struct {char a;short b;char c;short d;} StTy3_3;

int struct_type_3_1_test(StTy3_1 x, int n);
int struct_type_3_2_test(StTy3_2 x, int n);
int struct_type_3_3_test(StTy3_3 x, int n);

typedef struct {char a;char b;char c;char d;int e;char f;int g;} StTy4_1;
typedef struct {char a;char b;char c;int d;char e;} StTy4_2;
typedef struct {char a;short b;char c;short d;char e;short f;} StTy4_3;
typedef struct {char a;short b;char c;short d;int e;short f;char g;} StTy4_4;

int struct_type_4_1_test(StTy4_1 x, int n);
int struct_type_4_2_test(StTy4_2 x, int n);
int struct_type_4_3_test(StTy4_3 x, int n);
int struct_type_4_4_test(StTy4_4 x, int n);

typedef struct {long a;long b;long c;} StTy5_1;
typedef struct {long a;long b;long c;long d;long e;long f;long g;long h;} StTy5_2;

int struct_type_5_1_test(StTy5_1 x, int n);
int struct_type_5_2_test(StTy5_2 x, int n);
int struct_type_5_3_test(StTy5_1 x, StTy5_1 y, int n);
int struct_type_5_4_test(int aa, StTy5_1 x, int bb, StTy5_1 y, int n);

typedef struct {char a;float b;char c;} StTy6_1;

int struct_type_6_1_test(StTy6_1 x, int n);

typedef struct {float a;} StTy7_1;
typedef struct {double a;} StTy7_2;

int struct_type_7_1_test(StTy7_1 x);
int struct_type_7_2_test(StTy7_2 x);

typedef struct {float a;char b;} StTy8_1;
typedef struct {int a;double b;} StTy8_2;
typedef struct {float a;float b;} StTy8_3;
typedef struct {int a;float b;} StTy8_4;

int struct_type_8_1_test(StTy8_1 x, int n);
int struct_type_8_2_test(StTy8_2 x, int n);
int struct_type_8_3_test(StTy8_3 x, int n);
int struct_type_8_4_test(StTy8_4 x, int n);

typedef struct {long a;char b;} StTy9_1;
int struct_type_9_1_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy9_1 x, int n);

int struct_type_10_1_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, StTy9_1 x, int n);

int struct_type_11_1_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_4 x, int n);
int struct_type_11_2_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_2 x, int n);
int struct_type_11_3_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_1 x, int n);

int struct_type_12_1_test(int a0, int a1, int a2, int a3, int a4, int a5, int a6, StTy8_4 x, int n);

typedef struct {float a[2];} StTy13_1;
int struct_type_13_1_test(StTy13_1 x, int n);

typedef union {float a; int b;} UnTy1_1;
typedef union {float a; int b; long c;} UnTy1_2;
int union_type_1_1_test(UnTy1_1 x, int n);
int union_type_1_2_test(UnTy1_2 x, int n);

typedef struct { int a,b; short c; char d; } Ty4;
typedef struct { int a; float b; double c; } Ty5;
typedef struct { unsigned char a[3]; } Ty6;
typedef struct { long a, b, c; } Ty7;

int struct_test5(Ty5 x, int n);
int struct_test4(Ty4 x, int n);
int struct_test6(Ty6 x, int n);
int struct_test7(Ty7 x, int n);

int struct_type_1_1_test_2(StTy1_1 x) { return x.a; }
int struct_type_1_2_test_2(StTy1_2 x) { return x.a; }
int struct_type_1_3_test_2(StTy1_3 x) { return x.a; }
int struct_type_1_4_test_2(StTy1_4 x) { return x.a; }

int struct_type_2_1_test_2(StTy2_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;case 5:return x.f;case 6: return x.g;case 7: return x.h; default: return -1; }}
int struct_type_2_2_test_2(StTy2_2 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}
int struct_type_2_3_test_2(StTy2_3 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}

int struct_type_3_1_test_2(StTy3_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;default: return -1; }}
int struct_type_3_2_test_2(StTy3_2 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;default: return -1; }}
int struct_type_3_3_test_2(StTy3_3 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;default: return -1; }}

int struct_type_4_1_test_2(StTy4_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;case 5: return x.f;case 6: return x.g;default: return -1; }}
int struct_type_4_2_test_2(StTy4_2 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;default: return -1; }}
int struct_type_4_3_test_2(StTy4_3 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;case 5: return x.f;default: return -1; }}
int struct_type_4_4_test_2(StTy4_4 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;case 5: return x.f;case 6: return x.g;default: return -1; }}

int struct_type_5_1_test_2(StTy5_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;default: return -1; }}
int struct_type_5_2_test_2(StTy5_2 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return x.d;case 4: return x.e;case 5:return x.f;case 6: return x.g;case 7: return x.h; default: return -1; }}
int struct_type_5_3_test_2(StTy5_1 x, StTy5_1 y, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return y.a; case 4: return y.b; case 5: return y.c; default: return -1; }}
int struct_type_5_4_test_2(int aa, StTy5_1 x, int bb, StTy5_1 y, int n) {int cc = 123; switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;case 3: return y.a; case 4: return y.b; case 5: return y.c; default: return -1; int dd=456; }}

int struct_type_6_1_test_2(StTy6_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; case 2: return x.c;default: return -1; }}

int struct_type_7_1_test_2(StTy7_1 x) { return x.a; }
int struct_type_7_2_test_2(StTy7_2 x) { return x.a; }

int struct_type_8_1_test_2(StTy8_1 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}
int struct_type_8_2_test_2(StTy8_2 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}
int struct_type_8_3_test_2(StTy8_3 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}
int struct_type_8_4_test_2(StTy8_4 x, int n) {switch(n){case 0: return x.a; case 1: return x.b; default: return -1; }}

int struct_test14(Ty4 x, int n) {switch (n) {case 0:return x.a;case 1:return x.b;case 2:return x.c;default:return x.d;}}
int struct_test15(Ty5 x, int n) {switch (n) {case 0:return x.a;case 1:return x.b;default:return x.c;}}

int struct_type_9_1_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy9_1 x, int n) {switch (n) {case 0:return x.a;case 1:return x.b;default:return -1;}}

int struct_type_10_1_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, StTy9_1 x, int n) { switch (n) { case 0: return x.a; case 1: return x.b; default: return -1; } }

int struct_type_11_1_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_4 x, int n) { switch (n) { case 0: return x.a; case 1: return x.b; default: return -1; } }
int struct_type_11_2_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_2 x, int n) { switch (n) { case 0: return x.a; case 1: return x.b; default: return -1; } }
int struct_type_11_3_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7, StTy8_1 x, int n) { switch (n) { case 0: return x.a; case 1: return x.b; default: return -1; } }

int struct_type_12_1_test_2(int a0, int a1, int a2, int a3, int a4, int a5, int a6, StTy8_4 x, int n) { switch (n) { case 0: return x.a; case 1: return x.b; default: return -1; } }

int union_type_1_1_test_2(UnTy1_1 x, int n) {switch (n) { case 0: return x.a; case 1: return x.b; default: return -1;}}
int union_type_1_2_test_2(UnTy1_2 x, int n) {switch (n) { case 0: return x.a; case 1: return x.b; case 2: return x.c; default: return -1;}}

int struct_type_13_1_test_2(StTy13_1 x, int n) {return x.a[n];}


StTy1_1 struct_type_1_1_test_3();
StTy1_2 struct_type_1_2_test_3();
StTy1_3 struct_type_1_3_test_3();
StTy1_4 struct_type_1_4_test_3();

StTy2_1 struct_type_2_1_test_3();
StTy2_2 struct_type_2_2_test_3();
StTy2_3 struct_type_2_3_test_3();

StTy3_1 struct_type_3_1_test_3();
StTy3_2 struct_type_3_2_test_3();
StTy3_3 struct_type_3_3_test_3();

StTy4_1 struct_type_4_1_test_3();
StTy4_2 struct_type_4_2_test_3();
StTy4_3 struct_type_4_3_test_3();
StTy4_4 struct_type_4_4_test_3();

StTy5_1 struct_type_5_1_test_3();
StTy5_2 struct_type_5_2_test_3();

StTy6_1 struct_type_6_1_test_3();

StTy7_1 struct_type_7_1_test_3();
StTy7_2 struct_type_7_2_test_3();

StTy8_1 struct_type_8_1_test_3();
StTy8_2 struct_type_8_2_test_3();

typedef struct {
  unsigned char a[10];
} Ty20;
typedef struct {
  unsigned char a[20];
} Ty21;
typedef struct {
  long a;
  long b;
  float c;
} Ty22;

Ty4 struct_test24(void);
Ty5 struct_test25(void);
Ty6 struct_test26(void);
Ty20 struct_test27(void);
Ty21 struct_test28(void);
Ty22 struct_test29(void);

StTy1_1 struct_type_1_1_test_4() { return (StTy1_1){10}; }
StTy1_2 struct_type_1_2_test_4() { return (StTy1_2){10}; }
StTy1_3 struct_type_1_3_test_4() { return (StTy1_3){10}; }
StTy1_4 struct_type_1_4_test_4() { return (StTy1_4){10}; }

StTy2_1 struct_type_2_1_test_4() {return (StTy2_1){10,20,30,40,50,60,70,80}; }
StTy2_2 struct_type_2_2_test_4() {return (StTy2_2){10,20};}
StTy2_3 struct_type_2_3_test_4() {return (StTy2_3){10,20};}

StTy3_1 struct_type_3_1_test_4() { return (StTy3_1){10, 20, 30, 40, 50}; }
StTy3_2 struct_type_3_2_test_4() { return (StTy3_2){10, 20, 30, 40}; }
StTy3_3 struct_type_3_3_test_4() { return (StTy3_3){10, 20, 30, 40}; }

StTy4_1 struct_type_4_1_test_4() {return (StTy4_1){10,20,30,40,50,60,70}; }
StTy4_2 struct_type_4_2_test_4() {return (StTy4_2){10,20,30,40,50}; }
StTy4_3 struct_type_4_3_test_4() {return (StTy4_3){10,20,30,40,50,60}; }
StTy4_4 struct_type_4_4_test_4() {return (StTy4_4){10,20,30,40,50,60,70}; }

typedef struct {
  long a;
  short b;
} StTy4_5;
StTy4_5 struct_type_4_5_test_1() {return (StTy4_5){10,20}; }

StTy5_1 struct_type_5_1_test_4() {return (StTy5_1){10,20,30}; }
StTy5_2 struct_type_5_2_test_4() {return (StTy5_2){10,20,30,40,50,60,70,80}; }

StTy6_1 struct_type_6_1_test_4() { return (StTy6_1){10, 20.8, 30}; }

StTy7_1 struct_type_7_1_test_4() { return (StTy7_1){10.8}; }
StTy7_2 struct_type_7_2_test_4() { return (StTy7_2){10.8}; }

StTy8_1 struct_type_8_1_test_4() { return (StTy8_1){10.8,20}; }
StTy8_2 struct_type_8_2_test_4() { return (StTy8_2){10, 20.8}; }

Ty4 struct_test34(void) { return (Ty4){10, 20, 30, 40}; }

Ty5 struct_test35(void) { return (Ty5){10, 20, 30}; }

Ty6 struct_test36(void) { return (Ty6){10, 20, 30}; }

Ty20 struct_test37(void) {
  return (Ty20){10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
}

Ty21 struct_test38(void) {
  return (Ty21){1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
                11, 12, 13, 14, 15, 16, 17, 18, 19, 20};
}


int main() {
  ASSERT(3, ret3());
  ASSERT(8, add2(3, 5));
  ASSERT(2, sub2(5, 3));
  ASSERT(21, add6(1, 2, 3, 4, 5, 6));
  ASSERT(66, add6(1, 2, add6(3, 4, 5, 6, 7, 8), 9, 10, 11));
  ASSERT(136, add6(1, 2, add6(3, add6(4, 5, 6, 7, 8, 9), 10, 11, 12, 13), 14, 15, 16));

  ASSERT(7, add2(3, 4));
  ASSERT(1, sub2(4, 3));
  ASSERT(55, fib(9));

  ASSERT(1, ({ sub_char(7, 3, 3); }));

  ASSERT(1, sub_long(7, 3, 3));
  ASSERT(1, sub_short(7, 3, 3));

  ASSERT(1, abs(-1));

  g1 = 3;

  ASSERT(3, *g1_ptr());
  ASSERT(5, int_to_char(261));
  ASSERT(-5, div_long(-10, 2));

  ASSERT(1, bool_fn_add(3));
  ASSERT(0, bool_fn_sub(3));
  ASSERT(1, bool_fn_add(-3));
  ASSERT(0, bool_fn_sub(-3));
  ASSERT(1, bool_fn_add(0));
  ASSERT(1, bool_fn_sub(0));

  ASSERT(3, static_fn());

  ASSERT(3, ({ int x[2]; x[0]=3; param_decay(x); }));

  ASSERT(2, counter());
  ASSERT(4, counter());
  ASSERT(6, counter());

  ret_none();

  ASSERT(6, add_all(3,1,2,3));
  ASSERT(5, add_all(4,1,2,3,-1));

  ASSERT(0, ({ char buf[100]; sprintf(buf, "%d %d %s", 1, 2, "foo"); strcmp("1 2 foo", buf); }));

  ASSERT(0, ({ char buf[100]; fmt(buf, "%d %d %s", 1, 2, "foo"); strcmp("1 2 foo", buf); }));

  ASSERT(6, add_float(2.3, 3.8));
  ASSERT(6, add_double(2.3, 3.8));

  ASSERT(7, add_float3(2.5, 2.5, 2.5));
  ASSERT(7, add_double3(2.5, 2.5, 2.5));

  ASSERT(0, ({ char buf[100]; sprintf(buf, "%.1f", (float)3.5); strcmp(buf, "3.5"); }));
  ASSERT(0, ({ char buf[100]; fmt(buf, "%.1f", (float)3.5); strcmp(buf, "3.5"); }));

  ASSERT(5, (add2)(2, 3));
  ASSERT(5, (&add2)(2, 3));
  ASSERT(7, ({ int (*fn)(int, int) = add2; fn(2, 5); }));
  ASSERT(6, fnptr(add_all)(3, 1, 2, 3));

  ASSERT(3, param_decay2(ret3));

  ASSERT(5, sizeof(__func__));
  ASSERT(0, strcmp("main", __func__));
  ASSERT(0, strcmp("func_fn", func_fn()));

  ASSERT(0, strcmp("main", __FUNCTION__));
  ASSERT(0, strcmp("function_fn", function_fn()));

  ASSERT(55, add10_int(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  ASSERT(55, add10_float(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  ASSERT(55, add10_double(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

  ASSERT(0, ({ char buf[200]; sprintf(buf, "%d %.1f %.1f %.1f %d %d %.1f %d %d %d %d %.1f %d %d %.1f %.1f %.1f %.1f %d", 1, 1.0, 1.0, 1.0, 1, 1, 1.0, 1, 1, 1, 1, 1.0, 1, 1, 1.0, 1.0, 1.0, 1.0, 1); strcmp("1 1.0 1.0 1.0 1 1 1.0 1 1 1 1 1.0 1 1 1.0 1.0 1.0 1.0 1", buf); }));

  ASSERT(4, many_args1(1, 2, 3, 4, 5, 6, 40, 10));
  ASSERT(4, many_args2(1, 2, 3, 4, 5, 6, 7, 8, 40, 10));
  ASSERT(8, many_args3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 80, 10));
  ASSERT(1, many_args4(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9));

  ASSERT(1, ({StTy1_1 x={1}; struct_type_1_1_test(x);}));
  ASSERT(10, ({StTy1_2 x={10}; struct_type_1_2_test(x);}));
  ASSERT(10, ({StTy1_3 x={10}; struct_type_1_3_test(x);}));
  ASSERT(10, ({StTy1_4 x={10}; struct_type_1_4_test(x);}));

  ASSERT(10, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,0);}));
  ASSERT(20, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,1);}));
  ASSERT(30, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,2);}));
  ASSERT(40, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,3);}));
  ASSERT(50, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,4);}));
  ASSERT(60, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,5);}));
  ASSERT(70, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,6);}));
  ASSERT(80, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test(x,7);}));

  ASSERT(10, ({StTy2_2 x={10,20}; struct_type_2_2_test(x,0);}));
  ASSERT(20, ({StTy2_2 x={10,20}; struct_type_2_2_test(x,1);}));

  ASSERT(10, ({StTy2_3 x={10,20}; struct_type_2_3_test(x,0);}));
  ASSERT(20, ({StTy2_3 x={10,20}; struct_type_2_3_test(x,1);}));

  ASSERT(10, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test(x,0);}));
  ASSERT(20, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test(x,1);}));
  ASSERT(30, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test(x,2);}));
  ASSERT(40, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test(x,3);}));
  ASSERT(50, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test(x,4);}));

  ASSERT(10, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test(x,0);}));
  ASSERT(20, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test(x,1);}));
  ASSERT(30, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test(x,2);}));
  ASSERT(40, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test(x,3);}));

  ASSERT(10, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test(x,0);}));
  ASSERT(20, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test(x,1);}));
  ASSERT(30, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test(x,2);}));
  ASSERT(40, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test(x,3);}));

  ASSERT(10, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,0);}));
  ASSERT(20, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,1);}));
  ASSERT(30, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,2);}));
  ASSERT(40, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,3);}));
  ASSERT(50, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,4);}));
  ASSERT(60, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,5);}));
  ASSERT(70, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test(x,6);}));

  ASSERT(10, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test(x,0);}));
  ASSERT(20, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test(x,1);}));
  ASSERT(30, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test(x,2);}));
  ASSERT(40, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test(x,3);}));
  ASSERT(50, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test(x,4);}));

  ASSERT(10, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,0);}));
  ASSERT(20, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,1);}));
  ASSERT(30, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,2);}));
  ASSERT(40, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,3);}));
  ASSERT(50, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,4);}));
  ASSERT(60, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test(x,5);}));

  ASSERT(10, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,0);}));
  ASSERT(20, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,1);}));
  ASSERT(30, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,2);}));
  ASSERT(40, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,3);}));
  ASSERT(50, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,4);}));
  ASSERT(60, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,5);}));
  ASSERT(70, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test(x,6);}));

  ASSERT(10, ({StTy5_1 x={10,20,30}; struct_type_5_1_test(x,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30}; struct_type_5_1_test(x,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30}; struct_type_5_1_test(x,2);}));

  ASSERT(10, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,0);}));
  ASSERT(20, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,1);}));
  ASSERT(30, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,2);}));
  ASSERT(40, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,3);}));
  ASSERT(50, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,4);}));
  ASSERT(60, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,5);}));
  ASSERT(70, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,6);}));
  ASSERT(80, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test(x,7);}));

  ASSERT(10, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,2);}));
  ASSERT(40, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,3);}));
  ASSERT(50, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,4);}));
  ASSERT(60, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test(x,y,5);}));

  ASSERT(10, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,2);}));
  ASSERT(40, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,3);}));
  ASSERT(50, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,4);}));
  ASSERT(60, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test(1,x,2,y,5);}));

  ASSERT(10, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test(x,0);}));
  ASSERT(20, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test(x,1);}));
  ASSERT(30, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test(x,2);}));

  ASSERT(10, ({StTy7_1 x={10.34}; struct_type_7_1_test(x);}));
  ASSERT(10, ({StTy7_2 x={10.34}; struct_type_7_2_test(x);}));

  ASSERT(10, ({StTy8_1 x={10.88,20}; struct_type_8_1_test(x,0);}));
  ASSERT(20, ({StTy8_1 x={10.88,20}; struct_type_8_1_test(x,1);}));

  ASSERT(10, ({StTy8_2 x={10,20.88}; struct_type_8_2_test(x,0);}));
  ASSERT(20, ({StTy8_2 x={10,20.88}; struct_type_8_2_test(x,1);}));

  ASSERT(10, ({StTy8_3 x={10.88, 20.88}; struct_type_8_3_test(x,0);}));
  ASSERT(20, ({StTy8_3 x={10.88, 20.88}; struct_type_8_3_test(x,1);}));

  ASSERT(10, ({StTy8_4 x={10, 20.88}; struct_type_8_4_test(x,0);}));
  ASSERT(20, ({StTy8_4 x={10, 20.88}; struct_type_8_4_test(x,1);}));

  ASSERT(10, ({StTy9_1 x={10,20}; struct_type_9_1_test(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy9_1 x={10,20}; struct_type_9_1_test(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy9_1 x={10,20}; struct_type_10_1_test(0,1,2,3,4,5,6,x,0);}));
  ASSERT(20, ({StTy9_1 x={10,20}; struct_type_10_1_test(0,1,2,3,4,5,6,x,1);}));

  ASSERT(10, ({StTy8_4 x={10,20}; struct_type_11_1_test(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_4 x={10,20}; struct_type_11_1_test(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_2 x={10,20}; struct_type_11_2_test(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_2 x={10,20}; struct_type_11_2_test(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_1 x={10,20}; struct_type_11_3_test(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_1 x={10,20}; struct_type_11_3_test(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_4 x={10,20}; struct_type_12_1_test(0,1,2,3,4,5,6,x,0);}));
  ASSERT(20, ({StTy8_4 x={10,20}; struct_type_12_1_test(0,1,2,3,4,5,6,x,1);}));

  ASSERT(10, ({StTy13_1 x={10,20}; struct_type_13_1_test(x,0);}));
  ASSERT(20, ({StTy13_1 x={10,20}; struct_type_13_1_test(x,1);}));

  ASSERT(10, ({UnTy1_1 x; x.a=10.0; union_type_1_1_test(x,0);}));
  ASSERT(20, ({UnTy1_1 x; x.b=20.0; union_type_1_1_test(x,1);}));
  ASSERT(10, ({UnTy1_2 x; x.a=10.0; union_type_1_2_test(x,0);}));
  ASSERT(20, ({UnTy1_2 x; x.b=20.0; union_type_1_2_test(x,1);}));
  ASSERT(30, ({UnTy1_2 x; x.c=30.0; union_type_1_2_test(x,2);}));

  ASSERT(10, ({ Ty4 x={10,20,30,40}; struct_test4(x, 0); }));
  ASSERT(20, ({ Ty4 x={10,20,30,40}; struct_test4(x, 1); }));
  ASSERT(30, ({ Ty4 x={10,20,30,40}; struct_test4(x, 2); }));
  ASSERT(40, ({ Ty4 x={10,20,30,40}; struct_test4(x, 3); }));

  ASSERT(10, ({ Ty5 x={10,20,30}; struct_test5(x, 0); }));
  ASSERT(20, ({ Ty5 x={10,20,30}; struct_test5(x, 1); }));
  ASSERT(30, ({ Ty5 x={10,20,30}; struct_test5(x, 2); }));

  ASSERT(10, ({ Ty6 x={10,20,30}; struct_test6(x, 0); }));
  ASSERT(20, ({ Ty6 x={10,20,30}; struct_test6(x, 1); }));
  ASSERT(30, ({ Ty6 x={10,20,30}; struct_test6(x, 2); }));

  ASSERT(10, ({ Ty7 x={10,20,30}; struct_test7(x, 0); }));
  ASSERT(20, ({ Ty7 x={10,20,30}; struct_test7(x, 1); }));
  ASSERT(30, ({ Ty7 x={10,20,30}; struct_test7(x, 2); }));

  ASSERT(1,  ({StTy1_1 x={1}; struct_type_1_1_test_2(x);}));
  ASSERT(10, ({StTy1_2 x={10}; struct_type_1_2_test_2(x);}));
  ASSERT(10, ({StTy1_3 x={10}; struct_type_1_3_test_2(x);}));
  ASSERT(10, ({StTy1_4 x={10}; struct_type_1_4_test_2(x);}));

  ASSERT(10, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,0);}));
  ASSERT(20, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,1);}));
  ASSERT(30, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,2);}));
  ASSERT(40, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,3);}));
  ASSERT(50, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,4);}));
  ASSERT(60, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,5);}));
  ASSERT(70, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,6);}));
  ASSERT(80, ({StTy2_1 x={10,20,30,40,50,60,70,80}; struct_type_2_1_test_2(x,7);}));

  ASSERT(10, ({StTy2_2 x={10,20}; struct_type_2_2_test_2(x,0);}));
  ASSERT(20, ({StTy2_2 x={10,20}; struct_type_2_2_test_2(x,1);}));

  ASSERT(10, ({StTy2_3 x={10,20}; struct_type_2_3_test_2(x,0);}));
  ASSERT(20, ({StTy2_3 x={10,20}; struct_type_2_3_test_2(x,1);}));

  ASSERT(10, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test_2(x,0);}));
  ASSERT(20, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test_2(x,1);}));
  ASSERT(30, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test_2(x,2);}));
  ASSERT(40, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test_2(x,3);}));
  ASSERT(50, ({StTy3_1 x={10,20,30,40,50}; struct_type_3_1_test_2(x,4);}));

  ASSERT(10, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test_2(x,0);}));
  ASSERT(20, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test_2(x,1);}));
  ASSERT(30, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test_2(x,2);}));
  ASSERT(40, ({StTy3_2 x={10,20,30,40}; struct_type_3_2_test_2(x,3);}));

  ASSERT(10, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test_2(x,0);}));
  ASSERT(20, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test_2(x,1);}));
  ASSERT(30, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test_2(x,2);}));
  ASSERT(40, ({StTy3_3 x={10,20,30,40}; struct_type_3_3_test_2(x,3);}));

  ASSERT(10, ({StTy4_1 x={10,20,30,40,50,60,70};struct_type_4_1_test_2(x,0);}));
  ASSERT(20, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test_2(x,1);}));
  ASSERT(30, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test_2(x,2);}));
  ASSERT(40, ({StTy4_1 x={10,20,30,40,50,60,70};struct_type_4_1_test_2(x,3);}));
  ASSERT(50, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test_2(x,4);}));
  ASSERT(60, ({StTy4_1 x={10,20,30,40,50,60,70}; struct_type_4_1_test_2(x,5);}));
  ASSERT(70, ({StTy4_1 x={10,20,30,40,50,60,70};struct_type_4_1_test_2(x,6);}));

  ASSERT(10, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test_2(x,0);}));
  ASSERT(20, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test_2(x,1);}));
  ASSERT(30, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test_2(x,2);}));
  ASSERT(40, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test_2(x,3);}));
  ASSERT(50, ({StTy4_2 x={10,20,30,40,50}; struct_type_4_2_test_2(x,4);}));

  ASSERT(10, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,0);}));
  ASSERT(20, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,1);}));
  ASSERT(30, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,2);}));
  ASSERT(40, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,3);}));
  ASSERT(50, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,4);}));
  ASSERT(60, ({StTy4_3 x={10,20,30,40,50,60}; struct_type_4_3_test_2(x,5);}));

  ASSERT(10, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,0);}));
  ASSERT(20, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,1);}));
  ASSERT(30, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,2);}));
  ASSERT(40, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,3);}));
  ASSERT(50, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,4);}));
  ASSERT(60, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,5);}));
  ASSERT(70, ({StTy4_4 x={10,20,30,40,50,60,70}; struct_type_4_4_test_2(x,6);}));

  ASSERT(10, ({StTy5_1 x={10,20,30}; struct_type_5_1_test_2(x,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30}; struct_type_5_1_test_2(x,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30}; struct_type_5_1_test_2(x,2);}));

  ASSERT(10, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,0);}));
  ASSERT(20, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,1);}));
  ASSERT(30, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,2);}));
  ASSERT(40, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,3);}));
  ASSERT(50, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,4);}));
  ASSERT(60, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,5);}));
  ASSERT(70, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,6);}));
  ASSERT(80, ({StTy5_2 x={10,20,30,40,50,60,70,80}; struct_type_5_2_test_2(x,7);}));

  ASSERT(10, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,2);}));
  ASSERT(40, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,3);}));
  ASSERT(50, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,4);}));
  ASSERT(60, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_3_test_2(x,y,5);}));

  ASSERT(10, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,0);}));
  ASSERT(20, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,1);}));
  ASSERT(30, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,2);}));
  ASSERT(40, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,3);}));
  ASSERT(50, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,4);}));
  ASSERT(60, ({StTy5_1 x={10,20,30};StTy5_1 y={40,50,60}; struct_type_5_4_test_2(1,x,2,y,5);}));

  ASSERT(10, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test_2(x,0);}));
  ASSERT(20, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test_2(x,1);}));
  ASSERT(30, ({StTy6_1 x={10,20.88,30}; struct_type_6_1_test_2(x,2);}));

  ASSERT(10, ({StTy7_1 x={10.34}; struct_type_7_1_test_2(x);}));
  ASSERT(10, ({StTy7_2 x={10.34}; struct_type_7_2_test_2(x);}));

  ASSERT(10, ({StTy8_1 x={10.88,20}; struct_type_8_1_test_2(x,0);}));
  ASSERT(20, ({StTy8_1 x={10.88,20}; struct_type_8_1_test_2(x,1);}));

  ASSERT(10, ({StTy8_2 x={10,20.88}; struct_type_8_2_test_2(x,0);}));
  ASSERT(20, ({StTy8_2 x={10,20.88}; struct_type_8_2_test_2(x,1);}));

  ASSERT(10, ({StTy8_3 x={10.88,20.88}; struct_type_8_3_test_2(x,0);}));
  ASSERT(20, ({StTy8_3 x={10.88,20.88}; struct_type_8_3_test_2(x,1);}));

  ASSERT(10, ({StTy8_4 x={10,20.88}; struct_type_8_4_test_2(x,0);}));
  ASSERT(20, ({StTy8_4 x={10,20.88}; struct_type_8_4_test_2(x,1);}));

  ASSERT(10, ({StTy9_1 x={10,20}; struct_type_9_1_test(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy9_1 x={10,20}; struct_type_9_1_test(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy9_1 x={10,20}; struct_type_10_1_test_2(0,1,2,3,4,5,6,x,0);}));
  ASSERT(20, ({StTy9_1 x={10,20}; struct_type_10_1_test_2(0,1,2,3,4,5,6,x,1);}));

  ASSERT(10, ({StTy8_4 x={10,20}; struct_type_11_1_test_2(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_4 x={10,20}; struct_type_11_1_test_2(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_2 x={10,20}; struct_type_11_2_test_2(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_2 x={10,20}; struct_type_11_2_test_2(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_1 x={10,20}; struct_type_11_3_test_2(0,1,2,3,4,5,6,7,x,0);}));
  ASSERT(20, ({StTy8_1 x={10,20}; struct_type_11_3_test_2(0,1,2,3,4,5,6,7,x,1);}));

  ASSERT(10, ({StTy8_4 x={10,20}; struct_type_12_1_test_2(0,1,2,3,4,5,6,x,0);}));
  ASSERT(20, ({StTy8_4 x={10,20}; struct_type_12_1_test_2(0,1,2,3,4,5,6,x,1);}));

  ASSERT(10, ({StTy13_1 x={10,20}; struct_type_13_1_test_2(x,0);}));
  ASSERT(20, ({StTy13_1 x={10,20}; struct_type_13_1_test_2(x,1);}));

  ASSERT(10, ({UnTy1_1 x; x.a=10.0; union_type_1_1_test_2(x,0);}));
  ASSERT(20, ({UnTy1_1 x; x.b=20.0; union_type_1_1_test_2(x,1);}));
  ASSERT(10, ({UnTy1_2 x; x.a=10.0; union_type_1_2_test_2(x,0);}));
  ASSERT(20, ({UnTy1_2 x; x.b=20.0; union_type_1_2_test_2(x,1);}));
  ASSERT(30, ({UnTy1_2 x; x.c=30.0; union_type_1_2_test_2(x,2);}));

  ASSERT(10, ({ Ty4 x={10,20,30,40}; struct_test14(x, 0); }));
  ASSERT(20, ({ Ty4 x={10,20,30,40}; struct_test14(x, 1); }));
  ASSERT(30, ({ Ty4 x={10,20,30,40}; struct_test14(x, 2); }));
  ASSERT(40, ({ Ty4 x={10,20,30,40}; struct_test14(x, 3); }));

  ASSERT(10, ({ Ty5 x={10,20,30}; struct_test15(x, 0); }));
  ASSERT(20, ({ Ty5 x={10,20,30}; struct_test15(x, 1); }));
  ASSERT(30, ({ Ty5 x={10,20,30}; struct_test15(x, 2); }));

  ASSERT(1, ({ struct_type_1_1_test_3().a; }));
  ASSERT(10, ({ struct_type_1_2_test_3().a; }));
  ASSERT(10, ({ struct_type_1_3_test_3().a; }));
  ASSERT(10, ({ struct_type_1_4_test_3().a; }));

  ASSERT(10, ({ struct_type_2_1_test_3().a; }));
  ASSERT(20, ({ struct_type_2_1_test_3().b; }));
  ASSERT(30, ({ struct_type_2_1_test_3().c; }));
  ASSERT(40, ({ struct_type_2_1_test_3().d; }));
  ASSERT(50, ({ struct_type_2_1_test_3().e; }));
  ASSERT(60, ({ struct_type_2_1_test_3().f; }));
  ASSERT(70, ({ struct_type_2_1_test_3().g; }));
  ASSERT(80, ({ struct_type_2_1_test_3().h; }));

  ASSERT(10, ({ struct_type_2_2_test_3().a; }));
  ASSERT(20, ({ struct_type_2_2_test_3().b; }));

  ASSERT(10, ({ struct_type_2_3_test_3().a; }));
  ASSERT(20, ({ struct_type_2_3_test_3().b; }));

  ASSERT(10, ({ struct_type_3_1_test_3().a; }));
  ASSERT(20, ({ struct_type_3_1_test_3().b; }));
  ASSERT(30, ({ struct_type_3_1_test_3().c; }));
  ASSERT(40, ({ struct_type_3_1_test_3().d; }));
  ASSERT(50, ({ struct_type_3_1_test_3().e; }));

  ASSERT(10, ({ struct_type_3_2_test_3().a; }));
  ASSERT(20, ({ struct_type_3_2_test_3().b; }));
  ASSERT(30, ({ struct_type_3_2_test_3().c; }));
  ASSERT(40, ({ struct_type_3_2_test_3().d; }));

  ASSERT(10, ({ struct_type_3_3_test_3().a; }));
  ASSERT(20, ({ struct_type_3_3_test_3().b; }));
  ASSERT(30, ({ struct_type_3_3_test_3().c; }));
  ASSERT(40, ({ struct_type_3_3_test_3().d; }));

  ASSERT(10, ({ struct_type_4_1_test_3().a; }));
  ASSERT(20, ({ struct_type_4_1_test_3().b; }));
  ASSERT(30, ({ struct_type_4_1_test_3().c; }));
  ASSERT(40, ({ struct_type_4_1_test_3().d; }));
  ASSERT(50, ({ struct_type_4_1_test_3().e; }));
  ASSERT(60, ({ struct_type_4_1_test_3().f; }));
  ASSERT(70, ({ struct_type_4_1_test_3().g; }));

  ASSERT(10, ({ struct_type_4_2_test_3().a; }));
  ASSERT(20, ({ struct_type_4_2_test_3().b; }));
  ASSERT(30, ({ struct_type_4_2_test_3().c; }));
  ASSERT(40, ({ struct_type_4_2_test_3().d; }));
  ASSERT(50, ({ struct_type_4_2_test_3().e; }));

  ASSERT(10, ({ struct_type_4_3_test_3().a; }));
  ASSERT(20, ({ struct_type_4_3_test_3().b; }));
  ASSERT(30, ({ struct_type_4_3_test_3().c; }));
  ASSERT(40, ({ struct_type_4_3_test_3().d; }));
  ASSERT(50, ({ struct_type_4_3_test_3().e; }));
  ASSERT(60, ({ struct_type_4_3_test_3().f; }));

  ASSERT(10, ({ struct_type_4_4_test_3().a; }));
  ASSERT(20, ({ struct_type_4_4_test_3().b; }));
  ASSERT(30, ({ struct_type_4_4_test_3().c; }));
  ASSERT(40, ({ struct_type_4_4_test_3().d; }));
  ASSERT(50, ({ struct_type_4_4_test_3().e; }));
  ASSERT(60, ({ struct_type_4_4_test_3().f; }));
  ASSERT(70, ({ struct_type_4_4_test_3().g; }));

  ASSERT(10, ({ struct_type_5_1_test_3().a; }));
  ASSERT(20, ({ struct_type_5_1_test_3().b; }));
  ASSERT(30, ({ struct_type_5_1_test_3().c; }));

  ASSERT(10, ({ struct_type_5_2_test_3().a; }));
  ASSERT(20, ({ struct_type_5_2_test_3().b; }));
  ASSERT(30, ({ struct_type_5_2_test_3().c; }));
  ASSERT(40, ({ struct_type_5_2_test_3().d; }));
  ASSERT(50, ({ struct_type_5_2_test_3().e; }));
  ASSERT(60, ({ struct_type_5_2_test_3().f; }));
  ASSERT(70, ({ struct_type_5_2_test_3().g; }));
  ASSERT(80, ({ struct_type_5_2_test_3().h; }));

  ASSERT(10, ({ struct_type_6_1_test_3().a; }));
  ASSERT(20, ({ struct_type_6_1_test_3().b; }));
  ASSERT(30, ({ struct_type_6_1_test_3().c; }));

  ASSERT(10, ({ struct_type_7_1_test_3().a; }));
  ASSERT(10, ({ struct_type_7_2_test_3().a; }));

  ASSERT(10, ({ struct_type_8_1_test_3().a; }));
  ASSERT(20, ({ struct_type_8_1_test_3().b; }));

  ASSERT(10, ({ struct_type_8_2_test_3().a; }));
  ASSERT(20, ({ struct_type_8_2_test_3().b; }));

  ASSERT(10, struct_test24().a);
  ASSERT(20, struct_test24().b);
  ASSERT(30, struct_test24().c);
  ASSERT(40, struct_test24().d);

  ASSERT(10, struct_test25().a);
  ASSERT(20, struct_test25().b);
  ASSERT(30, struct_test25().c);

  ASSERT(10, struct_test26().a[0]);
  ASSERT(20, struct_test26().a[1]);
  ASSERT(30, struct_test26().a[2]);

  ASSERT(10, struct_test27().a[0]);
  ASSERT(20, struct_test27().a[1]);
  ASSERT(30, struct_test27().a[2]);
  ASSERT(40, struct_test27().a[3]);
  ASSERT(50, struct_test27().a[4]);
  ASSERT(60, struct_test27().a[5]);
  ASSERT(70, struct_test27().a[6]);
  ASSERT(80, struct_test27().a[7]);
  ASSERT(90, struct_test27().a[8]);
  ASSERT(100, struct_test27().a[9]);

  ASSERT(1, struct_test28().a[0]);
  ASSERT(2, struct_test28().a[1]);
  ASSERT(3, struct_test28().a[2]);
  ASSERT(4, struct_test28().a[3]);
  ASSERT(5, struct_test28().a[4]);
  ASSERT(6, struct_test28().a[5]);
  ASSERT(7, struct_test28().a[6]);
  ASSERT(8, struct_test28().a[7]);
  ASSERT(9, struct_test28().a[8]);
  ASSERT(10, struct_test28().a[9]);
  ASSERT(11, struct_test28().a[10]);
  ASSERT(12, struct_test28().a[11]);
  ASSERT(13, struct_test28().a[12]);
  ASSERT(14, struct_test28().a[13]);
  ASSERT(15, struct_test28().a[14]);
  ASSERT(16, struct_test28().a[15]);
  ASSERT(17, struct_test28().a[16]);
  ASSERT(18, struct_test28().a[17]);
  ASSERT(19, struct_test28().a[18]);
  ASSERT(20, struct_test28().a[19]);

  ASSERT(10, struct_test29().a);
  ASSERT(20, struct_test29().b);
  ASSERT(30, struct_test29().c);

  ASSERT(10, struct_test34().a);
  ASSERT(20, struct_test34().b);
  ASSERT(30, struct_test34().c);
  ASSERT(40, struct_test34().d);

  ASSERT(10, struct_test35().a);
  ASSERT(20, struct_test35().b);
  ASSERT(30, struct_test35().c);

  ASSERT(10, struct_test36().a[0]);
  ASSERT(20, struct_test36().a[1]);
  ASSERT(30, struct_test36().a[2]);

  ASSERT(10, struct_test37().a[0]);
  ASSERT(20, struct_test37().a[1]);
  ASSERT(30, struct_test37().a[2]);
  ASSERT(40, struct_test37().a[3]);
  ASSERT(50, struct_test37().a[4]);
  ASSERT(60, struct_test37().a[5]);
  ASSERT(70, struct_test37().a[6]);
  ASSERT(80, struct_test37().a[7]);
  ASSERT(90, struct_test37().a[8]);
  ASSERT(100, struct_test37().a[9]);

  ASSERT(1, struct_test38().a[0]);
  ASSERT(2, struct_test38().a[1]);
  ASSERT(3, struct_test38().a[2]);
  ASSERT(4, struct_test38().a[3]);
  ASSERT(5, struct_test38().a[4]);
  ASSERT(6, struct_test38().a[5]);
  ASSERT(7, struct_test38().a[6]);
  ASSERT(8, struct_test38().a[7]);
  ASSERT(9, struct_test38().a[8]);
  ASSERT(10, struct_test38().a[9]);
  ASSERT(11, struct_test38().a[10]);
  ASSERT(12, struct_test38().a[11]);
  ASSERT(13, struct_test38().a[12]);
  ASSERT(14, struct_test38().a[13]);
  ASSERT(15, struct_test38().a[14]);
  ASSERT(16, struct_test38().a[15]);
  ASSERT(17, struct_test38().a[16]);
  ASSERT(18, struct_test38().a[17]);
  ASSERT(19, struct_test38().a[18]);
  ASSERT(20, struct_test38().a[19]);

  ASSERT(1,  ({struct_type_1_1_test_4().a;}));
  ASSERT(10, ({struct_type_1_2_test_4().a;}));
  ASSERT(10, ({struct_type_1_3_test_4().a;}));
  ASSERT(10, ({struct_type_1_4_test_4().a;}));

  ASSERT(10, ({ struct_type_2_1_test_4().a; }));
  ASSERT(20, ({ struct_type_2_1_test_4().b; }));
  ASSERT(30, ({ struct_type_2_1_test_4().c; }));
  ASSERT(40, ({ struct_type_2_1_test_4().d; }));
  ASSERT(50, ({ struct_type_2_1_test_4().e; }));
  ASSERT(60, ({ struct_type_2_1_test_4().f; }));
  ASSERT(70, ({ struct_type_2_1_test_4().g; }));
  ASSERT(80, ({ struct_type_2_1_test_4().h; }));

  ASSERT(10, ({ struct_type_2_2_test_4().a; }));
  ASSERT(20, ({ struct_type_2_2_test_4().b; }));

  ASSERT(10, ({ struct_type_2_3_test_4().a; }));
  ASSERT(20, ({ struct_type_2_3_test_4().b; }));

  ASSERT(10, ({ struct_type_3_1_test_4().a; }));
  ASSERT(20, ({ struct_type_3_1_test_4().b; }));
  ASSERT(30, ({ struct_type_3_1_test_4().c; }));
  ASSERT(40, ({ struct_type_3_1_test_4().d; }));
  ASSERT(50, ({ struct_type_3_1_test_4().e; }));

  ASSERT(10, ({ struct_type_3_2_test_4().a; }));
  ASSERT(20, ({ struct_type_3_2_test_4().b; }));
  ASSERT(30, ({ struct_type_3_2_test_4().c; }));
  ASSERT(40, ({ struct_type_3_2_test_4().d; }));

  ASSERT(10, ({ struct_type_3_3_test_4().a; }));
  ASSERT(20, ({ struct_type_3_3_test_4().b; }));
  ASSERT(30, ({ struct_type_3_3_test_4().c; }));
  ASSERT(40, ({ struct_type_3_3_test_4().d; }));

  ASSERT(10, ({ struct_type_4_1_test_4().a; }));
  ASSERT(20, ({ struct_type_4_1_test_4().b; }));
  ASSERT(30, ({ struct_type_4_1_test_4().c; }));
  ASSERT(40, ({ struct_type_4_1_test_4().d; }));
  ASSERT(50, ({ struct_type_4_1_test_4().e; }));
  ASSERT(60, ({ struct_type_4_1_test_4().f; }));
  ASSERT(70, ({ struct_type_4_1_test_4().g; }));

  ASSERT(10, ({ struct_type_4_2_test_4().a; }));
  ASSERT(20, ({ struct_type_4_2_test_4().b; }));
  ASSERT(30, ({ struct_type_4_2_test_4().c; }));
  ASSERT(40, ({ struct_type_4_2_test_4().d; }));
  ASSERT(50, ({ struct_type_4_2_test_4().e; }));

  ASSERT(10, ({ struct_type_4_3_test_4().a; }));
  ASSERT(20, ({ struct_type_4_3_test_4().b; }));
  ASSERT(30, ({ struct_type_4_3_test_4().c; }));
  ASSERT(40, ({ struct_type_4_3_test_4().d; }));
  ASSERT(50, ({ struct_type_4_3_test_4().e; }));
  ASSERT(60, ({ struct_type_4_3_test_4().f; }));

  ASSERT(10, ({ struct_type_4_4_test_4().a; }));
  ASSERT(20, ({ struct_type_4_4_test_4().b; }));
  ASSERT(30, ({ struct_type_4_4_test_4().c; }));
  ASSERT(40, ({ struct_type_4_4_test_4().d; }));
  ASSERT(50, ({ struct_type_4_4_test_4().e; }));
  ASSERT(60, ({ struct_type_4_4_test_4().f; }));
  ASSERT(70, ({ struct_type_4_4_test_4().g; }));

  ASSERT(10, ({ struct_type_4_5_test_1().a; }));
  ASSERT(20, ({ struct_type_4_5_test_1().b; }));

  ASSERT(10, ({ struct_type_5_1_test_4().a; }));
  ASSERT(20, ({ struct_type_5_1_test_4().b; }));
  ASSERT(30, ({ struct_type_5_1_test_4().c; }));

  ASSERT(10, ({ struct_type_5_2_test_4().a; }));
  ASSERT(20, ({ struct_type_5_2_test_4().b; }));
  ASSERT(30, ({ struct_type_5_2_test_4().c; }));
  ASSERT(40, ({ struct_type_5_2_test_4().d; }));
  ASSERT(50, ({ struct_type_5_2_test_4().e; }));
  ASSERT(60, ({ struct_type_5_2_test_4().f; }));
  ASSERT(70, ({ struct_type_5_2_test_4().g; }));
  ASSERT(80, ({ struct_type_5_2_test_4().h; }));

  ASSERT(10, ({ struct_type_6_1_test_4().a; }));
  ASSERT(20, ({ struct_type_6_1_test_4().b; }));
  ASSERT(30, ({ struct_type_6_1_test_4().c; }));

  ASSERT(10, ({ struct_type_7_1_test_4().a; }));
  ASSERT(10, ({ struct_type_7_2_test_4().a; }));

  ASSERT(10, ({ struct_type_8_1_test_4().a; }));
  ASSERT(20, ({ struct_type_8_1_test_4().b; }));

  ASSERT(10, ({ struct_type_8_2_test_4().a; }));
  ASSERT(20, ({ struct_type_8_2_test_4().b; }));

  ASSERT(5, (***add2)(2,3));

  printf("OK\n");
}