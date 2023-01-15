CC=riscv64-unknown-linux-gnu-gcc
LZHCC=../build/lzhcc
QEMU=qemu-riscv64

TEST_SRCS=$(wildcard *.c)
TESTS=$(TEST_SRCS:.c=.exe)

%.exe: %.c
	$(CC) -o- -E -P -C $*.c | $(LZHCC) -c -o $*.o -
	$(CC) -o $@ $*.o -xc common

macro.exe: macro.c
	$(LZHCC) macro.c -c -o macro.o
	$(CC) -o macro.exe macro.o -xc common

test: $(TESTS)
	for i in $^; do echo $$i; $(QEMU) $$i || exit 1; echo; done
	sh driver.sh

clean:
	rm *.o *.exe