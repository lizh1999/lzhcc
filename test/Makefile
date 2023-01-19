CC=riscv64-unknown-linux-gnu-gcc
LZHCC=../build/lzhcc
QEMU=qemu-riscv64

TEST_SRCS=$(wildcard *.c)
TESTS=$(TEST_SRCS:.c=.exe)

%.exe: %.c
	$(LZHCC) -c -o $*.o $*.c -I.
	$(CC) -o $@ $*.o -xc common

test: $(TESTS)
	for i in $^; do echo $$i; $(QEMU) $$i || exit 1; echo; done
	sh driver.sh

clean:
	rm *.o *.exe