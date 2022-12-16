CC=riscv64-unknown-linux-gnu-gcc
LZHCC=../build/lzhcc
QEMU=qemu-riscv64

TEST_SRCS=$(wildcard *.c)
TESTS=$(TEST_SRCS:.c=.exe)

%.exe: %.c
	$(CC) -o- -E -P -C $*.c | $(LZHCC) -o $*.s -
	$(CC) -o $@ $*.s -xc common

test: $(TESTS)
	for i in $^; do echo $$i; $(QEMU) $$i || exit 1; echo; done
	sh driver.sh

clean:
	rm *.s *.exe