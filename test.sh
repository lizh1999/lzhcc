# !/bin/bash

assert() {
  expected=$1
  input=$2

  ./lzhcc "$input" > tmp.s || exit
  riscv64-unknown-linux-gnu-gcc tmp.s -static -o tmp
  qemu-riscv64 tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

cd build

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 41 ' 12 + 34 - 5 '

echo OK