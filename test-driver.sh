#!/bin/sh
tmp=`mktemp -d /tmp/lzhcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
  if [ $? -eq 0 ]; then
    echo "testing $1 ... passed"
  else
    echo "testing $1 ... failed"
    exit 1
  fi
}

cd build

# -o
rm -f $tmp/out
./lzhcc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
./lzhcc --help 2>&1 | grep -q lzhcc
check --help

echo OK