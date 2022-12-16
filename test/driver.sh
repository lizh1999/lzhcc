#!/bin/sh
tmp=`mktemp -d /tmp/lzhcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

LZHCC=../build/lzhcc

check() {
  if [ $? -eq 0 ]; then
    echo "testing $1 ... passed"
  else
    echo "testing $1 ... failed"
    exit 1
  fi
}

# -o
rm -f $tmp/out
$LZHCC -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
$LZHCC --help 2>&1 | grep -q lzhcc
check --help

echo OK