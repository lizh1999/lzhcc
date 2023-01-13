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

# -S
echo 'int main() {}' | $LZHCC -S -o- - | grep -q 'main:'
check -S

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' | > $tmp/out.c
(cd $tmp; $OLDPWD/$LZHCC out.c)
[ -f $tmp/out.o ]
check 'default output file'

(cd $tmp; $OLDPWD/$LZHCC -S out.c)
[ -f $tmp/out.s ]
check 'default output file'

echo OK