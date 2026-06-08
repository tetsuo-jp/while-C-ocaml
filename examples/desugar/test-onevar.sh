#!/bin/sh
# WHILE -> I (one-variable) translation (Prop. many-one-var, ch.19).
# Checks that the one-variable version computes the same function as the
# original (run on the core interpreter) AND uses exactly one variable.
#
# Usage: examples/desugar/test-onevar.sh   (from the repo root)
set -eu

W=src/core/while
fail=0
check() {  # $1 = program text, $2 = input value, $3 = label
  printf '%s' "$1" > /tmp/ov_p.while
  src/desugar/DesugarWhile          /tmp/ov_p.while > /tmp/ov_orig.while
  src/desugar/DesugarWhile --onevar /tmp/ov_p.while > /tmp/ov_one.while
  printf '%s' "$2" > /tmp/ov_d.val
  o=$($W /tmp/ov_orig.while /tmp/ov_d.val)
  v=$($W /tmp/ov_one.while  /tmp/ov_d.val)
  # core WHILE のキーワードはすべて小文字。変数は大文字/下線始まりなのでそれだけ数える
  nv=$(grep -oE '[A-Z_][A-Za-z0-9_]*' /tmp/ov_one.while | sort -u | wc -l)
  if [ "$o" = "$v" ] && [ "$nv" -eq 1 ]; then
    echo "ok   $3: $v  (1 variable)"
  else
    echo "FAIL $3: orig=$o onevar=$v vars=$nv"
    fail=1
  fi
}

check 'read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y' \
      "('1 . ('2 . ('3 . nil)))" 'reverse (2 vars)'
check 'read X; Y := cons nil X; write Y'             '(nil . nil)'           'succ'
check 'read X; X := tl X; write X'                   "('a . ('b . nil))"     'in=out variable'
check 'read X; Y := =? X X; write Y'                 "'a"                    'eq'
check 'read XY; X := hd XY; Y := tl XY; while X do { Y := cons (hd X) Y; X := tl X; } write Y' \
      "(('a . ('b . nil)) . ('c . nil))" 'append-ish (3 vars)'

[ "$fail" -eq 0 ] && echo "all one-variable checks passed" || exit 1
