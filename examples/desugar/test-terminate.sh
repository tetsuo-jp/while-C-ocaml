#!/bin/sh
# test-terminate.sh - online-termination control (item 1): bounded unrolling +
# generalization fallback in cogen --online.  count.while drives a loop by a STATIC
# list; specializing it normally UNROLLS the loop (loop-free residual).  When the
# unroll would exceed COGEN_UNROLL_BUDGET, the specializer GENERALIZES instead:
# it makes the loop state dynamic and emits a finite residual `while` -- so it never
# diverges, and the residual is still correct.  We assert: (a) with a generous budget
# the residual is loop-free and correct; (b) with a tiny budget over a long static
# list the residual has a `while` (generalized) and is STILL correct (same output).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data-if examples/desugar/count.while > "$tmp/count.data"

# oracle: [count]((s.d)) via the interpreter on the full input
oracle () { printf "(%s . %s)" "$1" "$2" > "$tmp/sd.val"; src/core/rundata "$tmp/count.data" "$tmp/sd.val"; }

run () { # budget  s  d -> sets GOT (residual output) WH (#'while); checks vs oracle
  printf "%s" "$2" > "$tmp/s.val"; printf "%s" "$3" > "$tmp/d.val"
  COGEN_UNROLL_BUDGET="$1" src/core/cogen --online "$tmp/count.data" "$tmp/s.val" > "$tmp/comp.data"
  GOT=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val"); ORC=$(oracle "$2" "$3")
  WH=$(grep -c "'while" "$tmp/comp.data" || true)
  [ "$GOT" = "$ORC" ] || { echo "FAIL budget=$1 s=$2: comp=$GOT != oracle=$ORC"; fail=1; }
}

echo "-- (a) generous budget: static loop UNROLLS (loop-free residual) --"
run 1000000 "('a . ('b . ('c . nil)))" "'x"
if [ "$WH" -eq 0 ]; then echo "ok   unrolled: residual loop-free, [comp]('x)=$GOT"
else echo "FAIL expected loop-free residual, got #while=$WH"; fail=1; fi

echo "-- (b) tiny budget over a long static list: GENERALIZES (residual while) + correct --"
long="('1 . ('2 . ('3 . ('4 . ('5 . ('6 . ('7 . ('8 . nil))))))))"   # length 8
run 3 "$long" "('p . 'q)"
if [ "$WH" -ge 1 ]; then echo "ok   generalized: residual has a 'while (${WH}), still correct -> $GOT"
else echo "FAIL expected generalized residual with a 'while, got #while=$WH"; fail=1; fi

echo "-- (c) generalized residual == fully-unrolled residual (same function) --"
run 1000000 "$long" "('p . 'q)"; big_unrolled=$GOT
run 1 "$long" "('p . 'q)";       big_general=$GOT
if [ "$big_unrolled" = "$big_general" ]; then echo "ok   budget-independent result: $big_general"
else echo "FAIL unrolled=$big_unrolled != generalized=$big_general"; fail=1; fi

[ $fail -eq 0 ] && echo "terminate: PASS (bounded unrolling + generalization, both correct)" || { echo "terminate: FAIL"; exit 1; }
