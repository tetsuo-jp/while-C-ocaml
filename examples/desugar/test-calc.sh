#!/bin/sh
# test-calc.sh - Jones-optimality achieved on a STATIC-SPINE-store interpreter.
# calc.while is a tiny interpreter whose store is a single variable A (no indexed
# variable list, no store-walk loop).  Specializing it with cogen --online to a
# static program therefore leaves NO residual loop: the dispatch over the static
# op-list unrolls and the residual is straight-line code on A.  This is the route-2
# fix for the universal.while pathology (where the store collapses to dynamic and
# loops survive, ratio 540x).  We assert: correctness, residual is loop-free
# (Jones-optimal), and the residual is strictly faster (book unit-cost) than
# interpreting the subject.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data-if examples/desugar/calc.while > "$tmp/calc.data"

echo "-- interpreter correctness: [calc]((prog.d)) --"
ic () { # prog d want
  printf "(%s . %s)" "$1" "$2" > "$tmp/pd.val"
  got=$(src/core/rundata "$tmp/calc.data" "$tmp/pd.val")
  if [ "$got" = "$3" ]; then echo "ok   [calc](($1 . $2)) = $got"
  else echo "FAIL [calc](($1 . $2)) = $got != $3"; fail=1; fi
}
ic "('hd . ('tl . nil))"     "(('x . ('y . 'z)) . 'w)" "('y . 'z)"
ic "('tl . nil)"             "('a . 'b)"               "'b"
ic "(('cons . 'NEW) . nil)"  "('p . 'q)"               "('NEW . ('p . 'q))"

echo "-- Jones-optimality: cogen --online residual is loop-free, correct, faster --"
jo () { # prog d
  printf "%s" "$1" > "$tmp/prog.val"
  src/core/cogen --online "$tmp/calc.data" "$tmp/prog.val" > "$tmp/comp.data"
  printf "%s" "$2" > "$tmp/d.val"; printf "(%s . %s)" "$1" "$2" > "$tmp/pd.val"
  got=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val"); orc=$(src/core/rundata "$tmp/calc.data" "$tmp/pd.val")
  w=$(grep -c "'while" "$tmp/comp.data" || true)
  tc=$(src/core/rundata --time "$tmp/comp.data" "$tmp/d.val"); ti=$(src/core/rundata --time "$tmp/calc.data" "$tmp/pd.val")
  if [ "$got" != "$orc" ]; then echo "FAIL prog=$1: [comp]($2)=$got != [calc]=$orc"; fail=1; return; fi
  if [ "$w" -ne 0 ]; then echo "FAIL prog=$1: residual still has a 'while (not Jones-optimal)"; fail=1; return; fi
  if [ "$tc" -ge "$ti" ]; then echo "FAIL prog=$1: residual not faster (comp=$tc interp=$ti)"; fail=1; return; fi
  echo "ok   prog=$1: comp loop-free, correct, time $tc < $ti (interpreter overhead removed)"
}
jo "('hd . ('tl . nil))"               "(('x . ('y . 'z)) . 'w)"
jo "('tl . ('tl . nil))"               "('a . ('b . ('c . nil)))"
jo "(('cons . 'TAG) . ('hd . nil))"    "('p . 'q)"
[ $fail -eq 0 ] && echo "calc: PASS (Jones-optimal on a static-spine interpreter)" || { echo "calc: FAIL"; exit 1; }
