#!/bin/sh
# test-drain.sh - Jones-optimality for a LOOPING subject (now FULL).
# drain.while is a static-spine interpreter that applies a static op-list Body to A
# while A is non-nil.  cogen --online specializes it so the OUTER (dynamic) loop
# residualizes to one residual `while` AND the INNER dispatch fully unrolls: the
# residual is `while A do { <compiled Body> }` with NO interpreter dispatch (no 'eq),
# and is strictly faster.  This is full Jones-optimality for a looping subject, made
# possible by the liveness-aware dynamicize: `Cd := Body` re-initialises Cd to a
# static value each iteration and Cd is neither live-in nor live-out of the loop, so
# it stays static and the dispatch over it unrolls (the old all-vars-dynamic rule
# kept Cd dynamic, leaving the dispatch in the residual).  We ASSERT correctness +
# outer-loop-residualized + zero residual dispatch + strictly faster.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data-if examples/desugar/drain.while > "$tmp/drain.data"

echo "-- looping subject: cogen --online residual is correct + has the residual loop --"
chk () { # body  d
  printf "%s" "$1" > "$tmp/body.val"
  src/core/cogen --online "$tmp/drain.data" "$tmp/body.val" > "$tmp/comp.data"
  printf "%s" "$2" > "$tmp/d.val"; printf "(%s . %s)" "$1" "$2" > "$tmp/bd.val"
  got=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val"); orc=$(src/core/rundata "$tmp/drain.data" "$tmp/bd.val")
  w=$(grep -c "'while" "$tmp/comp.data" || true)
  if [ "$got" = "$orc" ] && [ "$w" -ge 1 ]; then echo "ok   body=$1 d=$2 -> $got  (outer loop residualized: ${w} while)"
  else echo "FAIL body=$1 d=$2: comp=$got oracle=$orc while=$w"; fail=1; fi
}
chk "('tl . nil)"          "('a . ('b . ('c . nil)))"
chk "('hd . nil)"          "(('x . 'y) . 'z)"
chk "('tl . ('tl . nil))"  "('1 . ('2 . ('3 . ('4 . nil))))"

echo "-- Jones-optimality: zero residual dispatch in the loop body + strictly faster --"
printf "('tl . nil)" > "$tmp/body.val"
src/core/cogen --online "$tmp/drain.data" "$tmp/body.val" > "$tmp/comp.data"
printf "('x . ('y . nil))" > "$tmp/d.val"; printf "(('tl . nil) . ('x . ('y . nil)))" > "$tmp/bd.val"
eqn=$(grep -c "'eq" "$tmp/comp.data" || true)
tc=$(src/core/rundata --time "$tmp/comp.data" "$tmp/d.val"); ti=$(src/core/rundata --time "$tmp/drain.data" "$tmp/bd.val")
echo "   residual dispatch ops ('eq) = $eqn ; time comp=$tc vs interp=$ti"
if [ "$eqn" -ne 0 ]; then echo "FAIL: residual still dispatches ('eq=$eqn) -- not Jones-optimal"; fail=1; fi
if [ "$tc" -ge "$ti" ]; then echo "FAIL: residual not faster (comp=$tc interp=$ti)"; fail=1; fi
[ $fail -eq 0 ] && echo "drain: PASS (looping subject FULLY Jones-optimal: no dispatch, faster)" || { echo "drain: FAIL"; exit 1; }
