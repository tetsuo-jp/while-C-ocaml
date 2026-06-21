#!/bin/sh
# test-drain.sh - Jones-optimality for a LOOPING subject (partial), and the limit.
# drain.while is a static-spine interpreter that applies a static op-list Body to A
# while A is non-nil.  cogen --online correctly specializes it: the OUTER (dynamic)
# loop residualizes to one residual `while`, and the result is correct.  But the
# INNER dispatch overhead PARTIALLY survives, because `dynamicize` (called before
# residualizing the outer D-while) marks Cd dynamic just because it is assigned in
# the loop -- even though `Cd := Body` re-initialises it to a static value each
# iteration.  We assert correctness + outer-loop-residualized (green), and TRACK the
# residual interpreter overhead (the metric a liveness-aware dynamicize must drive
# to 0).
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

echo "-- tracked limitation: interpreter dispatch overhead remaining in the loop body --"
printf "('tl . nil)" > "$tmp/body.val"
src/core/cogen --online "$tmp/drain.data" "$tmp/body.val" > "$tmp/comp.data"
printf "('x . ('y . nil))" > "$tmp/d.val"; printf "(('tl . nil) . ('x . ('y . nil)))" > "$tmp/bd.val"
eqn=$(grep -c "'eq" "$tmp/comp.data" || true)
tc=$(src/core/rundata --time "$tmp/comp.data" "$tmp/d.val"); ti=$(src/core/rundata --time "$tmp/drain.data" "$tmp/bd.val")
echo "   residual dispatch ops ('eq) = $eqn (ideal 0) ; time comp=$tc vs interp=$ti"
echo "   (full optimality needs liveness-aware dynamicize: keep re-initialised loop vars static)"
[ $fail -eq 0 ] && echo "drain: PASS (looping subject correct + residual loop; overhead tracked)" || { echo "drain: FAIL"; exit 1; }
