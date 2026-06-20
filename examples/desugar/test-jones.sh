#!/bin/sh
# test-jones.sh - Jones-optimality baseline + tracked metric.  Specialize the
# self-interpreter universal.while to a LOOP-FREE subject p via cogen; the residual
# `comp` must be behaviourally equal to p ([comp](d) == [p](d)), and its SIZE
# relative to ⌜p⌝ is the Jones-optimality metric (ideal ~1).
#
# Today cogen is NOT Jones-optimal: the residual still carries interpreter overhead
# (a large ratio).  This test pins the CORRECTNESS contract (passes now) and prints
# the ratio that polyvariant program-point specialization (the next work) aims to
# shrink toward ~1 -- so progress is measurable.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data    examples/desugar/swap.while      > "$tmp/swap.data"
$DW --data-if examples/desugar/universal.while > "$tmp/u.data"
src/core/cogen "$tmp/u.data" "$tmp/swap.data" > "$tmp/comp.data"   # comp = [mix]((u . ⌜swap⌝))

chk () { # d  want : [comp](d) == [swap](d)
  printf "%s" "$1" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val")
  if [ "$got" = "$2" ]; then echo "ok   [cogen(u,swap)]($1) = $got"
  else echo "FAIL [cogen(u,swap)]($1) = $got != $2"; fail=1; fi
}
echo "-- correctness: [cogen(u,swap)](d) == [swap](d) --"
chk "('a . 'b)"        "('b . 'a)"
chk "('x . ('y . 'z))" "(('y . 'z) . 'x)"
chk "nil"              "(nil . nil)"

S=$(wc -c < "$tmp/swap.data"); C=$(wc -c < "$tmp/comp.data")
echo "-- Jones-optimality metric: |comp|=${C}B  |swap.data|=${S}B  ratio=$((C / S))x --"
echo "   (ideal ~1; large ratio = interpreter overhead remains -> polyvariant PP-spec target)"
[ $fail -eq 0 ] && echo "jones: PASS (correct; optimality ratio tracked)" || { echo "jones: FAIL"; exit 1; }
