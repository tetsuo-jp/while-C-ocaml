#!/bin/sh
# test-poly.sh - polyvariant (value-driven) specialization beats the monovariant BTA.
# poly.while has a loop `while T` that is STATIC in context, but a monovariant
# (one-binding-time-per-variable) BTA joins T to Dynamic because T is reused
# dynamically afterwards -- so it classifies the loop as D-while.  Consequently:
#   cogen          (offline, BTA-driven)  : RESIDUALIZES the loop (a 'while survives)
#   cogen --online (value-driven)         : UNROLLS the loop (no 'while, smaller)
# Both must stay correct: [comp](d) == [poly]((P.d)).  The online specializer decides
# Static/Dynamic from the ACTUAL annotated value, the essence of polyvariant
# specialization (different binding times per context).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data-if examples/desugar/poly.while > "$tmp/poly.data"
P="('p . ('p . ('p . nil)))"                 # static input: 3 ops
printf "%s" "$P" > "$tmp/P.val"
src/core/cogen          "$tmp/poly.data" "$tmp/P.val" > "$tmp/off.data"
src/core/cogen --online "$tmp/poly.data" "$tmp/P.val" > "$tmp/on.data"
off_w=$(grep -c "'while" "$tmp/off.data" || true); on_w=$(grep -c "'while" "$tmp/on.data" || true)
offB=$(wc -c < "$tmp/off.data"); onB=$(wc -c < "$tmp/on.data")

echo "-- correctness: offline & online both == [poly]((P.d)) --"
for d in "('a . 'b)" "nil" "('m . ('n . nil))"; do
  printf "(%s . %s)" "$P" "$d" > "$tmp/o.val"; orc=$(src/core/rundata "$tmp/poly.data" "$tmp/o.val")
  printf "%s" "$d" > "$tmp/d.val"
  o=$(src/core/rundata "$tmp/off.data" "$tmp/d.val"); n=$(src/core/rundata "$tmp/on.data" "$tmp/d.val")
  if [ "$o" = "$orc" ] && [ "$n" = "$orc" ]; then echo "ok   d=$d -> $orc"
  else echo "FAIL d=$d: oracle=$orc offline=$o online=$n"; fail=1; fi
done

echo "-- polyvariant win: monovariant residualizes the loop; value-driven unrolls it --"
if [ "$off_w" -ge 1 ]; then echo "ok   offline (monovariant) residualizes a 'while (${offB}B)"; else echo "FAIL offline expected a residual 'while"; fail=1; fi
if [ "$on_w" -eq 0 ]; then echo "ok   online (polyvariant) fully unrolled: no 'while (${onB}B)"; else echo "FAIL online still has a 'while"; fail=1; fi
if [ "$onB" -lt "$offB" ]; then echo "ok   online residual smaller: ${onB}B < ${offB}B"; else echo "FAIL online not smaller (${onB}B vs ${offB}B)"; fail=1; fi
[ $fail -eq 0 ] && echo "poly: PASS" || { echo "poly: FAIL"; exit 1; }
