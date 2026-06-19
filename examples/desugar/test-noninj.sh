#!/bin/sh
# test-noninj.sh - a CONCRETE non-injectivity witness for the optimizing
# specializer mix.while, grounding Theorem A (proofs/agda/RevSpecGarbage.agda).
#
# ignore_static.while reads In=(s.d) and outputs only the dynamic tail d, so
# specialization DISCARDS the static input s.  Hence [mix]((ignore_static.('S.s)))
# is the SAME residual for every s: two DISTINCT inputs (s='a vs s='b) map to one
# residual == mix is non-injective.  By Theorem A a *reversible* mix could not
# recover s from the residual and would have to carry garbage; the classical
# (irreversible) mix discards s freely -- which is exactly why it is non-injective.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
WHILE=src/core/while
[ -x src/core/while.opt ] && WHILE=src/core/while.opt
RUN=src/core/rundata
[ -x "$RUN" ] || { echo "build src/core/rundata first (make -C src/core rundata)"; exit 1; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/mix.while > "$tmp/mix.core"
$DW --data-if examples/desugar/ignore_static.while > "$tmp/ign.data"
IGN=$(cat "$tmp/ign.data")
fail=0

printf "(%s . ('S . 'a))" "$IGN" > "$tmp/ia.val"
printf "(%s . ('S . 'b))" "$IGN" > "$tmp/ib.val"
$WHILE "$tmp/mix.core" "$tmp/ia.val" > "$tmp/ca.data"
$WHILE "$tmp/mix.core" "$tmp/ib.val" > "$tmp/cb.data"

if cmp -s "$tmp/ca.data" "$tmp/cb.data"; then
  echo "ok   collision: [mix]((ignore_static.('S.'a))) == [mix]((..('S.'b)))  ($(wc -c < "$tmp/ca.data") B) -- mix is non-injective"
else
  echo "FAIL no collision (mix distinguished 'a and 'b)"; fail=1
fi

# both residuals correctly compute [ignore_static]((s.d)) = d
for d in "('p . 'q)" "('x . ('y . nil))"; do
  got=$($RUN "$tmp/ca.data" "$(printf '%s' "$d" > "$tmp/d.val"; echo "$tmp/d.val")")
  if [ "$got" = "$d" ]; then echo "ok   [comp_a]($d) = $got  (= the dynamic input, s discarded)"
  else echo "FAIL [comp_a]($d) = $got != $d"; fail=1; fi
done

[ $fail -eq 0 ] && echo "non-injectivity witness: PASS" || { echo "FAILURES"; exit 1; }
