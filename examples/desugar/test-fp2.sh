#!/bin/sh
# test-fp2.sh - the SECOND Futamura projection (compiler generation), SLOW (~40 min).
#
#   comp2 = [mix]((⌜mix⌝ . ('S . ⌜int_min⌝)))        -- specialize mix to int_min
#   [comp2](('S . op)) == [mix]((int_min . ('S . op)))  -- comp2 is int_min's compiler
#
# comp2 is large (~9.4 MB, a correct *interpreting* compiler), so it is run with
# the native `rundata` (a ⌜p⌝ interpreter), not universal.while.  Needs while.opt
# and rundata: `make -C src/core while.opt rundata`.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
WHILE=src/core/while.opt
RUN=src/core/rundata
[ -x "$WHILE" ] || { echo "build src/core/while.opt first"; exit 1; }
[ -x "$RUN" ]   || { echo "build src/core/rundata first (ocamlopt ... RunData.ml)"; exit 1; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/mix.while > "$tmp/mix.core"
$DW --data-if examples/desugar/mix.while     > "$tmp/mix.dataif"
$DW --data-if examples/desugar/int_min.while > "$tmp/intmin.data"
MIXD=$(cat "$tmp/mix.dataif"); INT=$(cat "$tmp/intmin.data")

echo "generating comp2 = [mix]((mix . ('S.int_min))) ... (slow)"
printf "(%s . ('S . %s))" "$MIXD" "$INT" > "$tmp/fp2in.val"
$WHILE "$tmp/mix.core" "$tmp/fp2in.val" > "$tmp/comp2.data"
echo "comp2 = $(wc -c < "$tmp/comp2.data") bytes"
COMP2=$(cat "$tmp/comp2.data")

fail=0
check () { # op
  printf "(%s . ('S . %s))" "$INT" "$1" > "$tmp/sw.val"
  $WHILE "$tmp/mix.core" "$tmp/sw.val" > "$tmp/B.data"        # fp1 residual
  printf "('S . %s)" "$1" > "$tmp/in.val"
  $RUN "$tmp/comp2.data" "$tmp/in.val" > "$tmp/cop.data"      # [comp2](('S.op))
  if cmp -s "$tmp/cop.data" "$tmp/B.data"; then echo "ok   [comp2](('S.$1)) == fp1 residual"
  else echo "FAIL [comp2](('S.$1)) != fp1 residual"; fail=1; fi
}
check "'swap"
check "'id"

# uses-op (not baked): swap-target != id-target
printf "('S . 'swap)" > "$tmp/a.val"; $RUN "$tmp/comp2.data" "$tmp/a.val" > "$tmp/cs.data"
printf "('S . 'id)"   > "$tmp/b.val"; $RUN "$tmp/comp2.data" "$tmp/b.val" > "$tmp/ci.data"
if cmp -s "$tmp/cs.data" "$tmp/ci.data"; then echo "FAIL comp2 ignores op (baked)"; fail=1
else echo "ok   comp2 uses op (swap-target != id-target)"; fi

# end-to-end
printf "('a . 'b)" > "$tmp/d.val"
echo "ok   [[comp2]('S.swap)](('a.'b)) = $($RUN "$tmp/cs.data" "$tmp/d.val")"

[ $fail -eq 0 ] && echo "fp2: PASS" || { echo "fp2: FAILURES"; exit 1; }
