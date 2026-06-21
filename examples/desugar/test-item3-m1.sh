#!/bin/sh
# test-item3-m1.sh - the GOAL test for item-3 (program-point memoization), the
# completion of C5 Jones-optimality for CONTROL-BEARING subjects and the book's
# universal.  This is written TEST-FIRST: it is RED until item-3 (M1-M3, see
# docs/ITEM3-DESIGN.md) lands, and encodes the precise success metric (residual
# CONTROL, not size): the interpreter loop/dispatch must vanish, leaving only the
# SUBJECT's own control.
#
# Targets (GREEN goal; baseline 2026-06-22 in comments):
#   usi x rev        : #while==1 (rev's OWN loop), #eq==0 (interpreter dispatch
#                      unrolled INSIDE the residual loop), |resid| small.
#                      baseline RED: 87 KB, #while=1, #eq=1.
#   universal x swap : #while==0, #eq==0 (straight-line subject -> loop-free).
#                      baseline RED: 211 KB, #while=1, #eq=1.
# Correctness is asserted UNCONDITIONALLY (must hold at every milestone).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0

# a clean core list-reverse: exactly ONE while loop (the subject's own control)
cat > "$tmp/rev.while" <<'EOF'
read X;
  Y := nil;
  while X do {
    Y := cons (hd X) Y;
    X := tl X;
  }
write Y
EOF
$DW --data-if examples/desugar/usi.while       > "$tmp/usi.data"
$DW --data-if examples/desugar/universal.while > "$tmp/u.data"
$DW --data    "$tmp/rev.while"                 > "$tmp/rev.data"
$DW --data    examples/desugar/swap.while      > "$tmp/swap.data"

# --- usi x rev: correctness + Jones-optimal control (subject loop only) ---
src/core/cogen --online "$tmp/usi.data" "$tmp/rev.data" > "$tmp/usirev.comp"
chk () { # comp-data  d-val  expected
  got=$(src/core/rundata "$1" "$2"); [ "$got" = "$3" ] || { echo "FAIL correctness: got=$got expect=$3"; fail=1; }
}
printf "('1 . ('2 . ('3 . nil)))" > "$tmp/d1"; chk "$tmp/usirev.comp" "$tmp/d1" "('3 . ('2 . ('1 . nil)))"
printf "nil"                      > "$tmp/d2"; chk "$tmp/usirev.comp" "$tmp/d2" "nil"
printf "('a . nil)"               > "$tmp/d3"; chk "$tmp/usirev.comp" "$tmp/d3" "('a . nil)"
URW=$(grep -c "'while" "$tmp/usirev.comp" || true)
URE=$(grep -c "'eq"    "$tmp/usirev.comp" || true)
URB=$(wc -c < "$tmp/usirev.comp")
echo "usi*rev:        |resid|=${URB}B  #while=$URW  #eq=$URE   (goal: #while=1 #eq=0 small; baseline 87030B/1/1)"
[ "$URW" -eq 1 ] || { echo "FAIL usi*rev: #while=$URW (expect 1 = rev's own loop)"; fail=1; }
[ "$URE" -eq 0 ] || { echo "RED  usi*rev: #eq=$URE (goal 0 = interpreter dispatch unrolled inside loop)"; fail=1; }
[ "$URB" -lt 5000 ] || { echo "RED  usi*rev: |resid|=${URB}B (goal < 5000 = no interpreter overhead)"; fail=1; }

# --- universal x swap: straight-line subject -> fully loop-free ---
src/core/cogen --online "$tmp/u.data" "$tmp/swap.data" > "$tmp/uswap.comp"
printf "('a . 'b)" > "$tmp/d4"; chk "$tmp/uswap.comp" "$tmp/d4" "('b . 'a)"
USW=$(grep -c "'while" "$tmp/uswap.comp" || true)
USE=$(grep -c "'eq"    "$tmp/uswap.comp" || true)
echo "universal*swap: |resid|=$(wc -c <"$tmp/uswap.comp")B  #while=$USW  #eq=$USE   (goal: #while=0 #eq=0; baseline 210919B/1/1)"
[ "$USW" -eq 0 ] || { echo "RED  universal*swap: #while=$USW (goal 0 = loop-free)"; fail=1; }
[ "$USE" -eq 0 ] || { echo "RED  universal*swap: #eq=$USE (goal 0 = no dispatch)"; fail=1; }

if [ $fail -eq 0 ]; then echo "item3-m1: PASS (C5 complete: control-bearing + universal Jones-optimal)"
else echo "item3-m1: RED (item-3 not yet landed; correctness must still hold above)"; exit 1; fi
