#!/bin/sh
# test-item3-m1.sh - item-3 (program-point memoization) milestone test.
#
# M2 (DONE, this test, asserted GREEN): the memoizing driver (COGEN_MEMO=1) makes
# a CONTROL-BEARING subject Jones-optimal -- usi x rev residualizes to the
# SUBJECT's own loop with the interpreter dispatch UNROLLED inside it:
#   correct, #while==1 (rev's loop), #eq==0 (no interpreter dispatch), smaller.
#   baseline (no memo): 87030 B, #while=1, #eq=1.   memo: ~28 KB, #while=1, #eq=0.
#
# M3 (PENDING, informational below, not asserted): universal x swap -> #while=0,
# and memo mode for NESTED dynamic control (int_nest) -- the driver currently
# folds a single subject loop; nested loops / the book's `case Cd,St of` need the
# next increment.  See docs/ITEM3-M1-ROOTCAUSE.md.
#
# The memo driver is GATED behind COGEN_MEMO so the default specializer (whole
# green corpus) is untouched; this test drives the memo path explicitly.
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

chk () { got=$(src/core/rundata "$1" "$2"); [ "$got" = "$3" ] || { echo "FAIL correctness: got=$got expect=$3"; fail=1; }; }

# --- M2: usi x rev via the memoizing driver -> Jones-optimal control ---
COGEN_MEMO=1 src/core/cogen --online "$tmp/usi.data" "$tmp/rev.data" > "$tmp/usirev.comp"
printf "('1 . ('2 . ('3 . nil)))" > "$tmp/d1"; chk "$tmp/usirev.comp" "$tmp/d1" "('3 . ('2 . ('1 . nil)))"
printf "nil"                      > "$tmp/d2"; chk "$tmp/usirev.comp" "$tmp/d2" "nil"
printf "('a . nil)"               > "$tmp/d3"; chk "$tmp/usirev.comp" "$tmp/d3" "('a . nil)"
printf "('p . ('q . ('r . ('s . nil))))" > "$tmp/d4"; chk "$tmp/usirev.comp" "$tmp/d4" "('s . ('r . ('q . ('p . nil))))"
URW=$(grep -c "'while" "$tmp/usirev.comp" || true)
URE=$(grep -c "'eq"    "$tmp/usirev.comp" || true)
URB=$(wc -c < "$tmp/usirev.comp")
echo "M2  usi*rev (memo):  |resid|=${URB}B  #while=$URW  #eq=$URE   (goal #while=1 #eq=0; baseline 87030B/1/1)"
[ "$URW" -eq 1 ] || { echo "FAIL usi*rev: #while=$URW (expect 1 = rev's own loop)"; fail=1; }
[ "$URE" -eq 0 ] || { echo "FAIL usi*rev: #eq=$URE (expect 0 = interpreter dispatch unrolled in loop)"; fail=1; }
[ "$URB" -lt 87030 ] || { echo "FAIL usi*rev: |resid|=${URB}B not improved over baseline 87030"; fail=1; }

# --- M3 (pending, informational): universal x swap loop-free ---
src/core/cogen --online "$tmp/u.data" "$tmp/swap.data" > "$tmp/uswap.comp"
printf "('a . 'b)" > "$tmp/d5"; chk "$tmp/uswap.comp" "$tmp/d5" "('b . 'a)"
USW=$(grep -c "'while" "$tmp/uswap.comp" || true); USE=$(grep -c "'eq" "$tmp/uswap.comp" || true)
echo "M3  universal*swap:  #while=$USW #eq=$USE   (PENDING: goal #while=0; needs nested-control memo)"

if [ $fail -eq 0 ]; then echo "item3-m1: PASS (M2 done: usi*rev Jones-optimal control; M3 universal pending)"
else echo "item3-m1: FAIL"; exit 1; fi
