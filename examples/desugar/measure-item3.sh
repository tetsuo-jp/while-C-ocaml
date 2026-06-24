#!/bin/sh
# measure-item3.sh - the item-3 (program-point memoization) progress meter.
#
# It records the BASELINE that item-3 must change: specializing the book's
# self-interpreter `universal` to a straight-line subject leaves the main
# interpreter loop + one dispatch in the residual (#while=1, #eq=1) REGARDLESS of
# subject, atop a large subject-independent floor (~200 KB).  The static-spine
# `usi` is already loop-free (#while=0, #eq=0) on the same subjects -- the contrast.
#
# The precise success metric for item-3 (docs/ITEM3-DESIGN.md) is *residual control*,
# not size.  STATUS (item-3 M3a/b/c + multi-block DONE): the global-memo driver
# (COGEN_M3) folds a CONTROL-BEARING subject to the subject's own loop, dispatch
# unrolled -- `usi`x`rev` is loop-free `#while=1, #eq=0` (the M3 section below asserts
# it; also test-item3-m3.sh).  The book's literal `universal` still bails to `spec`
# under the driver: its `case Cd,St of` conjoins the static code test with a DYNAMIC
# value-stack test (the store-index binding-time wall, docs/FUTAMURA.md) -- resolved
# by the `case Cd of` reformulation `usi`, NOT by the driver.  So the straight-line
# `--online` rows below are the NON-memo contrast (universal=1/1, usi=0/0); the M3
# section records the driver RESULT.  (Not wired into run-all.sh: measurement/
# tripwire, run on demand.)
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0

$DW --data-if examples/desugar/usi.while       > "$tmp/usi.data"
$DW --data-if examples/desugar/universal.while > "$tmp/u.data"

# three distinct straight-line subjects of increasing size (id < swap < sl4)
printf 'read X;\n  Y := X;\nwrite Y\n' > "$tmp/id.while"
cat > "$tmp/sl4.while" <<'EOF'
read X;
  A := hd X;
  B := tl X;
  C := cons B A;
  Y := cons A C;
write Y
EOF
$DW --data "$tmp/id.while"               > "$tmp/id.data"
$DW --data examples/desugar/swap.while   > "$tmp/swap.data"
$DW --data "$tmp/sl4.while"              > "$tmp/sl4.data"

printf '%-12s %-5s %9s %7s %5s\n' interp subj "|resid|B" "#while" "#eq"
printf '%-12s %-5s %9s %7s %5s\n' ------ ---- --------- ------ -----

# row INTERP.data SUBJECT NAME : specialize, print size/#while/#eq, set W/E globals
row () {
  src/core/cogen --online "$1" "$2" > "$tmp/c.data"
  W=$(grep -c "'while" "$tmp/c.data" || true)
  E=$(grep -c "'eq"    "$tmp/c.data" || true)
  printf '%-12s %-5s %9s %7s %5s\n' "$3" "$4" "$(wc -c < "$tmp/c.data")" "$W" "$E"
}

for s in id swap sl4; do
  row "$tmp/u.data" "$tmp/$s.data" universal "$s"
  # BASELINE (item-3 open): universal leaves the interpreter loop + 1 dispatch.
  [ "$W" -eq 1 ] || { echo "TRIP universal*$s: #while=$W (baseline 1; item-3 may have landed -> update)"; fail=1; }
  [ "$E" -eq 1 ] || { echo "TRIP universal*$s: #eq=$E (baseline 1; item-3 may have landed -> update)"; fail=1; }
done
for s in id swap sl4; do
  row "$tmp/usi.data" "$tmp/$s.data" usi "$s"
  # CONTRAST (already done, C5): usi is loop-free on straight-line subjects.
  [ "$W" -eq 0 ] || { echo "FAIL usi*$s: #while=$W (expect 0, loop-free -- regression!)"; fail=1; }
  [ "$E" -eq 0 ] || { echo "FAIL usi*$s: #eq=$E (expect 0, no dispatch -- regression!)"; fail=1; }
done

# --- M3 driver (COGEN_M3): the item-3 RESULT, on a CONTROL-BEARING subject ---
cat > "$tmp/rev.while" <<'EOF'
read X;
  Y := nil;
  while X do { Y := cons (hd X) Y; X := tl X; }
write Y
EOF
$DW --data "$tmp/rev.while" > "$tmp/rev.data"
rowm3 () {                       # like row, but drives the COGEN_M3 global-memo path
  COGEN_M3=1 src/core/cogen --online "$1" "$2" > "$tmp/c.data" 2>/dev/null
  W=$(grep -c "'while" "$tmp/c.data" || true)
  E=$(grep -c "'eq"    "$tmp/c.data" || true)
  printf '%-12s %-5s %9s %7s %5s\n' "$3" "$4" "$(wc -c < "$tmp/c.data")" "$W" "$E"
}
echo
printf '%-12s %-5s %9s %7s %5s   (COGEN_M3 = item-3 global-memo driver)\n' interp subj "|resid|B" "#while" "#eq"
printf '%-12s %-5s %9s %7s %5s\n' ------ ---- --------- ------ -----
rowm3 "$tmp/usi.data" "$tmp/rev.data" usi rev
# RESULT (item-3 M3a/b/c + multi-block M3b): usi*rev folds to the subject's OWN loop,
# interpreter dispatch unrolled -- Jones-optimal control in the correct global-memo
# driver (matches M2's control counts; correct on int_if/int_nest where M2 was wrong).
[ "$W" -eq 1 ] || { echo "FAIL usi*rev (M3): #while=$W (expect 1 = rev's own loop)"; fail=1; }
[ "$E" -eq 0 ] || { echo "FAIL usi*rev (M3): #eq=$E (expect 0 = dispatch unrolled)"; fail=1; }
rowm3 "$tmp/u.data" "$tmp/swap.data" universal swap
# universal still BAILS to spec under the driver (the case Cd,St of binding-time
# wall); informational, NOT asserted -- its resolution is `usi`, not the driver.

echo
echo "non-memo baseline: universal straight-line = #while 1, #eq 1 (interpreter loop +"
echo "  dispatch survive); usi = 0, 0 (loop-free, C5)."
echo "item-3 driver (COGEN_M3) RESULT: usi*rev = #while 1, #eq 0 (subject's own loop,"
echo "  dispatch unrolled).  universal = case Cd,St of binding-time wall (usi is the fix)."
if [ $fail -eq 0 ]; then echo "measure-item3: baseline + M3 result hold"
else echo "measure-item3: numbers changed -- re-baseline"; exit 1; fi
