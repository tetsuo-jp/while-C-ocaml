#!/bin/sh
# measure-item3.sh - the item-3 (program-point memoization) progress meter.
#
# It records the BASELINE that item-3 must change: specializing the book's
# self-interpreter `universal` to a straight-line subject leaves the main
# interpreter loop + one dispatch in the residual (#while=1, #eq=1) REGARDLESS of
# subject, atop a large subject-independent floor (~200 KB).  The static-spine
# `usi` is already loop-free (#while=0, #eq=0) on the same subjects -- the contrast.
#
# The precise success metric for item-3 M1/M3 (docs/ITEM3-DESIGN.md) is therefore
# *residual control*, not size: universal's straight-line residual should go
# #while 1 -> 0 once configurations (pp, static-store) are memoized.  This script
# ASSERTS the current baseline, so it will TRIP (fail) exactly when item-3 lands --
# whoever closes item-3 updates the expected numbers here.  (Not wired into
# run-all.sh: it is a measurement/tripwire, run on demand.)
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

echo
echo "baseline: universal straight-line residual = #while 1, #eq 1 (interpreter loop"
echo "          + dispatch survive, subject-independent); usi = 0, 0 (loop-free)."
echo "item-3 M3 success = universal's #while: 1 -> 0 (see docs/ITEM3-DESIGN.md)."
if [ $fail -eq 0 ]; then echo "measure-item3: baseline holds (item-3 still open)"
else echo "measure-item3: numbers changed -- re-baseline if item-3 progressed"; exit 1; fi
