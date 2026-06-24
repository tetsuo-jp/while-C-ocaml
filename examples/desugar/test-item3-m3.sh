#!/bin/sh
# test-item3-m3.sh - item-3 M3 (single-pass GLOBAL-memo driver, COGEN_M3) test.
#
# M3 replaces M2's ancestor-stack/exception fold (COGEN_MEMO) with a GLOBAL memo
# table that can fold to ANY ancestor or sibling, so it stays CORRECT on non-loop
# dynamic-ifs and nested control (int_if/int_nest -- where M2's trial prototype was
# wrong / blew up, docs/ITEM3-M3-FINDINGS.md) while still aiming for loop-free
# (Jones-optimal) control on control-bearing subjects.  Milestones M3a/M3b/M3c are
# in docs/ITEM3-DESIGN.md.
#
# FOUNDATION step (this assertion set): the COGEN_M3 path is CORRECT vs the oracle
# on int_min/int_if/int_nest/usi*rev -- it currently DELEGATES to the proven `spec`
# (correctness-first); the #while counts are RECORDED as the baseline that M3b
# (structured folding) will improve.  Gated behind COGEN_M3 so the default + M2
# (COGEN_MEMO) paths are untouched.
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
$DW --data-if examples/desugar/usi.while > "$tmp/usi.data"
$DW --data    "$tmp/rev.while"           > "$tmp/rev.data"
for p in int_min int_if int_nest; do $DW --data-if "examples/desugar/$p.while" > "$tmp/$p.data"; done

oracle () { printf "(%s . %s)" "$2" "$3" > "$tmp/o.val"; src/core/rundata "$tmp/$1.data" "$tmp/o.val"; }

# correctness: [cogen --online,COGEN_M3 (p,s)](d) == [p]((s.d))
chk_m3 () { # prog s d
  orc=$(oracle "$1" "$2" "$3")
  printf "%s" "$2" > "$tmp/s.val"; printf "%s" "$3" > "$tmp/d.val"
  COGEN_M3=1 src/core/cogen --online "$tmp/$1.data" "$tmp/s.val" > "$tmp/t.data" 2>"$tmp/e" \
    || { echo "FAIL m3 cogen($1,$2): $(cat "$tmp/e")"; fail=1; return; }
  got=$(src/core/rundata "$tmp/t.data" "$tmp/d.val")
  [ "$got" = "$orc" ] && echo "ok   m3 $1 s=$2 d=$3 -> $orc" \
    || { echo "FAIL m3 $1 s=$2 d=$3: got=$got oracle=$orc"; fail=1; }
}

echo "-- M3 correctness ([cogen --online,COGEN_M3](p,s) d == [p]((s.d))) --"
for d in "nil" "('a . 'b)" "('x . ('y . 'z))" "('1 . ('2 . ('3 . nil)))"; do
  chk_m3 int_min  "'swap" "$d"
  chk_m3 int_if   "'swap" "$d"
  chk_m3 int_nest "'swap" "$d"
done

# usi x rev under M3 (control-bearing subject)
echo "-- M3 usi*rev (control-bearing subject) --"
COGEN_M3=1 src/core/cogen --online "$tmp/usi.data" "$tmp/rev.data" > "$tmp/usirev.comp" 2>"$tmp/e" \
  || { echo "FAIL m3 cogen(usi,rev): $(cat "$tmp/e")"; fail=1; }
chk_ur () { printf "%s" "$1" > "$tmp/d"; got=$(src/core/rundata "$tmp/usirev.comp" "$tmp/d"); \
  [ "$got" = "$2" ] && echo "ok   m3 usi*rev d=$1 -> $2" || { echo "FAIL m3 usi*rev d=$1: got=$got expect=$2"; fail=1; }; }
chk_ur "('1 . ('2 . ('3 . nil)))" "('3 . ('2 . ('1 . nil)))"
chk_ur "nil" "nil"
chk_ur "('a . nil)" "('a . nil)"
URW=$(grep -c "'while" "$tmp/usirev.comp" || true)
URE=$(grep -c "'eq"    "$tmp/usirev.comp" || true)
echo "M3  usi*rev (COGEN_M3):  #while=$URW #eq=$URE   (baseline; M3b goal: #while=1=subject loop, #eq=0)"

if [ $fail -eq 0 ]; then echo "item3-m3: PASS (foundation: COGEN_M3 path correct on int_if/int_nest/usi*rev; structured folding pending M3b)"
else echo "item3-m3: FAIL"; exit 1; fi
