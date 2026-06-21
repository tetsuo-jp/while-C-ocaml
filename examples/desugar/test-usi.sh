#!/bin/sh
# test-usi.sh - the STATIC-SPINE self-interpreter usi.while (item 2): a self-
# interpreter whose STEP dispatch is on the code stack Cd ALONE (single-scrutinee
# `case Cd of`), reading the value stack St directly in each arm.  universal.while's
# `case Cd, St of` conjoins the static Cd test with a dynamic St test, so under
# specialization Cd is dynamicized and the WHOLE interpreter loop residualizes
# (~265 KB even for identity).  With usi the dispatch guard is purely on Cd (= the
# static object program), so it folds: the interpreter loop UNROLLS for a straight-
# line subject, and the residual is ORDERS OF MAGNITUDE smaller than universal's.
#
# We assert: (a) usi is a correct self-interpreter on straight-line, looping and
# conditional object programs; (b) cogen --online residuals are correct; (c) for a
# straight-line subject the residual is >10x smaller than universal's (the main
# interpreter loop no longer residualizes -- the item-2 win).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
$DW --data-if examples/desugar/usi.while       > "$tmp/usi.data"
$DW --data-if examples/desugar/universal.while > "$tmp/u.data"

# object programs
printf 'read X;\n  Y := X;\nwrite Y\n' > "$tmp/id.while"
cat > "$tmp/rev.while" <<'EOF'
read L;
  R := nil;
  while L do { R := cons (hd L) R; L := tl L; }
write R
EOF
cat > "$tmp/ifp.while" <<'EOF'
read X;
  if X then { Y := hd X; } else { Y := 'empty; }
write Y
EOF
# a MULTI-VARIABLE straight-line program (4 vars, several hd/tl/cons) -- to show
# loop-free Jones-optimality is not limited to toy 1-2 variable subjects.
cat > "$tmp/sl4.while" <<'EOF'
read X;
  A := hd X;
  B := tl X;
  C := cons B A;
  Y := cons A C;
write Y
EOF

echo "-- (a) usi is a correct self-interpreter: [usi]((#p# . d)) == [p](d) --"
sint () { # progfile  d  expect
  $DW --data "$1" > "$tmp/p.data" 2>/dev/null
  printf "(%s . %s)" "$(cat "$tmp/p.data")" "$2" > "$tmp/pd.val"
  got=$(src/core/rundata "$tmp/usi.data" "$tmp/pd.val")
  if [ "$got" = "$3" ]; then echo "ok   [usi]($(basename "$1"), $2) = $got"
  else echo "FAIL [usi]($(basename "$1"), $2) = $got != $3"; fail=1; fi
}
sint "$tmp/id.while"          "'hello"                      "'hello"
sint examples/desugar/swap.while "('a . 'b)"               "('b . 'a)"
sint "$tmp/rev.while"         "('1 . ('2 . ('3 . nil)))"    "('3 . ('2 . ('1 . nil)))"
sint "$tmp/sl4.while"         "('p . 'q)"                   "('p . ('q . 'p))"
sint "$tmp/ifp.while"         "('a . 'b)"                   "'a"
sint "$tmp/ifp.while"         "nil"                         "'empty"

echo "-- (b) cogen --online residuals are correct --"
spec () { # progfile  d  expect
  $DW --data "$1" > "$tmp/p.data" 2>/dev/null
  src/core/cogen --online "$tmp/usi.data" "$tmp/p.data" > "$tmp/comp.data"
  printf "%s" "$2" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val")
  if [ "$got" = "$3" ]; then echo "ok   [usi*$(basename "$1")]($2) = $got  (|resid|=$(wc -c <"$tmp/comp.data")B)"
  else echo "FAIL [usi*$(basename "$1")]($2) = $got != $3"; fail=1; fi
}
spec "$tmp/id.while"              "'hello"            "'hello"
spec examples/desugar/swap.while "('a . 'b)"         "('b . 'a)"
spec examples/desugar/swap.while "('x . ('y . 'z))"  "(('y . 'z) . 'x)"
spec "$tmp/rev.while"            "('1 . ('2 . nil))" "('2 . ('1 . nil))"
spec "$tmp/sl4.while"            "('p . 'q)"         "('p . ('q . 'p))"

echo "-- (c) static-spine win: usi residual >10x smaller than universal's --"
$DW --data examples/desugar/swap.while > "$tmp/swap.data"
src/core/cogen --online "$tmp/usi.data" "$tmp/swap.data" > "$tmp/usi.comp"
src/core/cogen --online "$tmp/u.data"   "$tmp/swap.data" > "$tmp/u.comp"
USI=$(wc -c < "$tmp/usi.comp"); UNI=$(wc -c < "$tmp/u.comp")
echo "   |usi*swap|=${USI}B   |universal*swap|=${UNI}B   ratio=$((UNI / USI))x smaller"
if [ "$USI" -lt "$((UNI / 10))" ]; then echo "ok   usi residual is >10x smaller (main loop unrolls; Cd stays static)"
else echo "FAIL usi residual not >10x smaller (USI=$USI UNI=$UNI)"; fail=1; fi

echo "-- (d) Jones-optimal CONTROL: usi residual for a straight-line subject is LOOP-FREE --"
# the self-interpreter's loops (main dispatch, variable-store walks) all unroll, so
# NO residual 'while remains -- no interpretation overhead loop in the residual.
for s in id swap sl4; do
  case $s in id) prog="$tmp/id.while";; swap) prog=examples/desugar/swap.while;; sl4) prog="$tmp/sl4.while";; esac
  $DW --data "$prog" > "$tmp/p.data" 2>/dev/null
  src/core/cogen --online "$tmp/usi.data" "$tmp/p.data" > "$tmp/comp.data"
  w=$(grep -c "'while" "$tmp/comp.data" || true)
  if [ "$w" -eq 0 ]; then echo "ok   usi*$s residual is loop-free (#while=0)"
  else echo "FAIL usi*$s residual still has $w 'while (store-walk not unrolled)"; fail=1; fi
done

[ $fail -eq 0 ] && echo "usi: PASS (static-spine self-interpreter: correct + loop-free on straight-line)" || { echo "usi: FAIL"; exit 1; }
