#!/bin/sh
# test-oracle.sh - validate `rundata` (the ⌜p⌝/programs-as-data interpreter, which
# is the ORACLE for the cogen/genext differential tests) against the REFERENCE
# interpreter `while` (EvalWhile).  For every program and input, the three runners
# must agree:
#     while ⌜desugar p⌝(core text) d
#  == rundata (DesugarWhile --data    p) d     (if -> while expanded)
#  == rundata (DesugarWhile --data-if p) d     (first-class if kept)
# If rundata had a bug, all cogen tests would validate against a wrong oracle; this
# script removes that blind spot and exercises both ⌜p⌝ encodings.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/while ] && [ -x src/core/rundata ] || { echo "build: make -C src/core while rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0

check_prog () { # prog.while  input1 input2 ...
  prog=$1; shift
  base=$(basename "$prog" .while)
  $DW           "$prog" > "$tmp/$base.core"   2>"$tmp/e" || { echo "FAIL desugar $base: $(cat "$tmp/e")"; fail=1; return; }
  $DW --data    "$prog" > "$tmp/$base.data"   2>"$tmp/e" || { echo "FAIL --data $base: $(cat "$tmp/e")"; fail=1; return; }
  $DW --data-if "$prog" > "$tmp/$base.dataif" 2>"$tmp/e" || { echo "FAIL --data-if $base: $(cat "$tmp/e")"; fail=1; return; }
  for d in "$@"; do
    printf "%s" "$d" > "$tmp/d.val"
    w=$(src/core/while    "$tmp/$base.core"   "$tmp/d.val" 2>"$tmp/e1") || { echo "FAIL while $base($d): $(cat "$tmp/e1")"; fail=1; continue; }
    r=$(src/core/rundata  "$tmp/$base.data"   "$tmp/d.val" 2>"$tmp/e2") || { echo "FAIL rundata --data $base($d): $(cat "$tmp/e2")"; fail=1; continue; }
    ri=$(src/core/rundata "$tmp/$base.dataif" "$tmp/d.val" 2>"$tmp/e3") || { echo "FAIL rundata --data-if $base($d): $(cat "$tmp/e3")"; fail=1; continue; }
    if [ "$w" = "$r" ] && [ "$r" = "$ri" ]; then echo "ok   $base($d) = $w"
    else echo "FAIL $base($d): while=$w  --data=$r  --data-if=$ri"; fail=1; fi
  done
}

echo "-- rundata (⌜p⌝ oracle) vs while (EvalWhile), both ⌜p⌝ encodings --"
# lists + case + cons/hd/tl
check_prog examples/desugar/reverse-case.while  "nil" "('a . nil)" "('x . ('y . nil))" "('1 . ('2 . ('3 . nil)))"
# =? + static-or-dynamic if (int_min reads In=(s.d))
check_prog examples/desugar/int_min.while  "('swap . ('a . 'b))" "('id . ('p . 'q))" "('foo . nil)" "nil"
# dynamic while loop
check_prog examples/desugar/int_loop.while "('S0 . ('a . ('b . ('c . nil))))" "('S0 . nil)" "('t . ('x . nil))" "nil"
# if-then-else
check_prog examples/desugar/int_if.while   "('S0 . ('a . ('b . nil)))" "('S0 . nil)" "('t . ('x . nil))" "nil"
[ $fail -eq 0 ] && echo "oracle: PASS" || { echo "oracle: FAIL"; exit 1; }
