#!/bin/sh
# test-annotate.sh - the BTA ANNOTATOR (src/core/annotate): given ⌜p⌝ it runs the
# binding-time analysis and emits ⌜p_ann⌝, the same program with every construct
# suffix-tagged S/D ('asgnS/'asgnD, 'ifS/'ifD, 'whileS/'whileD, 'hdS/'hdD, …,
# 'quoteS; 'semi untagged).  This is the first half of the offline specializer
# spec.while (Phase B will dispatch on these tags).
#
# Tests:
#  (1) FAITHFULNESS (erase round-trip): annotate --erase (annotate p) reproduces p
#      exactly -- comparing through the same printer via annotate --reprint, so
#      structural identity <=> byte identity.  Proves annotation only layers tags.
#  (2) TAG AGREEMENT with the BTA: the guard/loop tags match the division (int_min
#      op-static -> 'ifS; int_loop -> 'whileD; int_if -> 'ifD; int_nest both).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/annotate ] || { echo "build: make -C src/core annotate"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
progs="int_min int_loop int_if int_nest reverse-case"

echo "-- (1) faithfulness: erase(annotate(p)) == reprint(p) --"
for p in $progs; do
  $DW --data-if "examples/desugar/$p.while" > "$tmp/$p.data"
  src/core/annotate          "$tmp/$p.data" > "$tmp/$p.ann"
  src/core/annotate --erase   "$tmp/$p.ann"  > "$tmp/$p.back"
  src/core/annotate --reprint "$tmp/$p.data" > "$tmp/$p.norm"
  if diff -q "$tmp/$p.norm" "$tmp/$p.back" >/dev/null; then echo "ok   $p: erase∘annotate = id"
  else echo "FAIL $p: erase∘annotate != id"; fail=1; fi
done

echo "-- (2) tag agreement with the BTA division --"
has ()    { if grep -q "$2" "$tmp/$1.ann"; then echo "ok   $1 has $2"; else echo "FAIL $1 missing $2"; fail=1; fi; }
hasnot () { if grep -q "$2" "$tmp/$1.ann"; then echo "FAIL $1 unexpectedly has $2"; fail=1; else echo "ok   $1 lacks $2"; fi; }
has    int_min  "'ifS"       # op static -> static if (will be unrolled by spec)
hasnot int_min  "'ifD"
has    int_loop "'whileD"    # dynamic loop -> residualized
has    int_if   "'ifD"       # dynamic if -> residualized
has    int_nest "'whileD"
has    int_nest "'ifD"
[ $fail -eq 0 ] && echo "annotate: PASS" || { echo "annotate: FAIL"; exit 1; }
