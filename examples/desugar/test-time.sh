#!/bin/sh
# test-time.sh - QUANTITATIVE optimality: specialization should not just shrink code
# but reduce the book's unit-cost running time (ch.16-19).  Two parts:
#  (1) cost-model oracle: rundata --time == while --time on the corpus (--data form,
#      if-expanded, so both use the identical EvalWhile cost model).
#  (2) speedup table: time_target(d) = time of the specialized residual on d, vs
#      time_p((s.d)) = time of interpreting the subject -- the residual must be no
#      slower (speedup >= 1), the measurable face of Jones-style optimality.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/while ] && [ -x src/core/rundata ] && [ -x src/core/cogen ] || { echo "build: make -C src/core while rundata cogen"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
progs="int_min int_loop int_if int_nest"
for p in $progs; do
  $DW          "examples/desugar/$p.while" > "$tmp/$p.core"    2>/dev/null || true
  $DW --data   "examples/desugar/$p.while" > "$tmp/$p.data"    2>/dev/null
  $DW --data-if "examples/desugar/$p.while" > "$tmp/$p.dataif"
done

echo "-- (1) cost-model oracle: rundata --time == while --time (--data, if-expanded) --"
ct () { # prog d
  printf "%s" "$2" > "$tmp/d.val"
  w=$(src/core/while   --time "$tmp/$1.core" "$tmp/d.val" 2>/dev/null)
  r=$(src/core/rundata --time "$tmp/$1.data" "$tmp/d.val" 2>/dev/null)
  if [ "$w" = "$r" ]; then echo "ok   time $1($2) = $w  (while == rundata)"
  else echo "FAIL time $1($2): while=$w rundata=$r"; fail=1; fi
}
ct int_min  "('swap . ('a . 'b))"
ct int_loop "('S0 . ('x . ('y . ('z . nil))))"
ct int_if   "('S0 . ('a . ('b . nil)))"
ct int_nest "('T . ('a . (nil . ('c . nil))))"

echo "-- (2) speedup: time_target(d)  vs  time_p((s.d))  (residual must be no slower) --"
sp () { # prog s d
  printf "%s" "$2" > "$tmp/s.val"
  src/core/cogen "$tmp/$1.dataif" "$tmp/s.val" > "$tmp/t.data"
  printf "%s" "$3" > "$tmp/d.val"
  tt=$(src/core/rundata --time "$tmp/t.data" "$tmp/d.val")            # time of residual
  printf "(%s . %s)" "$2" "$3" > "$tmp/sd.val"
  tp=$(src/core/rundata --time "$tmp/$1.dataif" "$tmp/sd.val")        # time of interpreting p
  if [ "$tt" -le "$tp" ]; then
    if [ "$tt" -gt 0 ]; then sx=$((tp / tt)); else sx="inf"; fi
    echo "ok   $1 s=$2 d=$3 : target=$tt <= interp=$tp  (~${sx}x)"
  else echo "FAIL $1 s=$2 d=$3 : target=$tt > interp=$tp (residual slower!)"; fail=1; fi
}
sp int_min  "'swap" "('a . 'b)"
sp int_min  "'id"   "('p . 'q)"
sp int_loop "'S0"   "('a . ('b . ('c . nil)))"
sp int_if   "'S0"   "('a . ('b . nil))"
sp int_nest "'T"    "('a . (nil . ('c . nil)))"
[ $fail -eq 0 ] && echo "time: PASS" || { echo "time: FAIL"; exit 1; }
