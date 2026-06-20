#!/bin/sh
# test-spec.sh - M1 for the self-applicable OFFLINE specializer spec.while.
# spec is annotation-driven: it consumes (⌜p_ann⌝ . s) -- where ⌜p_ann⌝ = annotate
# ⌜p⌝ -- and emits ⌜target⌝, dispatching CONTROL on the binding-time tags
# ('asgnS/'asgnD, 'ifS/'ifD, 'whileS/'whileD) rather than inferring them online
# (mix.while).  That is what makes it self-applicable to small compilers (Phase D).
#
# M1 (this test): [ [spec]((⌜p_ann⌝ . s)) ](d) == [p]((s.d))  -- spec produces a
# correct target.  The oracle [p]((s.d)) is computed by rundata (validated against
# `while` by test-oracle.sh).  The corpus grows as spec.while gains features:
#   static control (int_min) -> + D-while (int_loop) -> + D-if (int_if, int_nest).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/while ] && [ -x src/core/rundata ] && [ -x src/core/annotate ] || { echo "build: make -C src/core while rundata annotate"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/spec.while > "$tmp/spec.core" 2>"$tmp/e" || { echo "build spec.while (desugar) failed: $(cat "$tmp/e")"; exit 1; }
fail=0
progs="int_min int_loop int_if int_nest"

oracle () { printf "(%s . %s)" "$2" "$3" > "$tmp/o.val"; src/core/rundata "$tmp/$1.data" "$tmp/o.val"; }

diff1 () { # prog s d : [ [spec]((p_ann . s)) ](d)  ==  [p]((s.d))
  orc=$(oracle "$1" "$2" "$3")
  printf "(%s . %s)" "$(cat "$tmp/$1.ann")" "$2" > "$tmp/in.val"   # (⌜p_ann⌝ . s)
  src/core/while "$tmp/spec.core" "$tmp/in.val" > "$tmp/tgt.data" 2>"$tmp/e" || { echo "FAIL spec($1,$2): $(cat "$tmp/e")"; fail=1; return; }
  printf "%s" "$3" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val")
  if [ "$got" = "$orc" ]; then echo "ok   [[spec($1,$2)]]($3) = $got"
  else echo "FAIL [[spec($1,$2)]]($3) = $got != $orc"; fail=1; fi
}

for p in $progs; do
  $DW --data-if "examples/desugar/$p.while" > "$tmp/$p.data"
  src/core/annotate "$tmp/$p.data" > "$tmp/$p.ann"
done

echo "-- M1: [[spec((p_ann.s))]](d) == [p]((s.d)) --"
diff1 int_min "'swap" "('a . 'b)"
diff1 int_min "'swap" "('x . ('y . 'z))"
diff1 int_min "'id"   "('p . 'q)"
diff1 int_min "'foo"  "('m . 'n)"
diff1 int_min "'swap" "nil"
diff1 int_loop "'S0"  "('a . ('b . ('c . nil)))"
diff1 int_loop "'S0"  "nil"
diff1 int_loop "'tag" "('x . ('y . nil))"
diff1 int_if "'S0"  "('a . ('b . nil))"
diff1 int_if "'S0"  "nil"
diff1 int_if "'tag" "('x . nil)"
diff1 int_nest "'TAG" "('a . (nil . ('c . nil)))"
diff1 int_nest "'TAG" "nil"
diff1 int_nest "'Z"   "(nil . (nil . nil))"
[ $fail -eq 0 ] && echo "spec: PASS" || { echo "spec: FAIL"; exit 1; }
