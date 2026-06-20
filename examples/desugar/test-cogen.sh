#!/bin/sh
# test-cogen.sh - the general offline specializer (cogen): given a program and a
# static input it AUTO-produces the small specialized target, driven by the BTA
# annotations (Static -> reduce, Dynamic -> residualize).  Automates the offline
# pipeline that gen_int_min.while did by hand.  [cogen(p,s)](d) == [p]((s.d)).
#
# Covers static control (int_min: S-if unrolled) AND dynamic control (int_loop:
# D-while residualized; int_if: D-if residualized with branch merge), in both
# modes: `cogen p s` (emit the target) and `cogen --gen p` (emit the standalone
# generating-extension PROGRAM ⌜gen⌝, the Futamura cogen output).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW --data-if examples/desugar/int_min.while  > "$tmp/int_min.data"
$DW --data-if examples/desugar/int_loop.while > "$tmp/int_loop.data"
$DW --data-if examples/desugar/int_if.while   > "$tmp/int_if.data"
fail=0
oracle () { printf "(%s . %s)" "$2" "$3" > "$tmp/o.val"; src/core/rundata "$tmp/$1.data" "$tmp/o.val"; }

# -- option (b): cogen p s  emits the specialized target directly --
chk () { # prog op d
  printf "%s" "$2" > "$tmp/op.val"
  src/core/cogen "$tmp/$1.data" "$tmp/op.val" > "$tmp/tgt.data" 2>"$tmp/e" || { echo "FAIL cogen($1,$2): $(cat "$tmp/e")"; fail=1; return; }
  printf "%s" "$3" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val"); orc=$(oracle "$1" "$2" "$3")
  if [ "$got" = "$orc" ]; then echo "ok   [cogen($1,$2)]($3) = $got  ($(wc -c < "$tmp/tgt.data")B)"
  else echo "FAIL [cogen($1,$2)]($3) = $got != $orc"; fail=1; fi
}
echo "-- cogen auto-produces small targets (vs online mix's 9.36MB compiler) --"
echo "   static control (int_min: S-if unrolled):"
chk int_min "'swap" "('a . 'b)"
chk int_min "'swap" "('x . ('y . 'z))"
chk int_min "'id"   "('p . 'q)"
chk int_min "'foo"  "('m . 'n)"
echo "   dynamic control (int_loop: D-while residualized):"
chk int_loop "'S0"  "('a . ('b . ('c . nil)))"
chk int_loop "'S0"  "nil"
chk int_loop "'tag" "('1 . ('2 . ('3 . ('4 . nil))))"
echo "   dynamic control (int_if: D-if residualized, branch merge):"
chk int_if "'S0"  "('a . ('b . nil))"
chk int_if "'S0"  "nil"
chk int_if "'tag" "('p . ('q . ('r . nil)))"

# -- option (a): cogen --gen p  emits the generating-extension PROGRAM ⌜gen⌝ --
echo "-- cogen --gen: emit a standalone GENERATING-EXTENSION PROGRAM ⌜gen⌝ (Futamura cogen) --"
gchk () { # prog op d
  src/core/cogen --gen "$tmp/$1.data" > "$tmp/gen.data" 2>"$tmp/e" || { echo "FAIL --gen $1: $(cat "$tmp/e")"; fail=1; return; }
  printf "%s" "$2" > "$tmp/op.val"
  src/core/rundata "$tmp/gen.data" "$tmp/op.val" > "$tmp/tgt.data"   # run gen: emits the target
  printf "%s" "$3" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val"); orc=$(oracle "$1" "$2" "$3")
  if [ "$got" = "$orc" ]; then echo "ok   [[gen($1,$2)]]($3) = $got  (gen $(wc -c < "$tmp/gen.data")B, target $(wc -c < "$tmp/tgt.data")B)"
  else echo "FAIL [[gen($1,$2)]]($3) = $got != $orc"; fail=1; fi
}
gchk int_min  "'swap" "('a . 'b)"
gchk int_min  "'id"   "('p . 'q)"
gchk int_loop "'S0"   "('a . ('b . ('c . nil)))"
gchk int_loop "'tag"  "nil"
gchk int_if   "'S0"   "('a . ('b . nil))"
gchk int_if   "'tag"  "nil"
[ $fail -eq 0 ] && echo "cogen: PASS" || { echo "cogen: FAIL"; exit 1; }
