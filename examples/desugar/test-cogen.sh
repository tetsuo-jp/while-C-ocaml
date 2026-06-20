#!/bin/sh
# test-cogen.sh - the general offline specializer (cogen): given a program and a
# static input it AUTO-produces the small specialized target, driven by the BTA
# annotations (Static -> reduce, Dynamic -> residualize).  Automates the offline
# pipeline that gen_int_min.while did by hand.  [cogen(p,s)](d) == [p]((s.d)).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW --data-if examples/desugar/int_min.while  > "$tmp/int.data"
$DW --data-if examples/desugar/int_loop.while > "$tmp/loop.data"
INT=$(cat "$tmp/int.data")
fail=0
oracle () { printf "(%s . %s)" "$1" "$2" > "$tmp/o.val"; src/core/rundata "$tmp/int.data" "$tmp/o.val"; }
chk () { # op d
  printf "%s" "$1" > "$tmp/op.val"
  src/core/cogen "$tmp/int.data" "$tmp/op.val" > "$tmp/tgt.data"
  printf "%s" "$2" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val"); orc=$(oracle "$1" "$2")
  if [ "$got" = "$orc" ]; then echo "ok   [cogen(int,$1)]($2) = $got  ($(wc -c < "$tmp/tgt.data")B)"
  else echo "FAIL [cogen(int,$1)]($2) = $got != $orc"; fail=1; fi
}
echo "-- cogen auto-produces small targets (vs online mix's 9.36MB compiler) --"
chk "'swap" "('a . 'b)"
chk "'swap" "('x . ('y . 'z))"
chk "'id"   "('p . 'q)"
chk "'foo"  "('m . 'n)"
echo "-- dynamic control (int_loop's D-while) is out of scope: errors cleanly --"
printf "'tag" > "$tmp/op.val"
if src/core/cogen "$tmp/loop.data" "$tmp/op.val" >/dev/null 2>"$tmp/err"; then
  echo "FAIL int_loop did not error (D-while should be out of scope)"; fail=1
else
  case "$(cat "$tmp/err")" in *"D-while"*|*"out of scope"*) echo "ok   int_loop D-while rejected ($(head -c40 "$tmp/err"))";; *) echo "FAIL wrong error: $(cat "$tmp/err")"; fail=1;; esac
fi

echo "-- cogen --gen: emit a standalone GENERATING-EXTENSION PROGRAM ⌜gen⌝ (Futamura cogen) --"
src/core/cogen --gen "$tmp/int.data" > "$tmp/gen.data"   # [gen](op) = ⌜target⌝, itself a WHILE program
echo "     gen (offline compiler-as-a-program) = $(wc -c < "$tmp/gen.data")B"
gchk () { # op d
  printf "%s" "$1" > "$tmp/op.val"
  src/core/rundata "$tmp/gen.data" "$tmp/op.val" > "$tmp/tgt.data"   # run gen: emits the target
  printf "%s" "$2" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val"); orc=$(oracle "$1" "$2")
  if [ "$got" = "$orc" ]; then echo "ok   [[gen($1)]]($2) = $got  (target $(wc -c < "$tmp/tgt.data")B)"
  else echo "FAIL [[gen($1)]]($2) = $got != $orc"; fail=1; fi
}
gchk "'swap" "('a . 'b)"
gchk "'swap" "('x . ('y . 'z))"
gchk "'id"   "('p . 'q)"
gchk "'foo"  "('m . 'n)"
if src/core/cogen --gen "$tmp/loop.data" >/dev/null 2>"$tmp/err"; then
  echo "FAIL int_loop --gen did not error (D-while should be out of scope)"; fail=1
else
  case "$(cat "$tmp/err")" in *"D-while"*|*"out of scope"*) echo "ok   --gen int_loop D-while rejected";; *) echo "FAIL wrong --gen error: $(cat "$tmp/err")"; fail=1;; esac
fi
[ $fail -eq 0 ] && echo "cogen: PASS" || { echo "cogen: FAIL"; exit 1; }
