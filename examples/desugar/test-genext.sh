#!/bin/sh
# test-genext.sh - the generating extension of int_min (offline-optimal compiler).
# gen(op) emits the specialized target; [target](d) = [int_min]((op.d)).  gen is
# tiny (a few hundred bytes) -- the offline payoff vs the online mix's 9.36 MB
# comp2 (same function; the binding-time annotation op:S -> S-if -> unroll made
# it small).  rundata runs the emitted targets.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/rundata ] || { echo "build: make -C src/core rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/gen_int_min.while > "$tmp/gen.core"
$DW examples/desugar/int_min.while > "$tmp/int.core"   # for size reference only
echo "gen (offline compiler) = $(wc -c < "$tmp/gen.core") bytes"
fail=0
chk () { # op  d  want
  printf "%s" "$1" > "$tmp/op.val"
  src/core/while "$tmp/gen.core" "$tmp/op.val" > "$tmp/tgt.data"
  printf "%s" "$2" > "$tmp/d.val"
  got=$(src/core/rundata "$tmp/tgt.data" "$tmp/d.val")
  if [ "$got" = "$3" ]; then echo "ok   [gen($1)]($2) = $got"
  else echo "FAIL [gen($1)]($2) = $got != $3"; fail=1; fi
}
chk "'swap" "('a . 'b)"          "('swap . ('b . 'a))"
chk "'swap" "('x . ('y . 'z))"   "('swap . (('y . 'z) . 'x))"
chk "'id"   "('p . 'q)"          "('id . ('p . 'q))"
chk "'foo"  "('m . 'n)"          "('foo . ('m . 'n))"
[ $fail -eq 0 ] && echo "genext: PASS" || { echo "genext: FAIL"; exit 1; }
