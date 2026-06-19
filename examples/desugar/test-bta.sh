#!/bin/sh
# test-bta.sh - binding-time analysis (offline PE's first half).  Shows the SAME
# program getting different S/D annotations per binding-time division -- the
# distinction an online specializer cannot make at self-application but that
# offline BTA pre-decides, the key to a small (Jones-optimal) comp.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/bta ] || { echo "build: make -C src/core bta"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW --data-if examples/desugar/int_min.while  > "$tmp/int.data"
$DW --data-if examples/desugar/int_loop.while > "$tmp/loop.data"
fail=0
chk () { # file div expected-substring
  got=$(src/core/bta "$1" "$2" | grep -E "if|while" | tr -d ' ')
  case "$got" in *"$3"*) echo "ok   bta($2): $got";; *) echo "FAIL bta($2): $got (want $3)"; fail=1;; esac
}
echo "-- int_min: op static (fp1) -> S-if;  op dynamic (fp2 context) -> D-if --"
chk "$tmp/int.data" SD "S-if"
chk "$tmp/int.data" D  "D-if"
chk "$tmp/loop.data" SD "D-while"
[ $fail -eq 0 ] && echo "bta: PASS" || { echo "bta: FAIL"; exit 1; }
