#!/bin/sh
# A self-contained one-variable (I) timed universal program, by composing
# the WHILE->I translation with the timed universal program (ch.19,
# Construction cstr-eff-tim-univ-I: "translate tt to one-variable I code").
#
#   tu_I = DesugarWhile --onevar timed-universal.while
#
# tu_I is itself a ONE-VARIABLE program, yet it is still a timed universal
# program: on ((⌜p⌝.d).nil^n) it yields (⟦p⟧(d).nil) when time_p(d) <= n
# and nil otherwise. This checks both the one-variable property and that
# --onevar preserved the timed boundary.
#
# Usage: examples/desugar/test-efficient-tu.sh   (from the repo root)
set -eu

W=src/core/while
TU=/tmp/tu_I.while
src/desugar/DesugarWhile --onevar examples/desugar/timed-universal.while > "$TU"

# tt has many variables; check the translation collapsed them to exactly one.
nv=$(grep -oE '[A-Z_][A-Za-z0-9_]*' "$TU" | sort -u | wc -l)
if [ "$nv" -ne 1 ]; then
  echo "FAIL tu_I uses $nv variables (want 1)"
  exit 1
fi
echo "ok   tu_I is a one-variable program"

mkcount() { n=$1; s="nil"; i=0; while [ "$i" -lt "$n" ]; do s="(nil . $s)"; i=$((i + 1)); done; echo "$s"; }

# small program p so the (one-variable, heavily packed) interpreter is fast
printf 'read X; X := tl X; write X' > /tmp/etu_p.while
printf "%s" "('a . ('b . nil))" > /tmp/etu_d.val
res=$($W /tmp/etu_p.while /tmp/etu_d.val)              # ('b . nil)
steps=$($W --time /tmp/etu_p.while /tmp/etu_d.val)     # 3
pdata=$(src/desugar/DesugarWhile --data /tmp/etu_p.while)
PD="($pdata . ('a . ('b . nil)))"

printf '(%s . %s)' "$PD" "$(mkcount $((steps - 1)))" > /tmp/etu_in.val
lo=$($W "$TU" /tmp/etu_in.val)
printf '(%s . %s)' "$PD" "$(mkcount "$steps")" > /tmp/etu_in.val
ok=$($W "$TU" /tmp/etu_in.val)

if [ "$lo" = "nil" ] && [ "$ok" = "($res . nil)" ]; then
  echo "ok   tu_I preserves the timed boundary (time=$steps: n-1 -> nil, n -> ($res . nil))"
  echo "all efficient-tu checks passed"
else
  echo "FAIL boundary: n-1 -> $lo (want nil); n -> $ok (want ($res . nil))"
  exit 1
fi
