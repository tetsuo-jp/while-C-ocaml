#!/bin/sh
# Phase 2: measure the interpretation overhead of the timed universal
# program tt empirically (ch.19, Lemma "tt is an efficient timed universal
# program").
#
#   ratio = time_tt((p.d).nil^n) / time_p(d)     with n = time_p(d)
#
# Efficiency (Definition efficient-timed-universal) means this ratio is
# bounded by a constant k INDEPENDENT of p. The book proves this only for
# one-variable (I) programs, because multi-variable lookup/update cost
# O(#vars) per access. This script shows both:
#   (A) one-variable family  -> ratio converges to a constant (efficient)
#   (B) k-variable family     -> ratio grows with k (the inefficiency)
#
# Usage: examples/desugar/bench-efficiency.sh   (from the repo root)
set -eu

W=src/core/while
TT=/tmp/bench-tt.while
src/desugar/DesugarWhile examples/desugar/timed-universal.while > "$TT"

mkcount() { n=$1; s="nil"; i=0; while [ "$i" -lt "$n" ]; do s="(nil . $s)"; i=$((i+1)); done; echo "$s"; }
mklist()  { n=$1; s="nil"; i=0; while [ "$i" -lt "$n" ]; do s="('a . $s)"; i=$((i+1)); done; echo "$s"; }

ratio() {  # $1 = program file, $2 = input value  -> prints "time_p time_tt ratio"
  printf '%s' "$2" > /tmp/bench_d.val
  tp=$($W --time "$1" /tmp/bench_d.val)
  pdata=$(src/desugar/DesugarWhile --data "$1")
  printf '(%s . %s)' "($pdata . $2)" "$(mkcount "$tp")" > /tmp/bench_in.val
  ttt=$($W --time "$TT" /tmp/bench_in.val)
  printf '%d %d %d' "$tp" "$ttt" "$((ttt / tp))"
}

echo "== (A) one-variable program: read X; while X do { X := tl X; } write X =="
printf "%6s %10s %12s %8s\n" "len" "time_p" "time_tt" "ratio"
printf 'read X;\n  while X do { X := tl X; }\nwrite X' > /tmp/bench_one.while
for len in 1 2 4 8 16 32; do
  set -- $(ratio /tmp/bench_one.while "$(mklist "$len")")
  printf "%6d %10d %12d %8d\n" "$len" "$1" "$2" "$3"
done

echo
echo "== (B) k-variable family (input length 16, loop touches V_k) =="
printf "%5s %10s %12s %8s\n" "vars" "time_p" "time_tt" "ratio"
for k in 1 2 3 4 6 8; do
  {
    printf 'read X;\n'
    i=2; while [ "$i" -le "$k" ]; do printf '  V%d := nil;\n' "$i"; i=$((i + 1)); done
    if [ "$k" -ge 2 ]; then
      printf '  while X do { V%d := hd X; X := tl X; }\n  write V%d' "$k" "$k"
    else
      printf '  while X do { X := tl X; }\n  write X'
    fi
  } > /tmp/bench_kv.while
  set -- $(ratio /tmp/bench_kv.while "$(mklist 16)")
  printf "%5d %10d %12d %8d\n" "$k" "$1" "$2" "$3"
done

echo
echo "One-variable ratio is constant (efficient); the k-variable ratio grows"
echo "with k -- the O(#vars) access cost that confines the book's efficiency"
echo "theorem to the one-variable I language."
