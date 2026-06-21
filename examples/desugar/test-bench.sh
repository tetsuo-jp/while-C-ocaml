#!/bin/sh
# test-bench.sh - consolidated Jones-optimality results table (the paper's numbers).
# For each interpreter specialized by `cogen --online` to a static program, report
# the residual's size, its residual control (#'while), its residual dispatch (#'eq),
# and the book unit-cost time of running the residual vs. interpreting the subject
# (rundata --time).  The speedup = t_interp / t_comp.  This both documents the
# quantitative optimality story and GUARDS it: the static-spine interpreters calc
# (straight-line) and drain (looping) must stay Jones-optimal (no/one residual
# while, zero dispatch, strictly faster).  universal is reported as the open case
# (a residual store-walk loop survives -> ratio tracked, not asserted).
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0

$DW --data-if examples/desugar/int_min.while    > "$tmp/int.data"
$DW --data-if examples/desugar/calc.while      > "$tmp/calc.data"
$DW --data-if examples/desugar/drain.while      > "$tmp/drain.data"
$DW --data-if examples/desugar/universal.while  > "$tmp/u.data"
$DW --data    examples/desugar/swap.while        > "$tmp/swap.data"

printf '%-14s %6s %8s %7s %5s %8s %8s %8s\n' subject mode "|resid|B" "#while" "#eq" t_comp t_interp speedup
printf '%-14s %6s %8s %7s %5s %8s %8s %8s\n' ------- ---- -------- ------ ----- ------ -------- -------

# row: mode name interp.data static-input d -> measures and prints one table row.
# mode = online (cogen --online, value-driven polyvariant) | offline (cogen, BTA).
# Sets globals W (#while) E (#eq) TC (t_comp) TI (t_interp) for the caller to assert.
row () { # mode  name  interp.data  static-input  d
  printf "%s" "$4" > "$tmp/s.val"
  if [ "$1" = online ]; then src/core/cogen --online "$3" "$tmp/s.val" > "$tmp/comp.data"
  else                       src/core/cogen          "$3" "$tmp/s.val" > "$tmp/comp.data"; fi
  printf "%s" "$5" > "$tmp/d.val"; printf "(%s . %s)" "$4" "$5" > "$tmp/sd.val"
  # correctness (residual vs interpreting the whole (s.d))
  got=$(src/core/rundata "$tmp/comp.data" "$tmp/d.val")
  orc=$(src/core/rundata "$3" "$tmp/sd.val")
  [ "$got" = "$orc" ] || { echo "FAIL $2: [comp](d)=$got != [interp]((s.d))=$orc"; fail=1; }
  B=$(wc -c < "$tmp/comp.data")
  W=$(grep -c "'while" "$tmp/comp.data" || true)
  E=$(grep -c "'eq" "$tmp/comp.data" || true)
  TC=$(src/core/rundata --time "$tmp/comp.data" "$tmp/d.val")
  TI=$(src/core/rundata --time "$3" "$tmp/sd.val")
  if [ "$TC" -gt 0 ]; then s10=$(( (TI * 10) / TC )); else s10=0; fi
  printf '%-14s %6s %8s %7s %5s %8s %8s %6s.%sx\n' "$2" "$1" "$B" "$W" "$E" "$TC" "$TI" "$((s10 / 10))" "$((s10 % 10))"
}

row offline int_min "$tmp/int.data" "'swap" "('a . 'b)"
# int_min offline-optimal: S-if/S-while on static op unrolls -> loop-free, faster
[ "$W" -eq 0 ] || { echo "FAIL int_min: residual has $W 'while (expect 0, loop-free)"; fail=1; }
[ "$E" -eq 0 ] || { echo "FAIL int_min: residual has $E 'eq (expect 0, no dispatch)"; fail=1; }
[ "$TC" -lt "$TI" ] || { echo "FAIL int_min: residual not faster ($TC !< $TI)"; fail=1; }

row online calc  "$tmp/calc.data"  "('hd . ('tl . nil))"  "(('x . ('y . 'z)) . 'w)"
# calc must be Jones-optimal: loop-free, zero dispatch, strictly faster
[ "$W" -eq 0 ] || { echo "FAIL calc: residual has $W 'while (expect 0, loop-free)"; fail=1; }
[ "$E" -eq 0 ] || { echo "FAIL calc: residual has $E 'eq (expect 0, no dispatch)"; fail=1; }
[ "$TC" -lt "$TI" ] || { echo "FAIL calc: residual not faster ($TC !< $TI)"; fail=1; }

row online drain "$tmp/drain.data" "('tl . nil)"          "('x . ('y . nil))"
# drain must be Jones-optimal for a looping subject: exactly one residual while,
# zero dispatch, strictly faster
[ "$W" -eq 1 ] || { echo "FAIL drain: residual has $W 'while (expect 1, the outer loop)"; fail=1; }
[ "$E" -eq 0 ] || { echo "FAIL drain: residual has $E 'eq (expect 0, dispatch unrolled)"; fail=1; }
[ "$TC" -lt "$TI" ] || { echo "FAIL drain: residual not faster ($TC !< $TI)"; fail=1; }

row online universal "$tmp/u.data" "$(cat "$tmp/swap.data")" "('a . 'b)"
# universal is the OPEN case: a residual store-walk loop survives.  Report only
# (correctness already checked inside row); do NOT assert optimality here.
echo "   (universal: residual store-walk loop survives -> not yet Jones-optimal; the"
echo "    open target for program-point memoization / static-spine store rewrite)"

[ $fail -eq 0 ] && echo "bench: PASS (calc & drain Jones-optimal; universal ratio tracked)" || { echo "bench: FAIL"; exit 1; }
