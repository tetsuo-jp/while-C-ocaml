#!/bin/sh
# Phase 3 demonstration: the diagonalization program diag (ch.19).
#
# diag(⌜p⌝) negates the time-bounded diagonal ⟦p⟧(⌜p⌝):
#   ⟦p⟧(⌜p⌝) yields true  within budget  -> diag(⌜p⌝) = false (nil)
#   ⟦p⟧(⌜p⌝) yields false within budget  -> diag(⌜p⌝) = true  ((nil.nil))
#   ⟦p⟧(⌜p⌝) exceeds the budget          -> diag(⌜p⌝) = true  ((nil.nil))
# This flip is the contradiction behind the linear-time hierarchy theorem:
# no a·n-time program can agree with diag on its own encoding.
#
# Usage: examples/desugar/test-diag.sh   (from the repo root)
set -eu

W=src/core/while
DIAG=/tmp/diag-test.while
src/desugar/DesugarWhile examples/desugar/diag.while > "$DIAG"

fail=0
check() {  # $1 = program text, $2 = expected diag output, $3 = label
  printf '%s' "$1" > /tmp/diag_p.while
  src/desugar/DesugarWhile --data /tmp/diag_p.while > /tmp/diag_pd.val
  got=$($W "$DIAG" /tmp/diag_pd.val)
  if [ "$got" = "$2" ]; then
    echo "ok   $3: diag(⌜p⌝) = $got"
  else
    echo "FAIL $3: diag(⌜p⌝) = $got (want $2)"
    fail=1
  fi
}

# ⟦p⟧(⌜p⌝) is non-nil (true)  -> diag = false = nil
check 'read X; Y := X; write Y'                              'nil'         'identity (true -> false)'
# ⟦p⟧(⌜p⌝) is nil (false)     -> diag = true = (nil.nil)
check 'read X; Y := nil; write Y'                            '(nil . nil)' 'const-nil (false -> true)'
# p loops forever -> tu times out -> diag = true = (nil.nil)
check 'read X; Y := cons nil nil; while Y do { Z := nil; } write Z' \
                                                            '(nil . nil)' 'infinite loop (timeout -> true)'

[ "$fail" -eq 0 ] && echo "all diag checks passed" || exit 1
