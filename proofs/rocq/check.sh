#!/bin/sh
# check.sh - compile every Rocq module in dependency order under a flat logical path.
cd "$(dirname "$0")" || exit 1
# dependency order (append as the port grows):
MODULES="LevinBudget NatPairing UniversalInterp LevinOptimal ProgEncoding ProgramEnum ProgramDecode StepCount CostRelation LevinDovetail LevinGrounded LevinDovetailRun LevinComplete SelfInterp SelfInterpCorrect LevinRunFor Extract"
fail=0
for m in $MODULES; do
  if rocq compile -Q . "" "$m.v" >/dev/null 2>&1; then echo "ok   $m.v"
  else echo "FAIL $m.v"; rocq compile -Q . "" "$m.v"; fail=1; fi
done
[ $fail -eq 0 ] && echo "rocq: PASS" || { echo "rocq: FAIL"; exit 1; }
