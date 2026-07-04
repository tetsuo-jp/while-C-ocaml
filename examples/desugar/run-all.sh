#!/bin/sh
# run-all.sh - master runner for the example / end-to-end test scripts.  Runs every
# fast test-*.sh, prints a per-script PASS/FAIL summary, and exits nonzero if any
# failed.  If `agda` is on PATH it also runs the Agda design-spec proofs; if `rocq`
# and `ocamlc` are present, test-extract runs the extracted Rocq interpreter (both
# legs skip cleanly when their tool is absent).
#
#   SLOW=1 sh run-all.sh   also runs the heavy self-application test (test-fp2.sh,
#                          ~40 min, needs `make -C src/core while.opt rundata`).
#
# Prereqs: make -C src/core while rundata cogen bta simp ; make -C src/desugar
cd "$(dirname "$0")/../.." || exit 1   # repo root (the ch.19 scripts assume this cwd)
fast="test-oracle test-futamura test-fp-triv test-noninj test-bta test-annotate test-genext test-cogen test-poly test-spec test-jones test-calc test-drain test-bench test-terminate test-usi test-time \
      test-timed-universal test-diag test-onevar test-efficient-tu test-item3-m1 test-item3-m3 test-extract"
slow="test-fp2"
if [ "${SLOW:-0}" = 1 ]; then list="$fast $slow"; else list="$fast"; fi

pass=0; fail=0; failed=""
for t in $list; do
  printf '\n========== %s ==========\n' "$t"
  if sh "examples/desugar/$t.sh"; then pass=$((pass + 1)); else fail=$((fail + 1)); failed="$failed $t"; fi
done

# Agda proofs (optional: only if agda is installed)
if command -v agda >/dev/null 2>&1; then
  printf '\n========== agda proofs ==========\n'
  if sh proofs/agda/check.sh; then pass=$((pass + 1)); else fail=$((fail + 1)); failed="$failed agda"; fi
else
  printf '\n(agda not found; skipping proofs/agda/check.sh)\n'
fi

echo "============================================================"
echo "run-all: $pass passed, $fail failed${failed:+ (failed:$failed)}"
[ $fail -eq 0 ]
