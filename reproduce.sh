#!/bin/sh
# reproduce.sh - one-command reproduction of this repo's key results.
# Mirrors rwhile-C-ocaml/reproduce.sh.
#
#   ./reproduce.sh         fast: build, Jones-optimality results table, Agda --safe
#   ./reproduce.sh full    also the full end-to-end suite (run-all.sh, ~3 min)
#
# Reproduces:
#   - the desugarer + core interpreter/specializer build
#   - the Jones-optimality table (offline int_min; online calc/drain/usi/universal):
#     usi (static-spine self-interpreter) specializes a straight-line program to a
#     LOOP-FREE residual ~30x faster than interpretation (contribution C5)
#   - [fast] all Agda --safe proofs (7 modules); [full] the whole test suite
#
# Prerequisites (see CLAUDE.md "Build and test" for the per-machine PATH/opam setup):
#   bnfc; ocaml (ocamlc/ocamllex/ocamlyacc, ounit2+ocamlfind via opam);
#   GHC + happy + alex with syb/mtl/array/hspec (cabal); agda + standard-library.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
mode="${1:-fast}"

echo "== build (src/desugar, src/core) =="
make -C "$HERE/src/desugar"                              >/dev/null || { echo "desugar build failed"; exit 1; }
make -C "$HERE/src/core" while rundata cogen bta simp    >/dev/null || { echo "core build failed";    exit 1; }

echo
echo "== Jones-optimality results table (examples/desugar/test-bench.sh) =="
sh "$HERE/examples/desugar/test-bench.sh"

echo
if [ "$mode" = "full" ]; then
  echo "== full end-to-end suite (examples/desugar/run-all.sh; includes Agda) =="
  sh "$HERE/examples/desugar/run-all.sh"
else
  echo "== Agda --safe proofs (proofs/agda/) =="
  if command -v agda >/dev/null 2>&1; then sh "$HERE/proofs/agda/check.sh"
  else echo "(agda not found; skipping proof check)"; fi
fi

echo
echo "== done =="
