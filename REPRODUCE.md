# Reproducing the results

One command (mirrors `rwhile-C-ocaml/REPRODUCE.md`):

```sh
./reproduce.sh         # fast: build, Jones-optimality table, Agda --safe (11 modules)
./reproduce.sh full    # also the full end-to-end suite (run-all.sh, ~3 min)
```

## Prerequisites

- **bnfc** (grammars are compiled at build time; generated parser/AST are not committed).
- **OCaml** toolchain: `ocamlc`/`ocamllex`/`ocamlyacc`; for unit tests `ounit2` + `ocamlfind` (opam).
- **GHC** + `happy` + `alex`, with `syb mtl array hspec` (the desugarer).
- **Agda** + `standard-library` (for `proofs/agda/`).

On the development machine the toolchain is from Linuxbrew/cabal/opam; before building:
`export PATH="$HOME/.cache/cabal/bin:$PATH" LIBRARY_PATH=/home/linuxbrew/.linuxbrew/lib` and
`eval $(opam env)` (see `CLAUDE.md` → "Build and test").

## Claims → evidence map

Contribution labels are from `docs/PAPER-SKELETON.md`; the proven-vs-tested status of
each is in `docs/AGDA_CORRESPONDENCE.md`.

| claim | command | what to look for |
|---|---|---|
| **C5** Jones-optimality (value-driven online PE) | `sh examples/desugar/test-bench.sh` | `usi` row: `#while=0 #eq=0`, ~30× speedup (loop-free self-interpreter residual) vs `universal` ~211 KB |
| **C5** loop-free for straight-line subjects (incl. multi-var) | `sh examples/desugar/test-usi.sh` | usi*id / usi*swap / usi*sl4 residuals loop-free + correct |
| **C5** online-termination (widening + bounded unroll/generalize) | `sh examples/desugar/test-terminate.sh` | budget-independent, both unrolled and generalized residuals correct |
| **C2** classical Futamura fp1 (compile) | `sh examples/desugar/test-futamura.sh` | `[comp](d) == [int_min]((op.d))` |
| **C2** fp2/fp3 (compiler / cogen), self-application | `SLOW=1 sh examples/desugar/run-all.sh` (≈40 min, `test-fp2.sh`) | byte-verified residuals |
| **C2** generating extension (offline, hand vs auto) | `sh examples/desugar/test-genext.sh`, `test-cogen.sh` | small offline compiler; cogen matches |
| **C1** Theorem A (reversible-PE information bound) | `agda --safe proofs/agda/RevSpecGarbage.agda` | type-checks; measured side in `rwhile-C-ocaml` |
| **C4/C5** machine-checked design + enablers | `sh proofs/agda/check.sh` | 11 `--safe` modules `ok` (incl. `BTAWiden`, `SpinePreservingEmit`, `MemoFold`, `AVExprSound`, `SpecExprStore`, `SpecCmdStore`) |
| oracle cross-check (⌜p⌝ interpreter) | `sh examples/desugar/test-oracle.sh` | `rundata` agrees with `while`/EvalWhile both encodings |

The single entry point for everything is `examples/desugar/run-all.sh` (also run by CI);
`reproduce.sh full` calls it.

For the research positioning — how each result maps to existing PE work (Futamura,
JGS 1993, Glück 2012, Mogensen 1988, Leuschel, tag-elimination) and where the one
open gap stands — see `docs/RELATED-WORK.md`; the concrete plan to close it
(program-point memoization) is `docs/ITEM3-DESIGN.md`.

## Cross-repo

The reversible sibling `rwhile-C-ocaml` reproduces the reversible Futamura
projections and the measured garbage (`rwhile-C-ocaml/REPRODUCE.md`). The shared
store-index binding-time wall and its resolution are documented in
`docs/FUTAMURA.md` and `rwhile-C-ocaml/analysis_store_bti_classical_ref.md`.
