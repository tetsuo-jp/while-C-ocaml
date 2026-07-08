# Robustness plan — a machine-checked `WHILE ≡ I` (verified `--onevar`)

The remaining *basic* theorem about the I language (see `memory`/`docs/I-LANGUAGE.md`):
a **verified concrete compiler** realizing Jones's robustness `WHILE ≡ I` (ch. 3,
7-8; `def-compile-same-data`, `robust-computable.tex`). Today only the *abstract*
compilation preorder is machine-checked; the concrete `--onevar` translation is
implemented in Haskell and differentially tested, but **not proved correct**. This
doc decomposes the proof.

Note (2026-07-08): G1 (general self-interpreter correctness, `SelfInterpCorrect.simF`)
is **DONE** — it is *not* on this list. This plan is the next basic gap.

## What "done" means
Instantiate `Compilation._⟶_` with a concrete `WHILE ⟶ I`, i.e. a machine-checked
```
onevar-correct : ∀ p d → run_I (onevar p) d ≡ run_WHILE p d
```
and (for the equivalence) the trivial `I ⟶ WHILE`, giving `WHILE ≃ I` as one
inhabitant of the compilation preorder. Everything `--safe`, axiom-free.

## Current state (what exists, what is missing)
- **Abstract backbone — DONE.** `Compilation.agda`: `Lang D` (programs + semantics),
  `_⟶_` (`comp` + `correct`), the preorder `⟶-refl`/`⟶-trans`, and the equivalence
  `≃`. No concrete instance (`Compilation.agda` header: "the concrete compilers
  themselves — `--onevar`, GOTO↔WHILE — are [not here]").
- **The one-variable value model — DONE.** `OneVarTuring.getR`/`setR` pack a
  ℕ-indexed register file into one value, with the array axioms `get-set-eq`,
  `get-set-neq`. This is exactly the many-variable → one-variable state encoding the
  compiler needs, already proven at the value level.
- **The loop machinery — REUSABLE.** `MemoFold.run` is a fuel-bounded
  `while g do step` over an abstract state, and `MemoFold.run-inv` propagates an
  invariant `Inv` through the loop; `while-cong`/`fold-sound` show two loops agreeing
  when their bodies agree on the reachable states. This is the template for the
  while-case correctness (B4).
- **The I language — DONE.** `UniversalInterp.Exp` + `eval`; target of the compiler.
- **Missing:** a multi-variable WHILE AST + semantics in Agda, a model of the
  `onevar` translation, and the correctness proof.

## Progress (2026-07-09)
- **B1 DONE** (`proofs/agda/WhileLang.agda`, `--safe`, axiom-free): the minimal
  multi-variable WHILE (`WExp`, `WCmd`, fuel-indexed `runW`) over the register store
  `V` (`getR`/`setR`), with refl-checked example runs (assign / seq+read / if / while
  drain).
- **B2 SUBSUMED**: the store *is* `V` and registers are `OneVarTuring.getR`/`setR`, so
  no separate store↔packed encoding is needed; the array axioms are directly available.
- **Foundational lemma DONE**: `WhileLang.runW-mono` (fuel monotonicity, the +1 step;
  the imperative twin of `add-fuel`). The `≤`-lift is a routine follow-on (via
  `m∸n+n≡m`) when B4 needs it.
- **B3a DONE** (`proofs/agda/WhileCompile.agda`, `--safe`, axiom-free): the LOOP-FREE
  WHILE → I compiler + correctness. `substVar` (Exp substitution) realises sequencing
  and register descent; `getRE`/`setRE'` compile `getR`/`setR`; `compileE`/`compileLF`
  map WExp/loop-free WCmd to call-free `Exp`; **`compile-correct : ev0 (compileLF c) s ≡
  runLF c s`** against the total loop-free denotational semantics `runLF`, with a
  refl-checked concrete example. (`ev0` = the call-free I evaluator, inlined to avoid
  `SelfInterpCorrect`'s slow `simF` import — a build-perf choice, not a proof gap.)
- **Bridge (i) DONE** (`WhileCompile.runLF-sound`, `--safe`): `Σ n, runW n c s ≡ just
  (runLF c s)` for loop-free `c`, using `WhileLang.runW-mono-≤` (the `≤`-lift of
  `runW-mono`, added via `m∸n+n≡m`). So loop-free WHILE → I is connected both
  denotationally (`compile-correct`) and operationally (`runLF-sound`).
- **B5 (loop-free fragment) DONE** (`WhileCompile.WHILE⟶I`, `--safe`): the first
  CONCRETE inhabitant of `Compilation._⟶_` (previously only `⟶-refl`/`⟶-trans`).
  `WHILE-LF : Lang V` (Prog = `Σ WCmd LoopFree`, run = `runLF`) `⟶` `I-CF : Lang V`
  (Prog = `Exp`, run = `ev0`), via `compileLF`/`compile-correct`. Note: `Lang.run` is a
  *total* `Prog → D → D`, so the I side necessarily uses the total call-free `ev0`.
- **Bridge (ii) DONE** (`WhileCompile.CF.eval-callfree`, `--safe`): for a call-free
  `e`, `Σ n, Interp.eval prog n e x ≡ just (ev0 e x)` (any `prog`; call-free `eval` is
  prog-independent), via `eval-≤` (from `Mono.add-fuel`) and the `just-ifnil` push-out
  for `kifnil`. So `I-CF`'s `ev0` IS the canonical fuel-indexed I operational semantics
  on call-free programs (compiled programs are call-free) — the WHILE side is
  `runLF-sound`. (Note: importing `Interp`/`Mono` makes a cold `--safe` check of
  `WhileCompile` slow, ~minutes; the `.agdai` caches it.)
- **Only B3b `while` remains** — the L crux (needs `call`/recursion; the compiled loop
  is `prog_loop = kifnil (compileE e) var (call (compileLF c))`, and correctness is an
  iteration induction relating `run n prog_loop x` to `runW m (wwhile e c) x`, with the
  guard/body evaluated via `eval-callfree`; `MemoFold.run-inv` template). A single loop
  with a loop-free body is the first target; nested loops need a dispatcher.

## Bricks (each ends at a green `--safe` commit)

| brick | content | size | assets |
|---|---|---|---|
| **B1** | multi-variable WHILE AST + fuel-indexed semantics in Agda: commands `skip`/`x:=e`/`;`/`if`/`while`, expressions over a variable-indexed `Store`; `runW : ℕ → WCmd → Store → Maybe Store` | S–M | model the loop with `MemoFold.run`'s shape; keep it minimal (like `UniversalInterp`, not the full OCaml grammar) |
| **B2** | the state encoding `⟦·⟧ : Store ↔ V` (L variables ↔ one packed value) and its laws | S | **`OneVarTuring.getR/setR` + `get-set-eq/neq` already proven** — B2 is mostly wiring |
| **B3** | the translation `onevar : WCmd → Exp` (each multi-var read/write → `getR`/`setR` on the single packed `var`; the WHILE loop → the I-language loop/recursion) | M | mirror the Haskell `transOneVar` (`src/desugar/TransOneVar.hs`) so the differential test stays meaningful |
| **B4** | **correctness** `onevar-correct : ∀ p d → run_I (onevar p) d ≡ run_WHILE p d`, by induction on the WHILE command; `:=` via the array axioms (B2); **the `while` case is the crux** — fuel alignment + an invariant "the packed value denotes the current store" through the loop | L | `MemoFold.run-inv` (invariant through a fuel loop) + the specializer's `SpecCmdStore`/`WhileCase` congruence proofs are the established templates for exactly this loop-lockstep argument |
| **B5** | instantiate `WHILE⟶I : (Lang WHILE) ⟶ (Lang I)` = `record { comp = onevar; correct = onevar-correct }`; add `I⟶WHILE` (I ⊆ WHILE, `comp = id`-ish) → `WHILE ≃ I`; wire into the preorder | S | `Compilation.⟶-refl`/`⟶-trans`, `≃` |

## The crux (B4, while-case) and its template
The assignment/skip/seq/if cases are structural. The `while g do c` case must show the
I-language loop and the WHILE loop take the *same* number of meaningful steps and keep
the packed value in sync with the store. This is precisely the shape of
`MemoFold.run-inv` (an invariant `Inv s = (packed value denotes store s)` preserved by
each `step`) composed with a per-iteration body-agreement (`while-cong`/`fold-sound`).
The reversible sibling `rwhile`'s `RWhileAVSound` and this repo's `SpecCmdStore` stage-3b
loop congruence are the two worked precedents; reuse their structure, not a fresh
fuel-induction.

## Payoff and relation to Turing-completeness (③)
With WHILE taken as Jones's reference model of the computable functions, a verified
`WHILE ≃ I` plus WHILE's universality gives **monolithic Turing-completeness of I**
("Exp ⊇ all computable functions", gap ③) almost for free — so B1–B5 subsumes most of
③. It also lets the abstract robustness class `WHILE ≡ I ≡ GOTO ≡ CM ≡ TM`
(`Compilation` header, `BOOK-ROADMAP.md` ch.7-8) gain its first *concrete* edge.

## Risks / notes
- The Agda WHILE semantics (B1) is a fresh model; it need not match the OCaml
  `EvalWhile` or Haskell `transOneVar` definitionally. Keep faithfulness honest via the
  existing differential test `examples/desugar/test-onevar.sh` (executable `--onevar`)
  as a cross-check, and state B1's semantics as the object of the theorem.
- Keep WHILE MINIMAL (one integer/tree store, the five commands) — richness beyond
  Jones's core only inflates B4 without adding to the theorem.
- Order: B1 ‖ B2 → B3 → B4 → B5. B2 is nearly free (OneVarTuring). B4 is the multi-
  session part; everything else is S–M.
- Recommended first step: **survey `SpecCmdStore`/`MemoFold`/`WhileCase` to decide how
  much of B1's loop semantics and B4's invariant machinery can be reused verbatim**
  before writing a new WHILE model.
