# Paper skeleton (draft outline)

> Planning outline only — the prose draft belongs in Overleaf, not this public
> repo. This file records the claims, structure, evidence, and positioning so
> the contribution is organized. Numbers/artifacts are cross-referenced to this
> repository (`while-C-ocaml`) and the sibling `rwhile-C-ocaml`.

## Working title
**The information cost of reversible self-applicable partial evaluation:
optimization, reversibility, and self-application cannot all be cheap**

(Alt: "Futamura projections for Jones's WHILE, standard and reversible: an
executable, machine-checked study of the cost of self-application.")

## One-line thesis
The classical Futamura projections hold for ANY correct specializer, but making
a specializer **reversible** imposes an information lower bound absent in
classical PE: a reversible realization of a *non-injective* (i.e. optimizing)
specializer must carry garbage ≥ the information it discards. Hence
**{Jones-optimal, clean-reversible, cheap self-application} cannot all hold**.
We give the theorem (machine-checked), two executable specializers (optimizing
and trivial) for Jones's *Computability and Complexity* WHILE, and measure the
predicted trade-off against the reversible specializer of `rwhile-C-ocaml`.

## Contributions
- **C1 (theory, novel).** Theorem A: a reversible realization of a specializer
  `f` (injective `g`, decode `d`, `d∘g=f`) must, on every pair `f` merges, keep
  them distinct yet decode equally (garbage); quantitatively `fibre-map` injects
  `f`'s fibre into `d`'s fibre, so garbage ≥ |fibre| (Bennett–Landauer count).
  Clean reversible ⟺ injective. **Machine-checked** (`proofs/agda/RevSpecGarbage.agda`,
  `--safe`). Generalizes rwhile's `proj1-needs-trivial` (the `d=id` case).
- **C2 (artifact).** Executable Futamura fp1/fp2/fp3 for Jones C&C WHILE, in
  WHILE (programs-as-data): an *optimizing* AV specializer `mix.while` (fp1/fp2
  byte-verified, fp3 terminates) and the *trivial* `mix_triv.while` (all three
  instant). Verified with a native ⌜p⌝ interpreter `rundata` (no decoder needed).
- **C3 (measurement).** The "self-application cost signature": trivial 1×/2×|spec|
  vs optimizing 12×|mix|; 46 GB allocation / 22k minor GCs for the optimizing
  fp2; residual simplification −59%. Concrete non-injectivity witness for `mix`.
- **C4 (machine-checked design).** `--safe` Agda specs pinning the
  self-applicability invariants of the optimizing specializer (MKAV binding-time,
  emit-model soundness, PAT-READ condition, dynamic-if merge), guiding the
  implementation; plus `BTAWiden.agda` (widening sound + terminating) and
  `SpinePreservingEmit.agda` (spine-preserving emit faithful + sound) and
  `MemoFold.agda` (M2 fold soundness: folded `while` ≡ source loop when the
  per-iteration body preserves the step), `AVExprSound.agda` (the
  expression-level specializer congruence — the base case of G1) and
  `SpecExprStore.agda` (`spec_exp` over a store, the form the command level uses)
  `SpecCmdStore.agda` (the command level — `'asgn` (fold + emit), `'semi`, `'if`,
  and `'while` all preserve store consistency; emit via a no-cross-reference
  `SelfRef` invariant, `'if` via guard agreement + matching-branch dispatch, `'while`
  via `MemoFold.run-inv` over the store pair) and `SpecAssembly.agda` +
  `SpecAssemblyIf.agda` + `SpecAssemblyWhile.agda` (the whole-spec congruence
  assembled for the **full language**: `[cogen(p,s)](d)=[p]((s,d))` machine-checked
  for `skip`/`asgn`/`semi`/`if`/`while` — straight-line by structural induction, `'if`
  by escaping-set merge, `'while` by tying the body congruence to the loop kernel
  `WhileCase`/`MemoFold.run-inv` through the loop-generalisation fixpoint), backing C5.
  **The M3 global-memo driver itself is machine-checked (M4):** `MemoReconstruct` +
  `MemoReconstructGen` (the structured reconstruction is semantics-preserving — the
  emitted `sH; while g {body; sH}; exit` equals the header flowchart loop, for partial
  bodies and exits, so nested loops compose), `WhistleTerminates` (the M3c whistle's
  generalization terminates: a generalization strictly increases a dynamic-slot count
  bounded by the slot count — the implementation's monotone-bounded argument, NOT a
  full Higman/Kruskal wqo, which is inapplicable here since the embedding tests equality
  over unbounded static values), and `MemoDriverSound` (memoization is transparent: a
  consistent memo yields exactly the per-config specialization, so sharing configs adds
  no semantic change — per-config soundness is the G1 stack, fold/reconstruct are
  `MemoFold`/`MemoReconstructGen`). So `[cogen(p,s)](d)=[p]((s,d))` for the full language
  AND the memoized driver's reconstruction/termination/transparency are all `--safe`.
- **C5 (Jones-optimality, new).** A *value-driven polyvariant* online specializer
  (`src/core/Cogen.ml --online`) that decides static/dynamic from each construct's
  actual annotated value, not the monovariant BTA — so a statically-known loop the
  BTA collapses to dynamic still unrolls. Two enabling techniques: (i)
  **online-termination control** — a binding-time *widening* (sound + terminating,
  machine-checked in `BTAWiden.agda`) plus bounded unrolling with a generalization
  fallback, making the specializer total in its static input
  (`test-terminate.sh`); (ii) **spine-preserving emit** — on an emitted assignment
  with a static cons spine, store the slot as that spine with dynamic leaves
  replaced by self-references into the slot (emit-model sound, C4), so loops that
  walk the spine unroll. With a **static-spine self-interpreter** `usi.while`
  (single-scrutinee `case Cd of`, so the dynamic value-stack never poisons the
  static dispatch), `[cogen --online](usi, p)` for a straight-line `p` is
  **loop-free** (no residual interpretation loop) and ~30× faster than interpreting
  through `usi` — **Jones-optimal control** for a self-interpreter
  (`test-usi.sh`, `test-bench.sh`). This directly addresses the "residuals not yet
  Jones-optimal" limitation. **Extended to a *control-bearing* subject** by a
  single-pass **global-configuration-memo driver** (`cogen --online`, `COGEN_M3`):
  it folds the subject's loop into a residual `while` and keeps the interpreter's code
  pointer static inside it, so `usi`×rev compiles to the subject's own loop with
  **zero** interpreter dispatch (`#while=1, #eq=0`), correct (`test-item3-m3.sh`). The
  driver has three machine-checked-design parts: **M3a** a correct global memo
  (a config is specialized once; a re-reached config folds to that block, fixing the
  earlier ancestor-stack prototype that was wrong on non-loop/nested control,
  `docs/ITEM3-M3-FINDINGS.md`); **M3b** structured reconstruction of a reducible loop
  to the subject's own `while`; **M3c** a homeomorphic-embedding whistle that
  generalizes a growing configuration so the config set stays finite. Its soundness and
  termination are machine-checked (M4, below).
  *The one remaining subject* is the book's literal `universal`: its `case Cd, St of`
  conjoins the static code-stack test with a test on the **dynamic** value stack, so the
  dispatch goes dynamic and the main loop residualizes wholesale — the store-index
  **binding-time wall** (`docs/FUTAMURA.md`). This is **not** a driver gap (no store
  projection recovers a finite config set while the dispatch tests dynamic `St`; the
  config diverges, measured); it needs a **binding-time improvement** of the `St`
  dispatch, orthogonal to the memo driver. The `case Cd of` self-interpreter `usi` is
  precisely that reformulation, and a first guard-only BTI is shown necessary-but-
  insufficient (`docs/ITEM3-DESIGN.md`), so `usi` (used above) is the project's
  resolution of the wall.
  *Cross-repo validation:* the reversible sibling `rwhile-C-ocaml`
  (`analysis_store_bti.md`) independently hits the same store-index binding-time
  wall (125 residual store-walk loops in its self-applied `comp2`) and lists the
  *partially-static store* as its open fix candidate (A); our `usi` +
  spine-preserving emit realize exactly that for the classical case — the two
  implementations cross-validate both the obstacle and the technique. Both
  enabling techniques are machine-checked (`BTAWiden`, `SpinePreservingEmit`);
  the proven-vs-tested map is `docs/AGDA_CORRESPONDENCE.md`.

## What is classical vs novel (be explicit)
- **Classical**: Futamura (1971); the running self-applicable mix and cogen
  (Jones–Sestoft–Søndergaard 1985; Jones–Gomard–Sestoft 1993). Specializing an
  interpreter / self-application is NOT novel. We do not claim it.
- **Novel**: the reversible information lower bound for PE (C1) and its
  machine-checked form; the head-to-head optimizing-vs-trivial-vs-reversible
  cost study (C3) grounded in two sibling implementations; the verified-design
  methodology (C4). The reversible Futamura projections are themselves recent
  (Yokoyama lab, IEICE), not classical.
- **Classical in spirit, our realization (C5)**: value-driven/online
  polyvariant specialization and Jones-optimality are classical (JGS 1993;
  Glück 2012 *recursive polyvariant specialization* for online self-applicable
  flowchart PE — our exact setting); the **partially-static store** technique
  behind spine-preserving emit is Mogensen 1988; the store-index obstacle is the
  classical Jones-optimality **tag** problem (Makholm 2001; Danvy–López 2003;
  Brown–Palsberg 2018); online-termination via homeomorphic embedding is Leuschel
  2002 / TbHEm. The contribution is the *executable, machine-checked* realization
  for Jones C&C WHILE — a loop-free self-interpreter residual with a
  **machine-checked termination guarantee** (widening) and a **machine-checked
  partially-static store** (`SpinePreservingEmit`, the soundness side-condition
  Mogensen's prose leaves implicit). We do not claim online PE, polyvariant
  specialization, or Jones-optimality as novel concepts. Full
  result→citation map: `docs/RELATED-WORK.md`.

## Structure
1. Introduction — Futamura projections; the reversible question; thesis.
2. Background — WHILE (Jones C&C), programs-as-data, universal program;
   reversible PE (rwhile/spec_av); Bennett–Landauer.
3. An optimizing self-applicable specializer for WHILE (`mix`): AV, MKAV,
   first-class-if data, dynamic-if/while residualization, emit model. fp1/fp2/fp3.
4. The trivial specializer (`mix_triv`) and the size signature (1×/2× vs 12×).
5. **Theorem A** (reversible information lower bound) — statement, proof,
   machine-checked; corollaries (clean⟺injective; trade-off). §6 below is its
   evidence.
6. Evidence: standard vs reversible (rwhile) — the cost signature, allocation,
   and **measured garbage vs the lower bound** (see PAPER §(b) below).
7. Machine-checked design specs (C4) and the verified-PE methodology.
8. Related work (full result→component map in `docs/RELATED-WORK.md`: Futamura;
   JGS 1993; Glück 2012 recursive polyvariant specialization; Mogensen 1988
   partially-static structures; Leuschel homeomorphic embedding; Makholm /
   Danvy–López / Brown–Palsberg tag-elimination & Jones-optimality;
   Sørensen–Glück–Jones supercompilation); limitations (Jones-optimality reached for the value-driven
   online specializer on straight-line subjects via the static-spine self-
   interpreter (C5), and on a *control-bearing* subject via the single-pass
   global-configuration-memo driver (M3: `usi`×rev loop-free-modulo-subject, `#eq=0`,
   correct; the driver's soundness/termination machine-checked, M4); the book's
   `case Cd,St of` universal still residualizes interpreter overhead, but this is a
   **binding-time wall** (the dispatch tests the dynamic value stack), resolved by the
   `case Cd of` reformulation `usi` (a binding-time improvement orthogonal to the
   driver), not a driver gap — `docs/ITEM3-DESIGN.md` records the measured divergence
   and a necessary-but-insufficient guard-only BTI; trade-off "triangle" partly
   informal); future work.
9. Conclusion. Artifact appendix (repos, `test-*.sh`, Agda).

## Key results table (fill with final numbers)
| projection | optimizing `mix` | trivial `mix_triv` | reversible `spec_av`(rwhile) |
|---|---|---|---|
| fp2 comp2 size | 22.6 MB | 5.5 KB (≈1×) | 812,515 nodes (≈1×) |
| fp3 cogen size | 29.6 MB (≈12×) | 8.9 KB (≈2×) | 1,630,121 nodes (≈2×) |
| fp3 build time | ~2 h | ~0.01 s | ~18 s |
| correctness | byte-verified | byte-verified | byte-verified (d2p) |

## Jones-optimality table (C5, `test-bench.sh`)

Residual size, residual control (`#while`), residual dispatch (`#eq`), and book
unit-cost time of the residual vs. interpreting the subject (speedup), via
`cogen --online` (value-driven) unless noted. Loop-free = no residual
interpretation overhead loop.

| subject | mode | \|resid\| | #while | #eq | speedup | note |
|---|---|---|---|---|---|---|
| `int_min` | offline (BTA) | 592 B | 0 | 0 | 1.6× | S-control unrolls |
| `calc` | online | 582 B | 0 | 0 | 5.5× | static-spine, loop-free |
| `drain` | online | 484 B | 1 | 0 | 4.1× | looping subj.; 1 outer loop, no dispatch |
| `usi`×swap | online | ~10 KB | **0** | **0** | ~30× | **self-interpreter, loop-free (Jones-optimal control)** |
| `usi`×rev | online+memo (`COGEN_M3`) | 69 KB | **1** | **0** | — | **control-bearing subject: residual = rev's OWN loop, interpreter dispatch unrolled inside it; via the correct global-memo driver (M3a/b/c), machine-checked (M4)** |
| `usi`×rev (M2) | online+memo (`COGEN_MEMO`) | 28 KB | **1** | **0** | — | earlier ancestor-stack prototype (smaller, but wrong on non-loop/nested; superseded by `COGEN_M3`) |
| `universal`×swap | online | ~211 KB | 1 | 1 | ~2× | book `case Cd,St of` binding-time wall: dispatch tests dynamic `St` ⇒ config diverges, bails to `spec`; resolved by the `case Cd of` reformulation `usi`, not the driver |

Headline: specializing the *static-spine self-interpreter* `usi` to a straight-line
program yields a **loop-free** residual ~30× faster than interpretation — i.e. the
first Futamura projection produces a genuinely *compiled* program with no residual
interpreter overhead, the value-driven route to Jones-optimality.  This **extends to a
*control-bearing* subject** via a single-pass **global-configuration-memo driver**
(`cogen --online`, `COGEN_M3`): it folds the subject's loop and keeps the interpreter's
code pointer static inside it, so `usi`×rev residualizes to `while X do { <compiled
body> }` — the subject's own loop with **zero** interpreter dispatch (`#eq=0`), correct.
The driver (M3a correct global memo, M3b structured loop reconstruction, M3c
homeomorphic-embedding whistle) is correct on the non-loop/nested subjects where the
earlier ancestor-stack prototype was wrong (`docs/ITEM3-M3-FINDINGS.md`), and its
reconstruction/termination/transparency are machine-checked (M4). The one subject it
does **not** crack is the book's literal `universal`: `case Cd, St of` makes the
dispatch test the dynamic value stack, so the config set diverges and it bails to `spec`
— the store-index **binding-time wall**, resolved by the `case Cd of` reformulation
`usi` (a binding-time improvement orthogonal to the driver), not by more driving.

## §(b) Lower bound (Theorem A) vs measured garbage (rwhile §6)

Theorem A gives the *floor*; `rwhile-C-ocaml`'s reversible specializer
`spec_av_rev.rwhile` gives the *measured* garbage. The paper pairs them:

- **Floor (Theorem A, `fibre-map`)**: for a non-injective spec, the reversible
  realization's garbage over each residual ≥ |fibre| = the inputs it merges
  there (≥ log|fibre| bits). It does *not* depend on the implementation; it is
  the irreducible information the clean residual cannot encode.
- **Measured (rwhile §6, fp1 on `ri_min`)**: clean residual `B` = **103 nodes**;
  naive reversible residual `comp` = **1739 nodes** ⇒ garbage ≈ 1636 nodes.
  After minimization (N-sizing + static delocalization) `comp` = **755 nodes**
  (−57%) ⇒ garbage ≈ 652 nodes. The dominant 1055-node piece was the AV-store
  `CLEAR(Vl)` (N=256) — pure *implementation overhead*, removed by sizing
  `FpN`→13.

**Interpretation (the paper's point).** The garbage splits into
*overhead* (removable: the 1055-node store-clear, killed by N-sizing) and
*fundamental* (bounded below by Theorem A: the information `spec` actually
discards — intermediate values not recoverable from the residual). Minimization
drives the measured garbage 1739→755 *toward* the floor; the residual hard part
(boolean flags, the uncompute discipline — open in rwhile) is what sits near the
bound. So Theorem A explains *why* rwhile cannot reach a clean (garbage-free)
optimizing reversible spec, and *how far* minimization can go (down to, not
below, the discarded-information floor).

Tie-in to our standard `mix`: it is the *same* computation without the
reversibility constraint, so it simply discards that information (the
non-injectivity witness `ignore_static` shows the discard concretely) — fast and
clean, but irreversible. The reversible side must pay the floor in garbage.

## §(c) Agda unification with rwhile's reversible-projection proofs

`proofs/agda/RevSpecGarbage.agda` (Theorem A) and
`rwhile-C-ocaml/proofs/agda/RWhileRevProjGen.agda` fit together:

- **Generalization.** RevSpecGarbage's `direct-needs-injective`
  (`(∀x→g x≡f x) → Inj g → Inj f`) **is** rwhile's `proj1-needs-trivial`
  (`Inj i → (∀x→srcMap x≡i x) → Inj srcMap`) — the decode-`=id` (garbage-free,
  "direct") special case. Theorem A generalizes it by allowing a decode `d`
  (garbage), with `separates`/`same-residual`/`fibre-map` for the general case.
  Machine-checked (one line) in our module, so the subsumption is verified.
- **Bracket on garbage.** Theorem A gives the **lower** bound (garbage ≥ |fibre|,
  `fibre-map` injective); rwhile's `input-preserving-inj` gives the **upper**
  bound (keeping the whole input is always a valid reversible realization, so
  garbage ≤ |X|). Together: `|fibre(o)| ≤ garbage(o) ≤ |X|` — the paper states
  the reversible spec's garbage is bracketed, with rwhile's measured 755 nodes
  (after minimization) living inside this band.
- **Dichotomy.** rwhile's `garbage-necessary` ≈ our `no-clean-if-merges`
  (non-injective ⇒ no clean realization). Same statement, two repos.

Engineering note: a single combined Agda development (one `.agda-lib` importing
both) would make the subsumption literal rather than by-correspondence; deferred
because the two repos use separate libraries. The by-correspondence statement +
the machine-checked `direct-needs-injective` already establish it.

## Venue
IPSJ PRO (プログラミング研究会) as a research paper if C1 is the spine; or a
solid implementation/comparison report otherwise. Final novelty/venue call: the
authors (Yokoyama lab) know the reversible-PE literature.
