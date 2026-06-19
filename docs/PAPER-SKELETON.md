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
- **C4 (machine-checked design).** Four `--safe` Agda specs pinning the
  self-applicability invariants of the optimizing specializer (MKAV binding-time,
  emit-model soundness, PAT-READ condition, dynamic-if merge), guiding the
  implementation.

## What is classical vs novel (be explicit)
- **Classical**: Futamura (1971); the running self-applicable mix and cogen
  (Jones–Sestoft–Søndergaard 1985; Jones–Gomard–Sestoft 1993). Specializing an
  interpreter / self-application is NOT novel. We do not claim it.
- **Novel**: the reversible information lower bound for PE (C1) and its
  machine-checked form; the head-to-head optimizing-vs-trivial-vs-reversible
  cost study (C3) grounded in two sibling implementations; the verified-design
  methodology (C4). The reversible Futamura projections are themselves recent
  (Yokoyama lab, IEICE), not classical.

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
8. Related work; limitations (optimizing residuals not yet Jones-optimal —
   PAT-READ-ITER/store-sizing is future work; trade-off "triangle" is partly
   informal); future work.
9. Conclusion. Artifact appendix (repos, `test-*.sh`, Agda).

## Key results table (fill with final numbers)
| projection | optimizing `mix` | trivial `mix_triv` | reversible `spec_av`(rwhile) |
|---|---|---|---|
| fp2 comp2 size | 22.6 MB | 5.5 KB (≈1×) | 812,515 nodes (≈1×) |
| fp3 cogen size | 29.6 MB (≈12×) | 8.9 KB (≈2×) | 1,630,121 nodes (≈2×) |
| fp3 build time | ~2 h | ~0.01 s | ~18 s |
| correctness | byte-verified | byte-verified | byte-verified (d2p) |

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
