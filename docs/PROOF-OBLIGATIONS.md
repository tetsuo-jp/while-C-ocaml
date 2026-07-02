# Outstanding proof obligations

A living list of lemmas still to be proved in `proofs/agda/`, grouped by theme.
Several abstract theorems (halting, recursion, Rice, semi-decidability) are
proved with the *routine language-closure steps left as explicit hypotheses* —
discharging those for the concrete one-variable language `UniversalInterp.Exp`
is the bulk of the remaining work, and most of it funnels through **G1** (the
general self-interpreter correctness theorem).

Size legend: **S** small (one session) · **M** medium · **L** large (multi-session).

---

## G1. General self-interpreter correctness — **DONE (keystone closed)**

> **`SelfInterpCorrect.simF`** : `∀ G e {p env v} → Interp.eval p G e env ≡ just v
> → Σ F, run sint F (⌜p⌝.(⌜e⌝.env)) ≡ just v` — the self-interpreter matches the
> direct interpreter for EVERY program (incl. `call`/recursion) and input,
> whenever the direct interpreter terminates.  The ∀-quantified Thm 4.2.1, proved
> by induction on the object eval fuel `G`.

**Done** via the small-problems decomposition: per-constructor dispatch lemmas
(by `refl`; `call` is a 3-level nested bind `18/8/11`, found by fuel grid search),
`mono-≤` to lift the IH, `trans`/rewrite to combine.  The remaining §2 items below
that said "needs G1" are now unblocked.

<details><summary>(historical: the original obligation)</summary>

> the ∀-quantified Thm 4.2.1; `SelfInterp` verifies it only on concrete programs
> by `refl` — now generalised by `simF`.</details>

- A fuel-relative structural induction over the interpreted expression, with a
  fuel bound `F(p, inp)`; the `call` case recurses with the same `⌜p⌝`.  **L.**
- **Call-free fragment DONE: `proofs/agda/SelfInterpCorrect.agda`** — `sim` proves
  G1 for the whole call-free fragment (var/knil/khd/ktl/kcons/kifnil) for all
  `p`, `env`.  The decomposition that worked: each recursive case = a per-
  constructor DISPATCH LEMMA (by `refl`; the dispatch keeps `J + F` fuel in the
  inner phase, constants `D,J` found by a fuel grid search, e.g. khd `12/6`, ktl
  `13/6`, kcons `12/7`, kifnil `16/8`) + `add-fuel`/`mono-≤` to lift the IH to the
  common inner fuel + `trans`/`cong` (kcons: two sub-requests; kifnil: `ifnil-just`
  + a 3-way bump).
- **Remaining: the `call` case** — re-interpret `⌜p⌝` with `env := ⟦e⟧`; needs an
  induction on the OBJECT eval fuel (the meaning is then partial / fuel-indexed,
  no longer the total `ev0`).  This closes G1 for the full language.  **M–L.**
- fuel monotonicity is available (`UniversalInterp.Mono.add-fuel`; `mono-≤` here).

**Everything in §2 below depends on G1** (it is what turns the abstract
hypotheses into concrete facts about `Exp`).

---

## 2. Discharge the abstract hypotheses for the concrete language `Exp`

Each computability theorem currently takes a "the language can build program X"
hypothesis; build X as an actual `Exp` program and discharge it.

- **`HaltingUndecidable.build-diagonal`** — from a self-halting decider `h`,
  build the flip-loop diagonal `g` in `Exp` (run `h` on `(⌜p⌝.⌜p⌝)` via `sint`,
  branch with `kifnil`, loop with `call`). Needs G1. **M.**
- **`RecursionTheorem.build`** — build `e` with `⟦e⟧(⌜v⌝.d) = ⟦p⟧(⌜spec v ⌜v⌝⌝.d)`:
  compose self-specialisation (`MixSpec`-style) with `p`. Needs G1 + a concrete
  `spec` returning encoded programs. **M.**
- **`RiceTheorem`'s self-selecting `q`** — from a decider build, via the
  recursion theorem, `q` that consults the decider on `⌜q⌝` and mimics `a`/`b`.
  Needs the concrete recursion theorem. **M.**
- **`SemiDecidable.Post`** — dovetail two semi-deciders into a decider (parallel
  execution by interleaving fuel). **M–L.**
- **`SemiDec SH`** (self-halting is semi-decidable, Thm 5.6.1) — `sint` halts on
  `(⌜p⌝.(⌜p⌝.⌜p⌝))` iff `p` halts on `⌜p⌝`. Needs G1. **S–M** (given G1).
- **`MixSpec` standalone residual for recursive `p`** — **DONE**:
  `MixSpecStandalone.standalone-spec` gives `[residual](d) = [p]((sv,d))` for the
  standalone residual `run ⌜sint⌝` carrying `⌜p⌝` (recursion served by `sint`, no
  ambient `p`), for every terminating incl. recursive `p`. It is `universal-correct`
  specialised to a partially-static input `(sv . d)`, extending `MixSpec.specE-sound`
  (which delegated recursion to the ambient program) past its "honest scope".

## 3. Concrete model instantiations

Instantiate the abstract `Model` records with `Exp`/`V`/`eval`/`sint` and prove
their fields, so the abstract theorems fire on the real language.

- `HaltingUndecidable.Model` for `Exp` — `Code := V`, `Halts p x := Σ n v, eval
  n (decode p) x ≡ just v`, `Yields` via a boolean encoding. The record is mostly
  routine; `build-diagonal` is §2. **M.**
- `RecursionTheorem.Model`, `RiceTheorem.Model`, `SemiDecidable.Model` — likewise;
  `spec` from `MixSpec`/`Cogen`, `⟦_⟧`/`Yields` from `eval`. **M each.**

## 4. Levin's theorem (Thm 20.1.2) — beyond the budget core

`LevinBudget` proves the two quantitative halves (`gsum<2last`, `overshoot`).
**Layer A DONE** — `LevinOptimal.levin-optimal` assembles them into the optimality
inequality `gsum k < a_q·(T_q+T_r)`, `a_q = 4·a·2^j`, with the interpreter overhead
`a` (`overhead`) and q's index `j` (`idx`) as explicit hypotheses (abstract-record
pattern; see `docs/LEVIN-PLAN.md`). Remaining = discharge those two for the I
language (Layer B):

- **`int-efficient` (Thm 19.1.3)** — the I self-interpreter `sint` has linear
  overhead: `time_sint(⌜p⌝.d) ≤ a · time_p(d)` for a constant `a`.
  **B1.1 DONE**: `StepCount.evalT`/`time` — a total-step-counting semantics (the
  reference `eval`'s fuel is only a DEPTH bound, so a new instrumented interpreter
  summing both branches was needed), with `evalT⇒eval` (same value) + concrete
  counts. **B1.2 Phase 1 DONE**: `CostRelation` — a big-step cost relation
  `_,_⇓_#_` (fuel-free, one rule per constructor) with both adequacy directions vs
  `evalT` (`complete`, `sound` via `evalT-mono`/`-≤`); a sub-run enters a rule as an
  opaque premise, dodging the refl-dispatch dead end. **B1.2 Phase 2 DONE**:
  `SintCost` — all 7 constructors + 2 bases as derivation trees, `sint , node ⇓ … #
  (K + Σ subcost)` (`var 8`/`knil 12`/`khd 39`/`ktl 48`/`kcons 45`/`kifnil 73`/`call
  64`), so `a = max K = 73`. **B1.2 Phase 3 DONE — `int-efficient` PROVEN**
  (`IntEfficient`): `time_sint(⌜p⌝.(⌜p⌝.d)) ≤ 73·time_p(d)` by induction on the object
  derivation (`int-efficient` relational + `int-efficient-time` executable). This is
  Jones Thm 19.1.3 and discharges Layer A's `overhead` hypothesis (`a = 73` is a
  theorem, not an assumption). **Remaining for full Levin**: B2b (surjective decoder)
  and B3 (build `opt`, `time_opt = gsum k`); see `docs/LEVIN-PLAN.md`.
- **program enumeration** — **B2a DONE**: injective Gödel numbering
  `ProgramEnum.index-injective` (via `NatPairing.pair-inj` + `⌜_⌝`/`vnat`), so each
  solver `q` has a unique index `j := index q` (programs countable). **B2b remaining**:
  the total surjective decoder `enum : ℕ → Exp` with `enum (index p) ≡ p` (well-founded
  inversion of the pairing) for the operational dovetail. **M–L** (heavier than first
  estimated; folded into B3).
- **`opt` and `levin-optimal`** — the inequality itself is **DONE** abstractly
  (`LevinOptimal.levin-optimal`, Layer A). Remaining (Layer B3): build `opt` as an
  actual `Exp` program (dovetail + run via `sint` under doubling budgets + check
  with `r`) and prove `time_opt(x) = gsum k`, tying the concrete program's running
  time to Layer A's abstract `gsum`. **L.**

## 5. C-tier robustness results (Jones ch.7–8)

- **Prop 3.7.4** — `--onevar` (WHILE→I) translation **correctness** (currently
  differential-tested). **M.**
- **Prop 8.2.1 / 8.2.2** — WHILE ≡ GOTO (Böhm–Jacopini): model GOTO, translate
  both ways, prove semantics preserved. **M–L.**
- **Thm 8.7.2 / Cor 8.7.3** — 2-counter-machine universality: encode `k` counters
  into 2 via Gödel coding (`2^a·3^b·…`), simulate, and transfer halting
  undecidability (the transfer half is `ReductionUndecidable`). **L.**
- **Thm 13.4.3** — Kleene Normal Form for WHILE. **L.**

## 6. Other differential rows worth upgrading (`docs/AGDA_CORRESPONDENCE.md`)

- `EvalWhile.ml` core semantics — an Agda model of the *full* WHILE semantics
  (the current `Exp` is the I fragment). **L.**
- `rundata` ⌜p⌝ oracle agreement — **DONE**: `UniversalCorrect.universal-correct`
  proves `run F ⌜sint⌝ (⌜p⌝.(⌜p⌝.inp)) ≡ ⟦p⟧(inp)` for EVERY terminating program
  `p` (`simF` ∘ `G4Run.run-encode`). The ∀-quantified universal-program correctness
  (`SelfInterpRun` had only concrete instances).

---

### Dependency summary

```
G1 (general self-interpreter)  ──┬──► §2 discharges ──► §3 concrete models
                                 └──► SemiDec SH, oracle agreement
int-efficient (19.1.3) ──► Levin opt (20.1.2)   [LevinBudget core already done]
(independent) ──► onevar correctness, WHILE≡GOTO, 2CM, normal form
```

G1 and `int-efficient` are the two keystones; most concrete results unlock once
they are in place.
