# Outstanding proof obligations

A living list of lemmas still to be proved in `proofs/agda/`, grouped by theme.
Several abstract theorems (halting, recursion, Rice, semi-decidability) are
proved with the *routine language-closure steps left as explicit hypotheses* —
discharging those for the concrete one-variable language `UniversalInterp.Exp`
is the bulk of the remaining work, and most of it funnels through **G1** (the
general self-interpreter correctness theorem).

Size legend: **S** small (one session) · **M** medium · **L** large (multi-session).

---

## G1. General self-interpreter correctness — the keystone

> **`sint-correct` : `∀ p inp → Σ ℕ (λ F → run sint F (⌜p⌝ ∙ (⌜p⌝ ∙ inp)) ≡ ⟦p⟧ inp)`**
> (the ∀-quantified Thm 4.2.1; currently `SelfInterp` verifies it only on
> concrete programs by `refl`).

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
- **`MixSpec` standalone residual for recursive `p`** — the `call`-based residual
  delegates to the ambient program; the standalone (self-global) residual for a
  recursive `p` is `sint` carrying `⌜p⌝`. Restate `specE-sound` via `G4Run.run`
  using `sint`. Needs G1. **M.**

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
Remaining:

- **`int-efficient` (Thm 19.1.3)** — the I self-interpreter `sint` has linear
  overhead: `time_sint(⌜p⌝.d) ≤ a · time_p(d)` for a constant `a`. A cost
  analysis of `sint` in the fuel model (fuel = steps). This is the real
  prerequisite Levin "exploits". **L.**
- **program enumeration** — an effective enumeration `nat→Exp` (or over `⌜p⌝ : V`)
  so the search can dovetail all programs. **S–M.**
- **`opt` and `levin-optimal`** — assemble: build `opt` (dovetail + run via `sint`
  under doubling budgets + check with `r`), and prove `time_opt(x) ≤ a_q·(time_q(x)
  + time_r(x.f(x)))` from `gsum<2last` + `overshoot` + `int-efficient`. **L.**

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
- `rundata` ⌜p⌝ oracle agreement — follows from G1 once `run` (`G4Run`) is the
  oracle. **M** (given G1).

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
