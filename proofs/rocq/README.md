# Rocq port of the Levin chain

A Rocq 9.1 port of the Agda Levin development (`proofs/agda/`). **Statement-level
fidelity**: theorem statements match the Agda modules; proofs are idiomatic Rocq
(`induction` + `lia`/`nia`, `Arguments … : simpl never` + unfold lemmas for the
interpreter). Build/check: `sh check.sh` (compiles every module in dependency order
under a flat logical path via `rocq compile -Q . ""`).

## Status

| Rocq module | ports Agda | status |
|---|---|---|
| `LevinBudget.v` | `LevinBudget.agda` | ✅ pow2/gsum, gsum_lt_2last, overshoot(')/, pow2_add, monotonicity |
| `NatPairing.v` | `NatPairing.agda` | ✅ pair/pair_inj, unpair/unpair_pair, bounds |
| `UniversalInterp.v` | `UniversalInterp.agda` | ✅ V/Exp/eval, mono/add_fuel/eval_mono_le |
| `LevinOptimal.v` | `LevinOptimal.agda` | ✅ LevinSearch, finish, levin_optimal |

## Remaining (dependency order)

1. `ProgEncoding` — `⌜_⌝` / `decode` / `decode-⌜⌝` (structural). **M**
2. `ProgramEnum` — `vnat` / `index` / `index-injective`; `ProgramDecode` — `vunnat` /
   `enum` / `enum-index`. **M**
3. `StepCount` — `evalT`; `CostRelation` — the `⇓` relation, `complete`/`sound`/
   `evalT-count`/`evalT-mono-≤`. **M–L**
4. `SelfInterp` — the concrete self-interpreter term `sint : Exp` (large, mechanical). **L**
5. `SelfInterpCorrect` (`simF`), `SintCost`, `IntEfficient` (`int-efficient`, `a = 73`) —
   the interpreter-correctness/cost crux; refl-dispatch / derivation-tree cost proofs over
   the specific `sint`. **L (the hard middle)**
6. `LevinGrounded`, `LevinDovetail`, `LevinDovetailRun`, `LevinComplete`, `LevinRunFor` —
   the Levin-specific layers (mostly `lia`/`nia`, but depend on 4–5). **M**

The arithmetic/combinatorial layers (1–3, 6) are `lia`-friendly; the self-interpreter and
its correctness (4–5) are the genuine multi-session effort, as in the Agda original.
