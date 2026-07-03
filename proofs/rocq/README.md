# Rocq port of the Levin chain

A Rocq 9.1 port of the Agda Levin development (`proofs/agda/`). **Statement-level
fidelity**: theorem statements match the Agda modules; proofs are idiomatic Rocq
(`induction` + `lia`/`nia`, `Arguments … : simpl never` + unfold lemmas for the
interpreter). Build/check: `sh check.sh` (compiles every module in dependency order
under a flat logical path via `rocq compile -Q . ""`). All lemmas are axiom-free
(`Print Assumptions` = *Closed under the global context*).

## Status — 13 modules, the Levin optimality STATEMENT reached

| Rocq module | ports Agda | key content |
|---|---|---|
| `LevinBudget.v` | LevinBudget | pow2/gsum, gsum_lt_2last, overshoot(')/, pow2_add, monotonicity |
| `NatPairing.v` | NatPairing | pair/pair_inj, unpair/unpair_pair, bounds |
| `UniversalInterp.v` | UniversalInterp | V/Exp/eval, mono/add_fuel/eval_mono_le |
| `ProgEncoding.v` | ProgEncoding | enc/decode + decode_enc |
| `ProgramEnum.v` | ProgramEnum | vnat/index + index_injective |
| `ProgramDecode.v` | ProgramDecode | vunnat/enum + enum_index |
| `StepCount.v` | StepCount | evalT + evalT_eval |
| `CostRelation.v` | CostRelation | Steps + complete/sound/evalT_count/evalT_mono_le |
| `LevinOptimal.v` | LevinOptimal | LevinSearch, finish, levin_optimal |
| `LevinGrounded.v` | LevinGrounded | search (overhead:=73), levin_grounded/_solver |
| `LevinDovetail.v` | LevinDovetail | find/time_opt, find_least/find_complete/find_finish |
| `LevinDovetailRun.v` | LevinDovetailRun | Jones-stage oracle over abstract run + finishing |
| `LevinComplete.v` | LevinComplete | levin_complete / levin_operational |

`levin_operational` (`LevinComplete`) is the end statement: the operational dovetail
search halts at a stage with real (phase-charged) time `gsum kop < aq*Swork`,
`aq = 4·73·2^j`.

## Remaining — the interpreter-correctness crux (multi-session)

Everything below discharges `overhead = 73` (currently a stated constant in
`LevinGrounded`) and instantiates the abstract `run` of `LevinDovetailRun`:

1. `SelfInterp` — the concrete self-interpreter term `sint : Exp` (large, mechanical). **L**
2. `SelfInterpCorrect` (`simF`), `SintCost`, `IntEfficient` (`int-efficient`, `a = 73`) —
   the correctness/cost proofs over the specific `sint`; in Agda these were the hardest
   part (refl-dispatch / derivation-tree costs, plus a documented negative finding that
   refl-dispatch does not transfer to cost). **L (the hard middle)**
3. `LevinRunFor` — `runFor` (real `sint`) instantiates `run`; `run_hits` from
   `int-efficient-time`, giving the fully concrete operational search. **M** (once 1–2 land)

The arithmetic/combinatorial/adequacy layers (all 13 above) are done; the self-interpreter
and its cost theorem are the genuine remaining effort, as in the Agda original.
