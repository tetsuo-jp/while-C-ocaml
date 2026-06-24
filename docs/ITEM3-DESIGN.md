# item-3 design — program-point memoization (loop-free `universal` / control-bearing subjects)

Design spec for the one open Jones-optimality gap (`docs/FUTAMURA.md` "item 3";
`docs/RELATED-WORK.md` §item-3). This is a **multi-session build**, written down so
the architecture, milestones, and tests are fixed in advance (test-first).
**M1a + M2 are implemented** (gated `COGEN_MEMO` in `Cogen.ml`); **M3 (universal/
nested) is the single-pass global-memo driver** behind a separate gate `COGEN_M3`.
**M3a (correct global-memo driver) + M3b (structured fold, incl. multi-block) + M3c
(HEm-whistle generalization) are DONE** — the driver is correct on int_min/int_if/
int_nest, folds single (int_loop) and multi-block reducible loops to loop-free
`while`s, and converges control-bearing subjects. **`usi×rev` is now loop-free
under `COGEN_M3`: `#while=1, #eq=0`** (the subject's own loop, interpreter dispatch
unrolled) — Jones-optimal control matching M2's 28016B/1/0, but in the *correct*
global-memo driver (M2 is wrong on int_if/int_nest). The one remaining subject is
**`universal`**, which still does not *converge* under M3c (its store-walk over the
object code stack `Cd` doesn't fold). The cause is **not** a missing whistle: the
book's `universal` uses `case Cd, St of`, which conjoins the *static* `Cd` test with
a test on the *dynamic* value stack `St`, so the dispatch goes dynamic, `Cd`
dynamicizes, and the main loop residualizes wholesale — the **store-index binding-time
wall** (`docs/FUTAMURA.md`). `usi` (`case Cd of`) is the project's deliberate
resolution of that wall. So `universal×swap #while=0` needs a **binding-time
improvement** of the `St` dispatch ("The Trick" / arity-raising), *orthogonal* to the
global-memo driver — see "Remaining" under Milestones. The driver-side Jones-optimality
goal is **achieved for the wall-free formulation**: `usi×rev` is loop-free.

## Goal

`[cogen --online](u, p)` should be **loop-free modulo the subject** for *every* `p`:
the only residual control is `p`'s own `while`/`if`, with no surviving interpreter
loop. For straight-line `p` this holds via `usi` + spine-preserving emit (`#while=0`).
Success metric (extend `test-jones.sh`/`test-bench.sh`):

```
usi × rev        #while == (rev's own #while)   ACHIEVED (M3, COGEN_M3): #while=1, #eq=0
universal × swap #while == 0                    OPEN: case Cd,St of binding-time wall,
                                                needs binding-time improvement (not the
                                                global-memo driver) -- usi is the fix
```

**The control-bearing Jones-optimality goal is met for the wall-free interpreter
(`usi`):** the M3 global-memo driver (`COGEN_M3`) folds `usi×rev` to the subject's
own single loop, dispatch unrolled (`#while=1, #eq=0`), correct, while staying correct
on `int_if`/`int_nest` (where the M2 prototype was wrong). The book's literal
`universal` is a *separate* problem (the `St`-dispatch binding-time wall above), which
`usi` was written to resolve — so the goal's spirit (no surviving interpreter loop for
a control-bearing subject) is realized.

**Progress meter:** `examples/desugar/measure-item3.sh` records the baseline and
trips when it changes. Measured baseline (2026-06-21): `universal` × {id, swap, sl4}
all give **`#while=1, #eq=1`** (the interpreter main loop + one dispatch survive,
subject-*independent*; size is a ~200 KB floor + a small subject-proportional part —
*not* a literal constant), while `usi` × the same is **`#while=0, #eq=0`** (loop-free,
C5). So the precise success metric is residual **control** (`#while 1 → 0`), not size.

## Why the current specializer cannot do it

`Cogen.ml --online` unrolls/inlines with **no global table of specialized
configurations**. When a dynamic guard forces `residualize_while`, the body is
specialized once with every escaping var dynamicized; if the store index or code
stack `Cd` is among them, the interpreter's own loop residualizes *whole* (the
diagnosis in `docs/FUTAMURA.md`: constant ~265 KB residual, guard = `Cd`). A
per-loop spine trial does **not** fix it (attempted 2026-06-20, byte-identical
residual, reverted): the residualizing store-walk is reached through *nested /
cascaded* residualizations that a single loop's trial never sees. Sharing must be
**global**, keyed by configuration — i.e. polyvariant program-point specialization.

## The classical algorithm to port (Glück 2012 / JGS 1993 ch.4)

Online **recursive polyvariant specialization** (Glück 2012, our exact setting:
online × self-applicable × flowchart):

- **Configuration** `cfg = (pp, σ_s)`:
  - `pp` = the static program point — the position in the code stack `Cd` (for
    `usi`/`universal`, the remaining object-program command list).
  - `σ_s` = the **static projection** of the store: the spine-preserving slot
    representation (`Sv`/`Cv` spine; dynamic leaves abstracted to a single ⊥
    placeholder). Two stores with the same spine shape at the same `pp` are the
    *same* configuration even if dynamic leaf *values* differ — this is what makes
    store-walk loops at a fixed `pp` collapse.
- **Memo table** `seen : cfg ⇀ name` and **worklist** `pending : cfg list`.
- **Specialize(cfg)**: if `cfg ∈ seen`, emit a *reference* to its residual (the
  fold); else allocate a fresh `name`, mark seen, specialize the construct at `pp`:
  - static control ⇒ unroll into the successor configuration(s) as today;
  - **dynamic control** (D-`while`/D-`if`) ⇒ compute the post-state configuration,
    `Specialize` it (enqueue), and emit residual control that *references* it.
- **Folding** (Sørensen–Glück–Jones 1996): a back-edge to an ancestor `cfg` on the
  current spine is re-expressed as the **subject's own loop**, not a fresh block.

## Output form — the WHILE-without-goto problem

JGS residuals are labelled flowcharts; WHILE has no `goto`. Two emit strategies:

- **(a) dispatch loop** on a residual program-counter:
  `while PC do { case PC of <block_i> }`. Always correct and finite, but
  reintroduces **one** residual loop — i.e. exactly today's `universal` status
  (#while ≥ 1). Useful as a *correctness-first* milestone, **not** the optimality
  goal.
- **(b) structured folding**: when the recurring configuration set is a single
  back-edge whose `pp` is a subject loop head, emit the subject's own `while`/`if`
  around the residualized body (no PC, no dispatch). This is the **loop-free**
  goal. Falls back to (a) when structure cannot be recovered (still correct).

## Interaction with existing machinery

- **Spine-preserving emit** (C5(ii)) is *reused* for `σ_s`: the static projection
  of a slot is its spine with dynamic leaves ⊥-abstracted, already computed by the
  `selfref`/`shp` logic. The memo key is the spine *shape*, not leaf values.
- **`dyn_live`**: configurations created *inside* residualized dynamic control must
  ⊥-abstract dynamic leaves (a slot's runtime spine can vary per iteration), so
  the key stays sound (cf. `SpinePreservingEmit.cross-unsound`).
- **Termination / widening (item 2)**: the configuration set must be finite. Use
  the homeomorphic-embedding **whistle** (Leuschel 2002; `docs/RELATED-WORK.md`
  §C5(i)) on configurations — generalize a `cfg` that embeds an ancestor — instead
  of the current ad-hoc unroll budget.

  **Finding (2026-06-21): item-2 is not separable from item-3.** A HEm whistle on
  the *store tuple alone* (the only state today's `Cogen.ml` unroll loop has) is
  essentially **vacuous** relative to the existing budget, so there is no useful
  standalone item-2 to land before item-3. Argument: with the standard
  componentwise (Higman) embedding on the loop-carried tuple, the whistle fires only
  if *every* carried component embeds (grows). But unbounded **static** growth of an
  accumulator requires a **shrinking** static driver — otherwise the loop also
  diverges at *runtime* (an infinite real loop, not a specialization artefact). That
  driver is loop-carried and shrinks, so its component fails to embed, so the
  tuple-whistle cannot fire before the budget does (e.g. `count.while`: counter
  shrinks, acc grows ⇒ no tuple embedding; the budget remains the actual backstop).
  The non-vacuous whistle needs the embedding to range over **program-point–indexed
  configurations** `(pp, σ_s)` (growth in the residual/call structure, not the flat
  store) — i.e. it only has teeth *inside* item-3's memoization. Conclusion: fold
  the HEm whistle into item-3 **M1** as the configuration-finiteness guard; keep the
  unroll budget as the standalone backstop until then. (The earlier "do item-2
  first" ordering is therefore retired.)

## Soundness (gap G1 alignment)

Each configuration is residualized once; the residual is the union of blocks
(strategy a) or the folded structured program (strategy b). The correctness
obligation `[residual](d) = [p]((s,d))` is the same congruence as gap G1; the
template is rwhile's `RWhileAVSound`/`RWhileAVSpec`. Milestone M1 (below) is the
place to introduce the Agda model of the memo loop.

## Milestones (each ends green; revert on red)

- **M1a — configuration machinery — DONE** (`proj_av`, `config`, `cfg`, `Fold`,
  `loop_stack`; commit `7917ebd`, gated `COGEN_MEMO`, behaviour-neutral).
- **M2 — structured folding for single-loop subjects — DONE** (commit `b587874`,
  `test-item3-m1.sh`). The memoizing driver folds the subject's loop and keeps the
  interpreter code pointer static inside it: `usi`×`rev` → `#while=1` (rev's own
  loop), `#eq=0` (interpreter dispatch unrolled), correct, 87 KB → 28 KB.
- **M3 — the book's `universal` + nested control — IN PROGRESS; the single-pass
  global-memo redesign** (NOT a patch on M2). The M2 trial-based prototype is proven
  not to extend (`docs/ITEM3-M3-FINDINGS.md`): non-loop-correctness and loop-folding
  cannot both own the continuation in the trial architecture. Drive ONCE with a
  global `seen : config ⇀ name`, fold on any recurrence, reconstruct structured WHILE.
  Built behind a separate gate `COGEN_M3` so the proven default + M2 (`COGEN_MEMO`)
  paths stay byte-identical. Decomposed into green sub-milestones:
  - **M3a — correct global-memo driver — DONE.** The single-pass worklist driver
    (`drive2` + `m3_specialize`/`m3_run_block`, gated `COGEN_M3`) is LIVE: a
    configuration `(pp, σ_s)` is looked up in the global `m3_seen` at every driving
    point, a hit is a back-edge to that block (any ancestor **or** sibling), the
    residual is accumulated as labelled blocks and reconstructed via **strategy (a)
    pc-dispatch** (`PC := entry; while PC do {if PC=?n then {block; set-PC} else …}`,
    PC a program-scanned unused slot). It is **correct** on `int_min`/`int_if`/
    `int_nest` — verified vs the `rundata` oracle by `examples/desugar/test-item3-m3.sh`
    — *including `int_if`/`int_nest`, where the M2 trial prototype produced wrong
    output / blew up* (the global memo merges reconverging branches automatically,
    dissolving the manual-merge bug, M3-FINDINGS #1/#75). A **safety net** (`M3Bail`
    → `spec`) means correctness is never lost: subjects whose configuration set does
    not yet converge fall back to the proven `spec`. Strategy (a) reintroduces one
    dispatch loop (`#while=1`, `#eq≥1`) **by design** — it is correctness-first, not
    size-optimal (on a single-D-if subject like `int_if`, plain `spec`'s
    `residualize_if` is already smaller/loop-free; M3's payoff is the nested /
    control-bearing subjects, delivered by M3b).
    - **Control-bearing subjects (resolved by M3c):** `usi×rev`'s output accumulator
      spine grows every iteration, so without a whistle its config never recurs (it
      hit `COGEN_M3_BUDGET` → `spec`). The **M3c whistle** (below) now generalizes the
      growing slot, so `usi×rev` **converges** through the driver (8 blocks, correct).
  - **M3b — structured folding → loop-free — DONE (single-loop + multi-block).**
    Two reconstructors in `Cogen.ml`, tried by `drive2` before the pc-dispatch
    fallback. (i) `m3_reconstruct_structured` — the fast path for the **canonical
    3-block self-loop** (`E:TIf(g,B,X)`, `B:TIf(g,B,X)` self back-edge, `X:THalt`) →
    `Estmts; while g {Bstmts}; Xstmts`. *Witness:* `int_loop` under `COGEN_M3` is
    **byte-identical to plain `spec`** (808 B, `#while=1`, `#eq=0`) vs M3a pc-dispatch
    2378 B/`#eq=1`. (ii) `m3_reconstruct_general` — a **general reducible-flowgraph →
    structured-WHILE** reconstruction: iterative **dominators** → back-edges (`u→v`
    with `v dom u`) → loop headers; a recursive region `emit` (forward `TGoto` inlines;
    a back-edge stops; a `TIf` at a header emits `H.stmts; while g { emit(in-loop) ;
    H.stmts }; emit(exit)`; a non-header `TIf` emits a residual `if`). Conservatively
    **bails to pc-dispatch** (returns false) on irreducible graphs, headers with >1
    back-edge, or any block with ≥2 forward in-edges (non-tree join). *Witness:*
    `usi×rev` (a peeled-prologue + single natural loop, 8 blocks) folds to **loop-free
    `#while=1, #eq=0`** (68943 B; control counts match M2's 28016B/1/0 — larger only
    because the peeled prologue is inlined, not memo-shared) — Jones-optimal control
    in the *correct* driver. Asserted in `examples/desugar/test-item3-m3.sh`.
    Diagnostics: `COGEN_M3_NOSTRUCT=1` forces pc-dispatch; `COGEN_M3_DUMP=1` prints
    the block graph. *Falls back to pc-dispatch:* `int_nest` (reconverging/join graph,
    not a tree-prologue+single-loop) — correct via (a). *Out of reach until it
    converges:* `universal` (see below).
  - **M3c — HEm-whistle generalization — DONE.** A homeomorphic-embedding whistle
    (`emb`/`store_embeds` in `Cogen.ml`, `'D` as the embedding-bottom: coupling +
    diving on the `'C` spine; self-checked) makes the configuration set finite. The
    driver threads the **ancestor configs** down each path; in `m3_specialize`, before
    allocating a fresh config `C`, if an ancestor `A` (same continuation) embeds `C`
    (i.e. `C` is *growing*, the item-2 whistle — which only has teeth at the
    configuration level, see §"Termination / widening"), it **generalizes**: the slots
    where `A`'s projection differs from `C`'s are dynamicized (`Dv`), and a *fixup
    block* `[var k := lift(value)]; goto G` materializes each generalized slot into a
    residual var before jumping to the generalized config `G` (the proven `dyn1`/
    `dynList` step). `G`'s grown slots are now `'D`, so it can't re-trigger the whistle
    (`'C ⋬ 'D`) — no regress; `COGEN_M3_BUDGET` remains the ultimate backstop.
    *Witness:* `usi×rev` now **converges** through the driver — config set finite at
    8 blocks (was diverging past 60), correct vs the oracle on every input, no `spec`
    fallback — and the multi-block M3b reconstruction (above) folds it to **loop-free
    `#while=1, #eq=0`**.
  - **Remaining (separate line): the book's `universal` and the `case Cd,St of`
    binding-time wall.** `universal×swap` still bails to `spec` (correct, `#while=1`).
    The cause is **not** a missing whistle/projection: `universal`'s `case Cd, St of`
    conjoins the *static* code-stack test (`Cd`) with a test on the *dynamic* value
    stack (`St`), so the dispatch becomes dynamic, `Cd` is dynamicized, and the
    interpreter's main loop residualizes wholesale (the store-index **binding-time
    wall**, `docs/FUTAMURA.md`; ~210 KB). No configuration projection over the store
    recovers a finite, recurring config set while the dispatch tests dynamic `St`.
    `usi` (`case Cd of` — guard on `Cd` alone, `St` read via hd/tl inside arms) is the
    project's *deliberate reformulation* that removes the wall, and the M3 driver makes
    `usi`×control-bearing loop-free (above). *Measured (2026-06-25):* `universal×swap`
    bails to `spec` at **every** budget (`COGEN_M3_BUDGET` 50/200/800 all give the same
    210919 B == `spec`), and the config trace shows the program point recurs (3 distinct
    continuations) while the store projection never does (30 fresh configs and counting)
    — i.e. divergence is store-driven, not a budget shortfall. So reaching
    `universal×swap #while=0`
    requires a **binding-time improvement** of the `St` dispatch ("The Trick" / bounded
    static variation / arity-raising the value stack), which is **orthogonal to the
    global-memo driver** — a distinct research line, not a continuation of M3a/b/c.
    *(This mirrors the rwhile store-index BT-wall finding;
    `rwhile-C-ocaml/analysis_store_bti_classical_ref.md`.)*

    **Finding (2026-06-25): the case-guard BTI alone is necessary but NOT sufficient.**
    A first binding-time improvement was tried and reverted (negative result): a
    *dispatch-splitting* `transCaseBTI` desugaring that matches the first scrutinee
    (`Cd`) exactly but the secondary scrutinees (`St`) **irrefutably** — skipping the
    `consp St` shape tests (sound under the stack invariant), keeping only refutable
    literal tests (the genuine `'dowh`/`'doif` control). This **is** semantics-
    preserving (BTI'd `universal` ≡ `universal` on the corpus, verified via `rundata`)
    and shrinks the desugared interpreter, but `universal`-BTI × `rev` **still diverges**
    under the driver — 60+ configs / 3 distinct continuations, exactly like `universal`,
    vs `usi`'s 8 (converges). So fixing the case-GUARD binding-times does not by itself
    crack the wall: `usi`'s convergence is **also structural** — beyond `case Cd of`, it
    threads the value stack `St` so the store projection stabilizes (St stays dynamic
    rather than gaining a growing partially-static spine). The remaining gap to
    `universal×swap #while=0` is therefore this St/store-threading reformulation (which
    `usi` embodies), on top of the guard BTI — confirming that `usi` is the *whole*
    resolution, not just its `case Cd of` guard. The experiment is in git history
    (reverted); reconstruct from this note if pursuing the St-threading step.
- **M4 — proof-driven soundness/termination of the memo driver — DONE (design specs).**
  The M3 driver is now machine-checked (`--safe`), not only differential-tested, via
  three modules added on top of the G1 stack + `MemoFold`:
  - **(1) reconstruction (general/nested):** `MemoReconstructGen.Recon.loop-sound` — the
    header flowchart loop ≡ the emitted `sH; while g {body; sH}; exit` for a **partial
    body AND partial exit** (`S → Maybe S`), so loops nest in bodies AND compose with
    arbitrary (incl. looping) continuations; `branch-sound`/`seq-sound` cover the
    if/forward-edge rules. (Generalises the single-loop `MemoReconstruct` kernel.)
  - **(2) whistle termination:** `WhistleTerminates` — a generalization strictly
    increases the dynamic-slot count (`gen-strict`), bounded by the slot count
    (`trues-bound`), so it fires finitely often (`gen-bound`). The implementation's
    monotone-bounded argument (a dynamicised slot is terminal), not a full Higman wqo.
  - **(3) global-memo loop:** `MemoDriverSound.Driver.drive-transparent` — memoization
    is transparent (a consistent memo ≡ re-specializing each config) + `store-preserves`
    (the worklist keeps the memo consistent). So sharing configs adds no semantic
    change; per-config soundness is G1, fold/reconstruct are `MemoFold`/`MemoReconstructGen`.
  Together these close the proven-vs-tested gap for the M3 driver's principles (the
  exact OCaml remains differential-tested; these are faithful design specs, the
  project's established style).

## Risks / fallbacks

- Structured folding may fail to recover a subject loop ⇒ fall back to strategy (a)
  (correct, one dispatch loop) — never lose correctness.
- Configuration blow-up without the whistle ⇒ do item-2 first.
- This is **not** a single-round change; M1 alone is a session. Keep the suite
  green at each milestone; the value-driven specializer for straight-line subjects
  (C5, already done) stays the fallback path.

## References

`docs/RELATED-WORK.md` §item-3 (Glück 2012; JGS 1993 ch.4; Sørensen–Glück–Jones
1996), §C5(i) (Leuschel whistle), §C5(ii) (Mogensen / spine-preserving emit);
`docs/FUTAMURA.md` (the diagnosis and the reverted per-loop-trial finding);
`docs/AGDA_CORRESPONDENCE.md` (G1/G2).
