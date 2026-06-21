# item-3 design — program-point memoization (loop-free `universal` / control-bearing subjects)

Design spec for the one open Jones-optimality gap (`docs/FUTAMURA.md` "item 3";
`docs/RELATED-WORK.md` §item-3). This is a **multi-session build**, written down so
the architecture, milestones, and tests are fixed in advance (test-first). No code
here — `Cogen.ml` is unchanged until a milestone below is implemented.

## Goal

`[cogen --online](u, p)` should be **loop-free modulo the subject** for *every* `p`:
the only residual control is `p`'s own `while`/`if`, with no surviving interpreter
loop. Today this holds for straight-line `p` via `usi` + spine-preserving emit
(`#while=0`), but a control-bearing `p` (e.g. `usi`×`rev`) and the book's
`universal` (`case Cd,St of`) still residualize the interpreter's main / store-walk
loop. Success metric (extend `test-jones.sh`/`test-bench.sh`):

```
usi × rev        #while == (rev's own #while)      (today: interpreter loop survives)
universal × swap #while == 0                        (today: 1 interpreter loop)
```

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

- **M1 — memo table + dispatch-loop emit (strategy a).** Add `seen`/`pending` to
  `Cogen.ml`; key on `(pp, σ_s)`; emit a PC-dispatched residual. *Witness:*
  `universal`×swap residual is **constant-size in subject** no longer — it shares
  configurations (residual shrinks toward `|p|`), correctness preserved on the
  whole corpus. Still has 1 dispatch loop. Add a memo-hit counter to `test-bench`.
- **M2 — structured folding (strategy b) for single-loop subjects.** Recover the
  subject's loop for `usi`×`rev`. *Witness:* `usi`×`rev` `#while == rev's #while`,
  no dispatch (`#eq` from interpreter = 0).
- **M3 — the book's `universal`.** Same on `case Cd,St of`. *Witness:*
  `universal`×swap `#while == 0`. (May need the single-scrutinee insight already in
  `usi`, or short-circuit `case`; revisit the reverted desugaring tweak with memo
  in place.)
- **M4 — G1 congruence** for the memoized specializer (Agda, rwhile template).

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
