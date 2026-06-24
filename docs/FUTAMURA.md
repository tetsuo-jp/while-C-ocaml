# The three Futamura projections for WHILE

Status (2026-06-19): **fp1 done & tested**; **fp2 (compiler generation) DONE &
verified** — `comp2 = [mix]((⌜mix⌝ . ('S.⌜int_min⌝)))` terminates (9.36 MB,
~42 min, native) and `[comp2](('S.op)) == [mix]((int_min . ('S.op)))` byte-for-
byte for `op ∈ {swap, id}`, end-to-end correct, and demonstrably *uses* `op`
(swap-target ≠ id-target). Got there via: MKAV binding-time input, first-class
`'if` programs-as-data (`--data-if`), dynamic-`if` residualization (the spec_av
`'cond` analogue), and the **emit-on-dynamic-assignment + dynamic-`while`
residualization** that makes self-application terminate. **fp3 (cogen)**
`= [mix]((⌜mix⌝.('S.⌜mix⌝)))` also **terminates** (29.6 MB, ~2 h); verifying
`[cogen](('S.⌜int_min⌝)) = comp2` means re-running the ~40-min fp2 work through
an interpreter, so it is a multi-hour check (in progress). `comp2`/`cogen` are
large (correct *interpreting* compilers, like rwhile's 3.3/6.6 MB ones);
shrinking them to Jones-optimal compilers is the PAT-READ-ITER / store-sizing
work (an **optimality**, not a correctness, gap). A **trivial specializer**
`mix_triv.while` (no optimization) makes all three projections tiny and instant
(fp3 cogen 8.9 KB, ~0.01 s) — see below.
Ported from the *reversible* specializer
`rwhile-C-ocaml/examples/spec_av.rwhile` to the standard (non-reversible) Jones
WHILE of this repository. The reversibility machinery (XOR self-clear /
`INV-AUX`) drops out; what remains is the binding-time / self-application
difficulty, which is the same one rwhile documents in
`FINDINGS_reversible_projections.md`.

## The setup

A specializer (partial evaluator) `mix` with the contract

```
[mix]((P . ('S . s)))  =  comp        and        [comp](d) = [P]((s . d))
```

— every program in the game reads ONE variable holding a pair `(static . dynamic)`;
`mix` bakes the static head `s` of `P`'s input and leaves the dynamic tail `d`
to `comp`. Given such a `mix`:

| projection | equation | meaning |
|---|---|---|
| **fp1** (compile)      | `[mix]((int . src)) = target`   | specialize an interpreter to a source = compile it |
| **fp2** (compiler-gen) | `[mix]((mix . int)) = compiler` | specialize `mix` to an interpreter = a compiler |
| **fp3** (cogen)        | `[mix]((mix . mix)) = cogen`    | specialize `mix` to itself = a compiler generator |

Files: [`mix.while`](../examples/desugar/mix.while) (the specializer),
[`int_min.while`](../examples/desugar/int_min.while) (a minimal interpreter, the
fp1 target — cf. rwhile's `ri_min.rwhile`), and
[`test-futamura.sh`](../examples/desugar/test-futamura.sh).

## `mix` design (annotated values)

`mix` is a partial evaluator over **annotated values (AV)**:

```
('S . v)            static value v
('D . e)            dynamic; e is residual EXPRESSION code (⌜ ⌝ form)
('C . (a1 . a2))    partially-static cons of two AVs
```

It is a universal-program-style stack machine (the same `Cd`/`St` shape as
[`universal.while`](../examples/desugar/universal.while)) whose store `Vl` holds
AVs. Static operations compute on AVs (`avhd`/`avtl`/`aveq`, `('C ...)` for
cons); dynamic operations build `('D . code)` / `('C ...)`. The **MKAV** input
rule (rwhile's binding-time finding) builds the read variable from the
binding-time tag `BT`: `BT='S` → partially-static `('C.(('S.s).('D.(var i))))`
(so `hd In` folds to the static `s`, `tl In` stays the dynamic input);
`BT='D` → fully dynamic `('D.(var i))`. The interpreted program uses a
**first-class conditional** `('if . (E . (C1 . (C2 . nil))))`:

- **static guard** `('S.v)` → pick a branch (unroll);
- **dynamic guard** → residualize: snapshot the store, specialize *both*
  branches, **merge** the result stores (slots equal in both kept as-is, slots
  that differ dynamicized to `('D.(var k))` with a per-branch fix-up assignment
  `(var k) := lift(slot)`), and emit `('if . (lift guard . (B1 . (B2 . nil))))`.
  This uses only **static** control inside `mix`, so its code stack `Cd` stays
  static under self-application — the property that unlocks fp2/fp3, and the
  analogue of spec_av's `'cond` handler.

`mix` accumulates residual commands in `RC` and at the end emits
`read (var i); RC ; (var j) := lift Vl[j]; write (var j)`.

## fp1 — DONE

`[mix]((int_min . ('S . 'swap)))` yields, decoded,
`read In; In := cons 'swap (cons (tl In) (hd In)); write In`, and
`[comp](('a.'b)) = ('swap.('b.'a)) = [int_min](('swap.('a.'b)))` (the `'id` case
is the identity). The static guard `=? Op 'swap` picks the branch at spec time.

## dynamic-`if` residualization — DONE

`[mix]((int_min . ('D . nil)))` (BT=`'D`, so the guard `=? Op 'swap` stays
dynamic) yields a residual that **retains a first-class `'if`**:

```
read In;
  if =? (hd In) 'swap then In_X := cons (tl (tl In)) (hd (tl In))
                      else In_X := tl In;
  In := cons (hd In) In_X;
write In
```

verified against the oracle for `'swap`, `'id`, and `'foo` (both branches).
`test-futamura.sh` covers fp1 and this. This is the spec_av `'cond` mechanism in
the non-reversible setting.

## first-class-`if` data — DONE (`DesugarWhile --data-if`)

`mix`'s own `if`s (e.g. `if =? BT 'S`) would otherwise be desugared into
`while`, becoming *dynamic loops* under self-application and clobbering `mix`'s
control. The new **`--data-if`** mode (Desugar.hs `desugarIf` = the pipeline
without `expandIf`; ProgToData.hs `progToDataIf`) emits `⌜mix⌝` with first-class
`'if` nodes, which the outer `mix` handles by the dynamic-`if` rule above
(static control), keeping `Cd` static. `⌜mix⌝` is ~1.2 MB.

## fp2 — compiler generation, DONE & verified

`comp2 = [mix]((⌜mix⌝ . ('S . ⌜int_min⌝)))` (with `--data-if` for `⌜mix⌝`),
run with `while.opt`: **terminates** — 9.36 MB, ~42 min, peak RSS 1.29 GB.
Verified with `src/core/rundata` (a native interpreter for the `⌜p⌝` form — the
oracle for residuals too large to run via `universal.while`; rwhile's `d2p`
analogue):

- `[comp2](('S.'swap))` is **byte-equal** to `B = [mix]((int_min.('S.'swap)))`
  (the fp1 residual, 726 B); `[comp2](('S.'id))` byte-equal to `B_id`;
- swap-target ≠ id-target, so `comp2` genuinely **uses** its `op` input
  (it is not a baked constant — the over-static fear below does *not* break it);
- end-to-end `[[comp2]('S.'swap)](('a.'b)) = ('swap.('b.'a))` and
  `[[comp2]('S.'id)](('a.'b)) = ('id.('a.'b))`.

So `comp2` is exactly a compiler for `int_min`: `[comp2]((BT.S)) =
[mix]((int_min.(BT.S)))`. That is the **second Futamura projection**.

### What made it terminate, and why it is correct (the barrier, resolved)

The barrier under self-application (rwhile's "MKAV necessary but not sufficient"):

1. with outer `BT='S` the inner `mix` reads its program's static head as
   `Op = ('S.op)` — **tag static, value dynamic-to-outer**;
2. `aveq(('S.op),('S.'swap))` returns `('S.(=? op 'swap))` (static tag over a
   dynamic value), so the inner `'doif`/guard handling routes `op`-dependent
   control through `mix`'s machinery and the inner code stack `Cd` is
   **dynamicized**;
3. the outer `mix` then meets the inner main loop `while Cd` with `Cd` dynamic.

Earlier `mix` (static `while`/`if` only) **hung** here. The fix that unblocked
fp2 is **gap #1**: the **emit-on-dynamic-assignment + dynamic-`while`
residualization** below. Now a dynamic `Cd` loop is *residualized* (the outer
emits a residual `while` whose body re-runs the inner interpreter step), so
specialization **terminates**, and the residual `comp2` re-does the
`op`-dependent work at *its* run time — hence **correct**. This is exactly
rwhile's outcome: a large *interpreting* compiler (theirs 3.3 MB, ours 9.36 MB),
correct but not Jones-optimal. The over-static `('S.dyn)` read (`MixPatRead`)
therefore costs **size/optimality, not correctness**; shrinking `comp2` to a
small compiler is the PAT-READ-ITER / store-sizing work (gap #2/#3).

(rwhile's `HANDOFF_fp2.md` §0b records the same over-static chain; their `comp2`
was generated but *broken* at the depth-1-read stage, fixed by **PAT-READ-ITER**;
fp3 needed **store-sizing** `FpN`=256 / `specsize`, `comp3` ~6.6 MB.)

### Reproduce fp2

`examples/desugar/test-fp2.sh` (slow, ~40 min): builds `⌜mix⌝` (`--data-if`),
runs `comp2 = [mix]((⌜mix⌝.('S.⌜int_min⌝)))`, and checks `[comp2](('S.op))`
against the fp1 residual with `src/core/rundata`.

## fp3 — cogen, terminates

`cogen = [mix]((⌜mix⌝ . ('S . ⌜mix⌝)))` (mix specialized to itself) **terminates**:
29.6 MB, ~2 h, peak RSS 1.68 GB (`while.opt`). By construction `[cogen]((BT.S))
= [mix]((⌜mix⌝ . (BT.S)))`, so `[cogen](('S.⌜int_min⌝))` should reproduce the fp2
compiler `comp2`, and `[[cogen](('S.⌜int_min⌝))](('S.'swap)) = B`. Verifying that
means running the 29.6 MB `cogen` (it re-does the ~40-min fp2 computation through
the `rundata` interpreter), so it is a multi-hour check rather than a quick test;
the termination itself is the third-projection milestone (cogen exists and is
finite). Same large-interpreting-compiler caveat as fp2.

## Trivial specializer — all three projections, fast

`examples/desugar/mix_triv.while` is **Jones's trivial specializer** (the `papp`
closure of rwhile's `RWhileFutamura2Inst`, residualized structurally for WHILE):
same contract as `mix` (`[mix_triv]((P.(BT.S))) = comp`, `[comp](d)=[P]((s.d))`),
but it does **no** optimization — for `BT='S` it bakes `s` as a constant and
prepends `(var i) := cons (quote s) (var i)` to `P`'s body, so the residual just
reconstructs `(s.d)` and runs `P`. Work is `O(|P|+|s|)`, so the whole hierarchy
is tiny and instant, including self-application:

| | optimizing `mix` | **trivial `mix_triv`** |
|---|---|---|
| fp2 `comp2` | 9.36 MB, ~42 min | **5.5 KB, ~0 s** |
| fp3 `cogen`  | 29.6 MB, ~2 h   | **8.9 KB, ~0.01 s** |

Verified by `examples/desugar/test-fp-triv.sh` (~0.1 s total): `[cogen]('S.⌜int_min⌝)`
byte-equals `comp2`, `[[cogen]('S.⌜int_min⌝)]('S.op)]` computes the correct
int_min target for `op ∈ {swap,id}` and is op-sensitive. This shows the Futamura
hierarchy is **instantiable and non-vacuous** with a fast self-applicable
specializer; `mix.while` is the genuinely *optimizing* counterpart.

The size **signature** matches rwhile's trivial specializer (see
[NOTE-fp3-speed-from-rwhile.md](NOTE-fp3-speed-from-rwhile.md)): `⌜mix_triv⌝`
≈ 4.4 KB, `comp2` ≈ 1× (5.5 KB, it freezes the spec with the program baked),
`cogen` ≈ 2× (8.9 KB) — exactly the "embed-only, no self-optimization" signature
(rwhile's `comp2`≈1×|spec|, `comp3`≈2×|spec|). The optimizing `mix`'s `cogen` is
instead ~12×|mix| because it genuinely expands/specializes under self-application
(the harder task; per that note, the speed gap is the *amount of specialization*,
not the residual-running path).

### Remaining work (shrinking the optimizing residuals)

1. **Dynamic-`while`/loop residualization** — **DONE** (proof-backed). `'doasgn`
   was rewritten to the **emit-on-dynamic-assignment** model
   (`MixEmitModel.emit-sound`): a fully static value folds; anything dynamic/
   partial emits `(var k) := <lift rhs>` and sets the slot to the self-reference
   `('D.(var k))`, so no cross-variable `'D` reference can go stale when the
   store is materialized (`MixEmitModel.fold-unsound`). A `dynall`
   (`DYNAMICIZE-ALL`) procedure plus a dynamic-guard `'dowh` arm
   (`'whmk`/`'whend` markers) residualize a `while` whose guard is dynamic:
   flush the store, re-evaluate the guard, specialize the body once, emit the
   residual `while`. Verified by `examples/desugar/int_loop.while` in
   `test-futamura.sh` (the dynamic loop is kept as a residual `while` and runs
   correctly), with fp1 / dynamic-`if` still green. This is what lets
   self-application **terminate** (the residual store fixpoint is all-dynamic),
   at the cost of large, interpreting `comp2`/`comp3`.
2. **fp3 (cogen) correctness check** — `cogen` already **terminates** (29.6 MB);
   confirming `[cogen]('S.⌜int_min⌝) == comp2` (hence
   `[[cogen]('S.⌜int_min⌝)]('S.'swap) == B`) is a multi-hour `rundata` run
   (in progress).
3. **Shrinking the residuals (optimality, not correctness)** — in layers:
   - **No-op elimination at generation — DONE & verified** (`mix.while`). Two
     sources of `(var k):=(var k)`: (i) `'doasgn`/assemble (fp1's case) and
     (ii) the **dynamic-if merge fix-ups** when a branch leaves a slot self
     (the fp2/fp3 bulk source). Both now skipped at generation. Measured:
     fp1 comp **726 → 592 B**; **comp2 22.6 → 9.36 MB (−59%)**, verified correct
     (`[comp2](('S.op))` equals the current-mix fp1 residual for swap/id,
     end-to-end, op-sensitive). Matches `simp`'s post-hoc reduction but at the
     source, and avoids the wasteful per-no-op `O(n)` store update (less of the
     46 GB churn). `simp(comp)==comp` now (no residual no-ops left).
   - **`src/core/simp`** remains as a post-pass/oracle for any residual; it also
     has a sound **global dead-store elimination** pass. Tellingly, DCE removes
     **nothing** from `comp2` (9.2 MB → 9.2 MB, byte-correct): after no-op
     removal the residual is **all live** — there is no slack for local/global
     simplification to recover. This *confirms* the leftover is the genuine
     interpreting-compiler structure, removable only by PAT-READ-ITER (below),
     not by any peephole/DCE cleanup.
   - **Store sizing** is, *here*, a **speed**-only lever (faster generation): our
     `dynall` already skips `('S.nil)` and self slots, so an oversized store adds
     ~no comp bloat (unlike rwhile's reversible store-clear, the source of its
     1055-node garbage). So it is *not* the lever for comp size in this repo.
   - **PAT-READ-ITER — the fundamental, remaining ~5×→~2× (research-scale)**.
     The leftover bloat is `mix`'s own interpreter loop residualized under
     self-application, because the over-static read makes a source guard
     `('S.dyn)` (`MixPatRead`) route through `mix`'s *static* control (assigning
     `Cd`), which the outer `mix` then dynamicizes. The proper fix is to make the
     read of a partial-static input residualize when its static half is dynamic
     one level up — but at `mix`'s own (online) level that value *is* static, so
     `aveq` cannot tell. This is the **online-vs-offline divide**: it needs a
     two-level / binding-time-annotated AV (a "static-but-symbolic" category) or
     a separate BTA pass — exactly what rwhile flags as large research and what
     `MixPatRead`/`RevSpecGarbage` characterize. Not closeable as a local patch.

### Toward an offline specializer — BTA done (`src/core/bta`)

The first half of an *offline* PE is built: `src/core/Bta.ml` is a monovariant
**binding-time analysis** with a partially-static domain (`S`, `D`, `C(b1,b2)`),
needed because the input `In=(s.d)` is part static / part dynamic. From a
binding-time *division* of the input it annotates every variable and conditional
without looking at values. It shows exactly the distinction the online `mix`
cannot make at self-application (`test-bta.sh`):

```
bta int_min SD   ->  var Op:S, var X:D, if #0 : S-if   (op static  = fp1: unroll, bake op)
bta int_min D    ->  if #0 : D-if                      (op dynamic = fp2 ctx: residualize)
bta int_loop SD  ->  while #0 : D-while                (drains the dynamic tail)
```

So the *annotation* (`D-if` when op is dynamic) pre-decides "residualize this
conditional" with no runtime-value dependence — the property that would keep
`comp2`/`cogen` small.

**The offline payoff, demonstrated** (`examples/desugar/gen_int_min.while`,
`test-genext.sh`): the **generating extension** of `int_min` (its offline
compiler — `gen(op)` emits the specialized target, `[gen(op)](d) =
[int_min]((op.d))`) is **528 bytes**, versus the online `mix`'s **9.36 MB**
`comp2` — the *same* function, ~18000× smaller, because the binding-time
annotation `op:S → S-if → unroll` removes the interpretation overhead. Verified
correct for swap/id/foo (`rundata` runs the emitted targets).

**Automated** (`src/core/cogen`, `test-cogen.sh`): `cogen p s` is the general
annotation-driven offline specializer — it runs the BTA, then reduces Static
constructs and residualizes Dynamic ones over partially-static annotated values
(so `In=(s.d)` is handled: `tl In` residualizes to the residual's input var). For
`int_min` it auto-produces the small targets the hand-derived `gen_int_min`
emits — 381–592 B, `[cogen(int_min,op)](d) = [int_min]((op.d))` for swap/id/foo —
from the annotations, no hand work.

**Dynamic control** is handled too: a dynamic guard residualizes the construct
rather than unrolling it. `cogen` keeps the `while`/`if`, specializes the body /
both branches once, and **dynamicizes** every variable the body assigns — the BTA
raises any assignment *under a dynamic guard* to `D` (without this rule a loop like
`while D do X:=cons 'a X` would wrongly keep `X` static), and the specializer seeds
those vars to their residual reference before residualizing. Verified on
`int_loop` (D-`while`, drains a dynamic list → 599 B target), `int_if` (D-`if`,
branch-merge → 777 B target), and `int_nest` (a D-`if` nested in a D-`while`):
`[cogen(p,s)](d) = [p]((s.d))`, checked differentially over boundary inputs + a
small fuzz in `test-cogen.sh` (104 cases, both modes, oracle-validated).

**The generating extension, automated** (`src/core/cogen --gen`, same
`test-cogen.sh`): where `cogen p s` (above) emits the *target*, `cogen --gen p`
emits **`⌜gen⌝` itself** — a standalone WHILE *program* (the offline compiler as a
program) with `[gen](s) = ⌜target⌝` and `[target](d) = [p]((s.d))`. This is the
true Futamura **cogen** output: the hand-derived `gen_int_min.while` produced *by
the transformation* instead of by hand. For `int_min` it is a 2.9 KB program; its
emitted targets (515–726 B) run correctly for swap/id/foo. The transformation
maps each construct to gen-code: static exprs become value computations, dynamic
exprs become residual-code *builders* (the `cons`-trees of `gen_int_min`), S-`if`
becomes a real `if` in `gen`, D-assigns append to a residual accumulator.
**Dynamic control is generated too**: for a D-`while`/D-`if` the generating
extension builds a residual `while`/`if` term — it threads a fresh sub-accumulator
gen-var per loop body / branch, then conses `('while guard body)` / `('if guard
b1 b2)` onto the outer accumulator. Verified on `int_loop` (`⌜gen⌝` 3.1 KB) and
`int_if` (`⌜gen⌝` 4.3 KB): `[[gen(p)](s)](d) = [p]((s.d))`.

### The self-applicable offline specializer (`spec.while`)

`examples/desugar/spec.while` is the offline specializer *as a WHILE program* — the
self-applicable `mix` of Jones–Sestoft–Søndergaard (1985). It consumes
`(⌜p_ann⌝ . s)` where `⌜p_ann⌝ = annotate ⌜p⌝` (the BTA annotator, `src/core/
annotate`), and emits `⌜target⌝` with `[target](d) = [p]((s.d))`. Unlike
`mix.while` (online: it infers binding times by inspecting AV tags at spec time),
`spec` **dispatches control on the annotation tags** `'asgnS/'asgnD`, `'ifS/'ifD`,
`'whileS/'whileD`. Because the control is then fixed by the *static* program,
specializing `spec` to a known `p_ann` folds that control away — the route to
*small* compilers under the projections. It reuses `mix.while`'s `Cd`/`St` stack
machine and AV procedures (`avhd/avtl/avlift/aveq`, `lookup/update`, `makeseq`,
`merge`, `dynall`); static control unrolls, D-`while`/D-`if` residualize.

**M1 done** (`test-spec.sh`, test-first): `[[spec]((⌜p_ann⌝ . s))](d) = [p]((s.d))`
for int_min (S-if), int_loop (D-while), int_if (D-if), int_nest (nested) — the
oracle via `rundata`, validated against `while` by `test-oracle.sh`.

Still remaining: **Phase D — self-application** — `comp = [spec]((⌜spec_ann⌝ .
⌜p_ann⌝))` and `cogen = [spec]((⌜spec_ann⌝ . ⌜spec_ann⌝))`; measure `|comp|` vs the
online `mix`'s 9.36 MB to see the annotation-driven payoff (and whether the
remaining AV-expression dispatch needs arity-raising to a pure-S/D store).
Measured: spec self-application does **not** complete in 75 min (RSS flat at
22 MB) — the O(n) AV-store walked over the 2.4 MB `⌜spec_ann⌝` is impractical;
spec is correct (M1) but not yet practically self-applicable.

### Jones-optimality — diagnosis (`test-jones.sh`)

The classic benchmark of a *good* specializer is **Jones-optimality**:
specializing the self-interpreter `u` (`universal.while`) to a subject `p` should
give back essentially `p` (the interpretation overhead removed) —
`[mix]((u . ⌜p⌝)) ≈ p`. `test-jones.sh` measures `|comp| / |⌜p⌝|` for the
loop-free subject `swap.while`. Findings:

| specializer | `cogen(u, swap)` (offline, monovariant) | `mix(u, swap)` (online) |
|---|---|---|
| size ratio | **540×** | 698× |
| correct?   | yes | **no** (computes identity) |

Neither is Jones-optimal. Root cause (offline): the monovariant BTA collapses
**every** variable of `u` to Dynamic — including the code-stack `Cd`, which is
"really" static (it holds the program). The fixpoint is vicious: the main loop is
tentatively D-while, so the dynamic-control rule dynamicizes its body assignments
(including `Cd`), which confirms the D-while. With `Cd` dynamic the interpreter
loop is *residualized whole* instead of unrolled over the static program → the
~150 KB interpreter survives into `comp`. The online `mix` does no better (and
returns an incorrect compiler here — a separate bug to chase). This is the
textbook motivation for **polyvariant program-point specialization** (Jones–
Gomard–Sestoft 1993): specialize each program point per static store value, so the
interpreter's dispatch unrolls and `comp` collapses toward `|p|`.
`test-jones.sh` tracks the ratio (540× → ~1) as the success metric.
(The store-index dynamicity that blocks this is the WHILE-store form of the
classical **tag** problem of Jones-optimality — Makholm's *Tag Elimination and
Jones-Optimality* (PADO 2001), Danvy & López's *Tagging, Encoding, and Jones
Optimality* (ESOP 2003) — and the "BTI-universal specializer" framing matches our
documented store-index wall; see [`RELATED-WORK.md`](RELATED-WORK.md).)

**Step 1 — value-driven (polyvariant) decisions: `cogen --online`.** The first
ingredient is to stop trusting the conservative monovariant BTA and decide
Static/Dynamic from each construct's *actual* annotated value: `av_static` for the
fold-vs-emit choice, and nil-vs-cons (a `Cv` is a statically-known cons) for
if/while guards. Assignments inside a residualized dynamic if/while are still
always emitted (the `under_dyn` rule, the spec-time analogue of the BTA's dynamic-
control rule). `test-poly.sh` is the witness that this beats monovariant: in
`poly.while` a loop is statically controlled but its loop variable is *reused
dynamically later*, so the monovariant BTA joins it to Dynamic and `cogen`
**residualizes the loop** (1322 B, a `'while` survives); `cogen --online` sees the
guard's real value is static and **unrolls it** (695 B, no `'while`) — both
correct. (`cogen --online` is correct across the whole int_min/int_loop/int_if/
int_nest corpus too; on `universal` it is now *correct* (the `under_dyn` fix above
also fixed the earlier identity bug) but still ~1047× — not yet optimal.)

**Why `universal` is still not optimal — diagnosis.** Instrumenting `cogen
--online` on `(universal, ⌜swap⌝)` (a *loop-free* subject, so the residual should
have no loops) shows **7 residualized `while`s, all with guard `(var 8/9/10)`** —
`universal`'s internal **store-walk loops** (in `lookup`/`update` and the store
set-up). Their guard variable is `Dv`, i.e. the variable store `Vl` collapses to
*fully* dynamic, so these static-length loops cannot unroll. For Jones-optimality
the store must stay **partially static** (a `Cv`: static spine = which slot holds
which variable, dynamic leaves = the values) all the way through `universal`'s
operations. Achieving that is a design choice — arity-raise the store into
per-variable components, or restructure `universal` to a static-spine store, or add
a polyvariant store treatment — not a mechanical fix; it is the next milestone for
the time-based optimality figure (`test-time.sh`).

**Quantitative optimality, measured (`rundata --time` + `test-time.sh`).** The
book's unit-cost time (ch.16-19) is now available for residuals: `rundata --time`
mirrors `EvalWhile.timeProc` exactly (verified equal to `while --time` on the
corpus). For the cases that *do* specialize well, the residual is no slower than
interpreting the subject and often faster (e.g. `int_min` specialized to `'id`:
6 vs 16 steps ≈ 2.6×) — the measurable face of Jones-optimality, independent of the
`universal` store issue above.

**Jones-optimality reached on static-spine interpreters, incl. looping ones
(`cogen --online`, `test-calc.sh` / `test-drain.sh`).** Once the store is a single
value (no indexed variable list, no store-walk loop), value-driven polyvariant
specialization is Jones-optimal: `calc.while` (a tiny op-list interpreter)
specializes to straight-line, loop-free residual code (no `'while`), 5.5× faster.
The looping case `drain.while` (apply a static op-list to `A` *while `A`*) is the
harder one: the outer loop is over dynamic `A`, so it must residualize to one
`while`, but the inner dispatch over the static op-list should still unroll. The
obstacle was `dynamicize`: before residualizing a dynamic loop it marked **every**
body-assigned variable dynamic, including `Cd := Body` (re-initialised static each
iteration) — leaving the dispatch in the residual (`'eq`>0, ≈1.07×). The fix is a
**liveness-aware `dynamicize`** (`Cogen.ml`, `escaping_set`): a body-assigned var is
dynamicized only if it is **live-in** (read before assigned in the body, so its
value changes across iterations) or **live-out** (read after the loop, via a
continuation stack threaded through `spec`'s `'semi`). A var that is neither — a
dead loop-local like `Cd` — stays static, so the dispatch over it unrolls. This is
sound by construction (a folded var's loop-head and loop-exit values are both dead).
Result: `drain`'s residual is `while A do { <compiled Body> }` with **zero**
dispatch (`'eq`=0) and 4.1× faster — full Jones-optimality for a looping subject.
The same narrowing for **D-`if`** branches is *not* yet applied: the self-
interpreter's dispatch over-unrolls (a statically-infinite spine) and `--online`
fails to terminate, so `residualize_if` stays conservative (emit every assigned
var). Narrowing D-`if` safely needs online-termination control (bounded unrolling /
generalization) — the remaining step toward Jones-optimality on `universal` itself.

**Results table (`test-bench.sh`).** The optimality numbers are collected in one
place — residual size, residual control (`#'while`), residual dispatch (`#'eq`),
and book unit-cost time of the residual vs. interpreting the subject — and the
optimal rows are asserted (regression guard):

```
subject          mode |resid|B  #while   #eq   t_comp t_interp  speedup
int_min        offline      592       0     0       12       20      1.6x   (offline-optimal: S-control unrolls)
calc           online       582       0     0        8       44      5.5x   (Jones-optimal: loop-free)
drain          online       484       1     0       14       58      4.1x   (Jones-optimal: 1 outer loop, no dispatch)
universal      online    210919       1     1     4400     9240      2.1x   (open: store-walk loop survives)
```

Note `cogen --online` already makes the self-interpreter's residual ~2× faster
with a single residual loop and one surviving dispatch — far from the 540× offline
blow-up; the remaining gap is the store-walk loop, the target above.

**Online-termination control (`test-terminate.sh`).** Value-driven unrolling can
diverge two ways; both are now bounded:

1. *BTA fixpoint.* The partially-static domain `C` has infinite ascending chains —
   a static-control accumulator `Acc := cons D Acc` grows `C(D, C(D, …))` without
   bound, so the naive fixpoint never converges (it hung on `count.while`). A
   **widening** (`widen`, `Cogen.ml`) caps the `C`-nesting at `bt_maxdepth` and
   normalizes `C(D,D)→D`; both are sound (D is top) and make the domain finite, so
   the BTA always terminates. Machine-checked: `proofs/agda/BTAWiden.agda` proves
   `widen-sound` (`t ⊑ widen d t`, widening only moves up the lattice) and
   `depth-widen` (`depth (widen d t) ≤ d`, the domain is finite ⇒ termination).
2. *Unrolling.* A static loop guard that the value analysis keeps non-nil too long
   would unroll forever. The online `while` now unrolls under a budget
   (`COGEN_UNROLL_BUDGET`, default 1e6) and, on exceeding it, **generalizes**: it
   rolls back, materializes each loop-carried variable's entry value, makes the loop
   state dynamic, and residualizes the loop. The result is finite and correct —
   `count.while` specialized with a tiny budget over a long static list yields a
   residual `while` computing the same function as the fully-unrolled residual.

This is the safety net the value-driven specializer needs to be a total function of
its static input. *(The budget is ad-hoc; the principled, citable replacement is a
**homeomorphic-embedding whistle** — Leuschel, *Homeomorphic Embedding for Online
Termination of Symbolic Methods* (2002) — generalizing when a configuration embeds
an earlier one. Type-based HEm (Leuschel et al. 2008/09) targets infinite
signatures, the same problem our `C`-domain `widen` solves by depth-capping, so
`widen` can be re-presented as a TbHEm instance. See
[`RELATED-WORK.md`](RELATED-WORK.md) §C5(i) / gap G2.)*

*Finding (scopes the remaining work).* With termination control in place we tried
re-enabling the liveness narrowing for D-`if` (so dispatch locals fold like loop
locals). It does **not** rescue `universal`: the self-interpreter's residual now
diverges *combinatorially* — many distinct static stores each spawn fresh
specialization, 2.6 GB without finishing — which a per-loop budget cannot bound.
The cure is **program-point memoization** (item 3): share specialized
configurations `(pp, static-store)` so each is residualized once. So D-`if`
narrowing is kept off until memoization lands; `residualize_if` stays conservative.

*Root-cause diagnosis of `universal`'s blow-up (items 2/3).* Instrumenting
`cogen --online` shows the residual control is **invariant in the subject and in
the unroll budget** — `#while=1, #eq=1` for identity, swap and a 3-assignment
program alike (budget 4 vs 1e6 identical), atop a large **subject-independent floor**
(~200 KB; measured `examples/desugar/measure-item3.sh`: id 200 KB, swap 211 KB,
sl4 237 KB — the floor plus a small subject-proportional part, *not* a literal
constant size). The one residual `while` has guard **variable 9 = `Cd`, the code
stack**: the *main interpreter loop residualizes wholesale*. The
trigger is a cascade — `residualize_if` fires 26× even for identity, and its
escaping sets contain `Cd` (9), the store index `K` (8) and the store `Vl`, so once
a dynamic `if` dynamicizes `Cd`, everything the dispatch derives from it (`KK`,
`E`) becomes dynamic and the store-walk loops residualize too. Two contributing
factors were isolated: (a) the desugared multi-scrutinee `case Cd, St of` compiles
its guard as a **non-short-circuiting** conjunction (`EAnd` → `=?(cons … …)(…)`),
so the dynamic value-stack test poisons the otherwise-static `Cd` dispatch; and
(b) the monovariant BTA classifies `Cd` as `D` (it is assigned under dynamic
control in the `doif`/`dowh` arms), so `escaping_set` dynamicizes it at every
residualized `if`. A short-circuiting `case` rewrite was tried for (a): it is
*semantically* correct (universal and `reverse-case` still evaluate correctly) but
only marginally shrank the residual and regressed the specializer, confirming (b)
is the dominant cause — so the real fix is **polyvariant / program-point treatment
of `Cd`** (item 3), or a self-interpreter whose code stack is provably static
(item 2), not a desugaring tweak. (Attempt reverted to keep the suite green.)

**Item 2 — a static-spine self-interpreter (`usi.while`, `test-usi.sh`).** The
constructive route: keep the existing specializer, but write the self-interpreter so
factor (a) never arises. `usi.while` is the same STEP stack machine as `universal`,
except the dispatch is a **single-scrutinee `case Cd of`** — only the *code* stack
(static = the object program) is matched; the *value* stack `St` is read with plain
`hd`/`tl`/`cons` inside each arm, never in a guard. So the dispatch guard is fully
static and folds, and the **main interpreter loop unrolls** (the surviving residual
`while` for a straight-line subject is a store-walk on var `K`, not `Cd`). Result:
`[cogen --online](usi, swap)` is **~10 KB vs universal's ~211 KB (≈21× smaller)**,
correct; the only residual control that remains is the object program's own
`while`/`if` (the `dowh`/`doif` markers, which legitimately test a dynamic `St`
value). `usi` is verified as a correct self-interpreter on straight-line, looping
and conditional object programs.

*Loop-free residuals — spine-preserving emit (`Cogen.ml`).* This is the WHILE-store
realization of Mogensen's **partially-static structures** (1988): a value kept with
a static cons spine and dynamic leaves. Our delta is a *machine-checked* form for
the variable store, with the explicit soundness side-condition (self-references
sound, cross-references not). See [`RELATED-WORK.md`](RELATED-WORK.md) §C5(ii). The
store-walk `while`
that initially survived was a symptom of a deeper specializer issue: building a cons
over a dynamic leaf (`T := cons (hd Vl) T`) yields a `Cv` with a dynamic leaf, which
`av_static` rejects, so the assignment was emitted and the slot collapsed to
`Dv (var k)` — **losing the static spine**, so the loop walking it residualized.
The fix: on an emitted assignment whose value has a static spine, store the slot as
a `Cv` mirroring that spine with each *dynamic* leaf replaced by a **self-reference**
into `(var k)` (`hd`/`tl` paths), keeping the `Sv` parts (incl. the spine
terminator) static. The spine then stays known, so store-walks unroll, while the
self-references stay stable under re-materialisation (the `MixEmitModel` soundness
condition; the spine case is machine-checked in `proofs/agda/SpinePreservingEmit.agda`
— `readback` faithful, `stable` sound). This is sound only **outside** residualized dynamic control (`dyn_live`
empty): under a residualized dynamic `if`/`while` a variable's runtime spine can
vary per iteration, so there the slot still collapses to `Dv`. With this,
`[cogen --online](usi, swap)` is **loop-free** (`#while = 0`, no interpretation
overhead loop) and **30× faster** than interpreting through `usi` — Jones-optimal
*control* for a self-interpreter on straight-line subjects. A **control-bearing**
subject is reached by the program-point-memoization driver (item 3, below); `universal`
is the one remaining subject, but that is a binding-time wall (its `case Cd, St of`
dynamicizes `Cd`), not a driver gap — see item 3. The driver follows Glück's
**recursive polyvariant specialization** (*A self-applicable online partial evaluator
for recursive flowchart languages*, SP&E 2012) — online × self-applicable × flowchart,
exactly our setting — memoizing `(Cd-position, static-store)` configurations and folding
cycles into the subject's own loops (supercompilation folding, Sørensen–Glück–Jones
1996); see [`RELATED-WORK.md`](RELATED-WORK.md) §item-3.

**Item 3 — DONE (M3a/b/c, `COGEN_M3`, `test-item3-m3.sh`), machine-checked (M4).** A
single-pass **global-configuration-memo** driver in `Cogen.ml` (gated behind
`COGEN_M3`) specializes each configuration once, folds a re-reached config to its block,
reconstructs a reducible loop to structured WHILE, and keeps the config set finite with
a homeomorphic-embedding whistle. Result: a control-bearing subject is Jones-optimal in
control — `[cogen --online](usi, rev)` is `while X do { <compiled body> }`, i.e. rev's
*own* loop with **zero** interpreter dispatch (`#while=1`, `#eq=0`), correct. It is
correct on the non-loop/nested subjects where the earlier ancestor-stack prototype
(`COGEN_MEMO`, superseded) was wrong, and its reconstruction/termination/transparency
are machine-checked (M4: `MemoReconstruct{,Gen}`, `WhistleTerminates`, `MemoDriverSound`).
The book's literal `universal` does **not** converge under the driver — its `case Cd,
St of` makes the dispatch test the dynamic value stack, so the configuration set diverges
(measured) — the store-index **binding-time wall**; it is resolved by the `case Cd of`
reformulation `usi` (a binding-time improvement orthogonal to the driver), not by more
driving (`docs/ITEM3-DESIGN.md`). Default (memo off) behaviour is unchanged.

*Cross-repo: the same wall, independently.* The reversible sibling
`rwhile-C-ocaml` hits the identical obstacle under self-application — its
`analysis_store_bti.md` reports that `comp2 = [spec_av]((spec_av.('S.ri_min)))`
keeps **125 residual store-walk loops** because the store index (from the inner
program pointer `Cd`) goes dynamic, and proves (via a static op-list `looptest`
with `CLoop=0`) that the loop machinery is fine — the index dynamicity is the sole
cause. That is exactly the diagnosis here. Their proposed fix **candidate (A)** —
a *partially-static store* (static index↔slot skeleton + dynamic content; index
static ⇒ unroll, index dynamic ⇒ residual loop) — is what our `usi`
(single-scrutinee dispatch, so the dynamic value-stack never poisons the static
index) plus spine-preserving emit **realize for the classical case**. In rwhile
candidate (A) is still open ("最難所"); the port sketch is
`rwhile-C-ocaml/analysis_store_bti_classical_ref.md`. So the two repos
cross-validate that the wall is fundamental, and the static-spine technique
resolves it (classically here; reversibly there it additionally needs the static
selection done via `SWAP-VIA-TEMP`).

### Machine-checked design specs (Agda)

The two decisive design points are pinned down by `--safe` Agda models in
[`proofs/agda/`](../proofs/agda/) (à la rwhile's `RWhileRevProj2BT.agda`):

- **`MixBindingTime.agda`** proves the MKAV binding-time test (`if =? BT 'S`,
  already in `mix.while`) is correct and that an unconditional `'S` mis-tags
  under self-application (`fp2-mistags`) — *necessary but not sufficient*.
- **`MixEmitModel.agda`** proves gap #1's choice: the current **fold** model
  (a dynamic slot referencing another variable) is **unsound** once
  `DYNAMICIZE-ALL` re-materialises that variable (`fold-unsound`), while the
  **emit-on-dynamic-assignment** model with self-referential slots is **sound**
  (`emit-sound`). gap #1 rewrote `'doasgn` to the emit model accordingly.
- **`MixPatRead.agda`** proves gap #2's condition: even with the BT-aware
  *builder*, the **read** of the partial-static input (`avhd`) commits `s`
  static regardless of its origin (`read-ignores-origin`), so under
  self-application it mis-tags (`read-mistags`) and a downstream conditional
  bakes a branch (`naive-bakes`); the origin-aware read uses the input
  (`correct-uses`). So gap #2 must make the read of a partial-static input
  consult the static half's *origin* binding time (PAT-READ-ITER).

All three typecheck with no postulates/holes (`agda --safe`).
See `proofs/agda/README.md`.

## Theorem A — reversible-PE information lower bound (research contribution)

The classical Futamura/Jones results (specializing an interpreter, self-applying
the specializer) are exactly that — classical. The novel angle here is what
happens when the specializer must be **reversible**.
`proofs/agda/RevSpecGarbage.agda` (`--safe`) proves: a reversible realization of
a specializer `f` (an *injective* `g` with decode `d`, `d(g x)=f x`) must, for
every pair `f` MERGES (`f x₁=f x₂`, `x₁≠x₂`), keep them distinct (`separates`)
yet decode them identically (`same-residual`) — i.e. carry **garbage**; a clean
(garbage-free) reversible spec is exactly an injective one (`no-clean-if-merges`,
`clean→no-merge`). This is the Bennett–Landauer bound for partial evaluation — a
constraint classical (irreversible) PE does not face (it discards freely).

**Concrete non-injectivity witness** (`examples/desugar/ignore_static.while`,
`test-noninj.sh`): a program that ignores its static input makes `mix` discard
`s`, so `[mix]((ignore_static.('S.'a)))` and `('S.'b)` are the **same** 288-byte
residual — verified byte-equal. So `mix` is non-injective ⇒ by Theorem A a
reversible `mix` would need garbage to recover `s`. **Sharp instantiation**: in
fact *every* static value (`'a 'b 'c (x.y) nil (1.2.3) …`) collapses to that one
residual — verified — so the fibre = the **whole** static-input set. Theorem A's
lower bound (`garbage ≥ |fibre|`) and rwhile's upper bound (`input-preserving`,
`garbage ≤ |input|`) therefore **coincide** here: a reversible `mix` for this
program must retain the entire `s` — the bound is tight.

This predicts the observed trade-off: the **trivial** `mix_triv` is injective
(it bakes `(p,s)` recoverably) ⇒ clean reversible, no garbage (why rwhile's
specializer is trivial-leaning); the **optimizing** `mix` is non-injective ⇒ a
reversible version must accumulate garbage (rwhile's `spec_av_rev` dead-path
embedding). "Optimizing + reversible + clean" is impossible.

## Reference

- **[`RELATED-WORK.md`](RELATED-WORK.md)** — the canonical bibliography for the PE
  side: each existing result (Futamura; JGS 1993; Glück 2012 recursive polyvariant
  specialization; Mogensen 1988 partially-static structures; Leuschel homeomorphic
  embedding; Makholm / Danvy–López / Brown–Palsberg tag-elimination & Jones-
  optimality; Sørensen–Glück–Jones supercompilation) mapped to a component or open
  gap of this repo, with the classical-vs-novel boundary made explicit.
- rwhile: `FINDINGS_reversible_projections.md` (AV, MKAV, store-sizing, the
  verification-via-direct-eval lesson), `FUTAMURA.md`, `spec_av.rwhile`.
- Jones, *Computability and Complexity*, ch. on the universal program;
  Futamura projections / `mix` self-application: Jones–Sestoft–Søndergaard 1985.
