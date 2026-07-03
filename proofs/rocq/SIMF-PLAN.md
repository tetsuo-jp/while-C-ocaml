# simF (self-interpreter correctness) in Rocq — decomposition plan

The one remaining Rocq blocker (A1): the RECURSIVE dispatch of `simF`.  Established
obstacle (`SelfInterpCorrect.v`): with per-node fuel, the tag-walk guards need concrete
fuel while the recursive sub-run must stay opaque; `reflexivity`/`cbn` cannot close it
(3 formulations tested).  The way forward is **manual `ev_*` stepping + guard-eval lemmas
+ `eval_mono_le` fuel alignment** — never `reflexivity` on the recursive call.  Below,
each piece is a small, independently green-able lemma.

Goal:
```
simF : forall p e env v, (exists G, eval p G e env = Some v)
                       -> exists F, selfrun F p e env = Some v.
```
Recall `selfrun F p e env = eval sint F sint (enc p . (enc e . env))`; the triple's
middle `E = enc e`, and the dispatch guards read `hd E = tag of e` (CONCRETE per
constructor).  DONE (Phase 0): base dispatch `sint_var`/`sint_knil`.

## Phase 1 — guard & accessor evaluation  [S, mostly reflexivity]

Small, fully-concrete (symbolic sub-args/env just pass through):

- **`guard_tag`** (per constructor X): the tag-walk guards `tagE`, `ktl tagE`, … on a
  triple whose middle is `enc (kX args)` evaluate to the tag-X spine values
  (nonempty…nonempty, nil).  E.g. for `khd` (tag 3): `[cons,cons,cons,nil]`.
  Concrete fuel; by `reflexivity`.  (~1 lemma per constructor, or one parametric.)
- **`acc_P` / `acc_args` / `acc_R`**: `eval sint n P triple = Some (enc p)`,
  `eval sint n argsE triple = Some (enc-of-args)`, `eval sint n R triple = Some env`.
- **`acc_subtriple`**: `eval sint n (kcons P (kcons argsE R)) triple
  = Some (Vcons (enc p) (Vcons (enc e) env))` — the sub-triple built for a sub-request.
  Symbolic `p,e,env` pass through; by `reflexivity` at concrete `n`.

## Phase 2 — per-constructor dispatch reduction  [M, the mechanical crux]

Each relates the run on `kX args` to the run(s) on the sub-expression(s), the recursive
call kept OPAQUE (proved by targeted `rewrite ev_* / Phase-1 lemmas`, then
`rewrite (eval_mono_le … IH)`, NOT by `reflexivity`).  `K_X` = concrete dispatch cost.

- **`disp_khd`**: `selfrun (K3 + F) p (khd e) env = omap hd (selfrun F p e env)`.
- **`disp_ktl`**: `… = omap tl (selfrun F p e env)`.
- **`disp_kcons`**: two sub-runs; `= omap2 Vcons (selfrun F p a env) (selfrun F p b env)`.
- **`disp_kifnil`**: guard sub-run + a branch sub-run selected by its value.

**PERFORMANCE (critical, learned the hard way).**  The symmetric-fuel equations close by
kernel `reflexivity` — but only the SINGLE-sub ones (khd/ktl, ~5s) are practical.  The
MULTI-sub equations blow up: `disp_kcons_eq` ~100s, `disp_kifnil_eq` >300s — reducing the
giant `sint` symbolically with two/three recursive sub-runs is exponential.  So kcons/
kifnil (and the `sim` induction on top, >500s) MUST use the manual route below, NOT
`reflexivity`.  Offsets found in isolation: khd 12/6, ktl 13/6, kcons 12/7, kifnil 16/8.
Status: `disp_khd`/`disp_ktl` KEPT (fast); `disp_kcons`/`disp_kifnil`/`sim` DEFERRED.

Method per lemma: `unfold selfrun`; `rewrite ev_kifnil`; `rewrite (guard_tag …)` to
reduce the guard; compute `ifnil` on the concrete tag value to select the branch;
`rewrite ev_khd`/`ev_call`/`ev_kcons` + `acc_*` to build the sub-triple; the residual
`eval sint m sint subtriple` is the recursive call — do NOT reduce it; instead
`rewrite (eval_mono_le F m … IH)` (with `m = K_X - c + F`, `F <= m`) to replace it by
`Some v`; finish by `reflexivity`.

## Phase 3 — the call-free induction (`sim`)  [M]

```
sim : forall e, call_free e -> forall p env v,
      eval p (depth e) e env = Some v -> exists F, selfrun F p e env = Some v.
```
Induction on `e`; base = Phase 0; step = the matching `disp_*` (Phase 2) applied to the
IH sub-result(s), with `eval_mono_le` to lift each sub-run to the common fuel.  This is
the Agda `sim` (call-free G1), the first real milestone.

## Phase 4 — the call case & full `simF`  [L]

`call e` dispatches (tag 6) to `ireq P (ireq argsE R)` = re-interpret `p` with
`env := interpret e`.  Needs induction on the OBJECT eval fuel `G` (recursion no longer
structural on `e`).  Combine with Phases 2–3 for the full `simF`.  This is the Agda
`simF` (whole language).

## Phase 5 (parallel track) — cost / `int-efficient`  [L]

`SintCost` (per-node dispatch STEP counts over `sint`, the `K_X` as costs) then
`IntEfficient` (`int-efficient`, `a = 73`) by induction on the cost derivation.  This
discharges `LevinRunFor`'s `int_efficient_time` hypothesis, making the Rocq Levin chain
fully self-contained.  Mirrors the Agda B1.2 (CostRelation + SintCost + IntEfficient,
all ported except SintCost/IntEfficient).

## Order & sizing

| phase | pieces | risk | sessions |
|---|---|---|---|
| 1 guard/accessor | ~6 small lemmas (reflexivity) | low | 0.5 |
| 2 dispatch (khd first) | 4 lemmas (mechanical stepping) | **med (khd is make-or-break)** | 1–1.5 |
| 3 call-free `sim` | 1 induction | low (given 2) | 0.5 |
| 4 call + `simF` | object-fuel induction | med–high | 1 |
| 5 cost/int-efficient | SintCost + IntEfficient | high | 1–1.5 |

**Start:** Phase 1 (all reflexivity, immediate green), then `disp_khd` (Phase 2) — if
that one lands, the remaining `disp_*` are the same shape and Phases 3–4 follow.

---

## Agda-side items (B/C) — first-move each (separate sessions)

- **B1 build-diagonal**: define `g = kifnil (run-sint on ⌜p⌝.⌜p⌝) knil (call var)`; first
  lemma = the two-case split on `⟦g⟧(⌜g⌝)`.
- **B2 Post**: `dovetail n` splitting fuel by parity; first = one direction of the iff.
- **B3 recursion thm**: a concrete `spec : Exp → V → Exp` (encoded-program returning).
- **C1 onevar**: port the translation; first = `var`/`knil` base semantics preservation.
- **C2 WHILE≡GOTO**: `data GOTO`; first = the `Compilation.Lang` instance.
