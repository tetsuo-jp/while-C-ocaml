# comp2book2007 machine-checking — wave 1 done, and the next targets

2026-07-18. Solo/inline session (the parallel prover fleet was killed mid-run by
a monthly spend limit; everything below was written and `agda --safe`-verified by
hand). See `BOOK-ROADMAP.md` for the full map.

## Done this wave (both `--safe`, axiom-free)

- **Thm 5.5.2 (semi-decidable closure), `SemiDecClosure.agda`.** Fills the gap the
  `DecidableClosure` header pointed at. `sdec-∩` is FULLY constructive: `kcons e₁ e₂`
  runs both on the same input and halts iff both halt (`haltsE-kcons`, from `evalT`'s
  `kcons` clause + `StepCountMono`). `sdec-∪` is proved relative to a `Dovetail`
  record (the parallel-or combinator) — same discipline as `SemiDecidable`'s `Post`.
- **`EvalTFuelTime.agda` (fuel-time bridge).** `fuel-time`: the reported step
  count is sufficient fuel (`evalT n e x = just (v,t) ⇒ evalT t e x = just (v,t)`,
  since depth ≤ size); corollary `time≤-fuel`.
- **`BoundedTime.agda` (bounded-time decidable).** `evalT-det` (reported result is
  fuel-independent) and `TimeLE?` : "e halts on x with true time ≤ B" is DECIDABLE
  by one bounded run — the exact ingredient to collect a program list's halting
  times ≤ B into `GapTheorem.gap`.
- **Thm 13.4.3 (Kleene Normal Form), `KleeneNF.agda`.** `T-dec` (decidable bounded
  halting via `evalT`), `U = proj₁`, the new converse bridge `eval⇒evalT`, and the
  equivalence `knf`.

## Next targets, in tractability order

1. **Thm 20.4.1 (Borodin Gap theorem) — DONE (for programs).** `GapTheorem.agda`
   (`gap`), `EvalTFuelTime.agda` (`fuel-time`), `BoundedTime.agda` (`TimeLE?`),
   `GapPrograms.agda` (`gap-prog`). Fully assembled; nothing left for the
   per-input program form. (Historical notes below.) The fuel/decidability layer
   (`EvalTFuelTime`, `BoundedTime`). Left: `maxGap k = ⊔_{j≤k} g (rung j)` with
   `j ≤ k ⇒ g (rung j) ≤ maxGap k`; collect `hs` = the `TimeLE? · x · B`-halting
   times of `ps` (B = maxGap (length ps)) with a membership lemma; then `gap hs`
   finishes (a program with time > B is excluded since `g (rung (toℕ j)) ≤ B`).
   Original subtlety, now resolved in `EvalTFuelTime` —
   in `evalT`, fuel = recursion DEPTH but the reported count `t` = total nodes
   (tree SIZE), and depth ≤ size, so the true time `t` satisfies
   `evalT n e d = just (v,t) ⇒ evalT t e d = just (v,t)` (fuel = reported time
   suffices; prove `fuel-time` by induction like `KleeneNF.eval⇒evalT`). Hence
   "e halts on d with time ≤ B" ⟺ `evalT B e d = just (v,t') ∧ t' ≤ B`
   (DECIDABLE). Then: `hs = mapMaybe (halting time ≤ B) ps` with
   `B = maxGap (length ps)` (max of `g (rung j)`, `j ≤ length ps`; needs no
   monotonicity of g); a program with time > B can't be in any interval below B;
   `gap hs` finishes. Original plan retained below for reference:
   - Ladder `rung g 0 = 0`, `rung g (suc j) = suc (g (rung g j))`; with `g t ≥ t`
     the rungs strictly increase and the intervals `Iⱼ = (rung j , g (rung j)]` are
     pairwise DISJOINT (`rung (suc j) = suc (g (rung j)) > g (rung j) = top of Iⱼ`).
   - Halting times of the first `k` programs on input `d` form a list `hs : List ℕ`
     (via `KleeneNF.T-dec` / bounded search; `length hs ≤ k`).
   - PIGEONHOLE: `k` points meet `≤ k` of the `k+1` intervals `I₀..I_k`, so some
     `Iⱼ*` is empty → `rung j*` is a gap: no program among the first `k` halts in
     `(rung j*, g (rung j*)]`.
   - The Agda-fiddly step is the pigeonhole. Clean induction on `hs` with the
     invariant "for a list of pairwise-disjoint intervals `I` with `|I| > length hs`,
     some interval is free of all of `hs`": base picks `head I`; step removes the
     `≤ 1` interval hit by the head element (disjointness ⇒ at most one), keeping
     `|I'| > length hs'`, and recurses. `single-interval` lemma:
     `inGap g (rung i) h → inGap g (rung j) h → i ≡ j`.
   - Deliver an abstract version first (`htime : Fin k → Maybe ℕ` interface), then
     instantiate `htime` to `evalT`-halting.
2. **Prop 8.2.1 / 8.2.2 (WHILE ≡ GOTO, Böhm–Jacopini)** — `Compilation._⟶_` is the
   framework; `WhileMachine`'s control-stack small-step machine is most of the GOTO
   side already. Build a numbered-instruction GOTO language + pc-variable simulation
   for one direction.
3. **Prop 3.7.4 (WHILE → I correctness, general/nested)** — bricks B3a/B3b/B3b+/B4
   are done (`docs/ROBUSTNESS-PLAN.md`); remaining is the full compiler via the B4
   normal form + the `runW ~ runM` simulation.

Larger/research-scale (Blum speedup 20.3.1, constant hierarchy 19.3.1, 2CM 8.7.2):
partials only; see the fleet prompt in
`.../workflows/scripts/comp2book-prove-fleet-*.js` for per-target decompositions.

## Update (session 2026-07-18/19): Gap done + more

New verified modules this session (all `--safe`, axiom-free):
- `GapPrograms.agda` — **Borodin Gap theorem 20.4.1 DONE for programs** (`gap-prog`).
- `EvalTFuelTime.agda`, `BoundedTime.agda` — the fuel/decidability layer.
- `REChar.agda` — Thm 5.7.2 r.e. characterizations, dovetail-free directions
  (`char-dom` (2)⇔(4), `char-range` (2)⇒(3)).

**The dovetail combinator is now the single highest-leverage next target.** It
blocks: `SemiDecClosure.sdec-∪` (union closure), a concrete `SemiDecidable.Post`
(Post's theorem), and Thm 5.7.2's (1)⇒(2)/(3)⇒(1). Design: a `call`-recursion in
the I language that, on a fuel-encoding input, runs `e₁` and `e₂` at increasing
fuel until one halts; correctness against `evalT` is the hard part (a search-loop
invariant). Building it once closes several theorems at their "full" strength.
