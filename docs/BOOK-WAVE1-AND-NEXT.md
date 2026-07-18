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
- **Thm 13.4.3 (Kleene Normal Form), `KleeneNF.agda`.** `T-dec` (decidable bounded
  halting via `evalT`), `U = proj₁`, the new converse bridge `eval⇒evalT`, and the
  equivalence `knf`.

## Next targets, in tractability order

1. **Thm 20.4.1 (Borodin Gap theorem)** — constructive per-input core. Proof plan
   (the piece to mechanize):
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
