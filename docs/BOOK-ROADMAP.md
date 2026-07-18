# Book roadmap: Jones, *Computability and Complexity* (comp2book2007) → this repo

A map from the WHILE-language results of Neil Jones's textbook
(*Computability and Complexity: From a Programming Perspective*, 2007 free
edition; LaTeX source in `~/dev/overleaf/comp2book2007`) to what is — or can be —
machine-checked here (`while-C-ocaml` + `proofs/agda/`).

Book results are cited by their `chapter.section.n` number and `\label{...}` key.
Status legend: **✓ done** (formalized/verified here) · **◎ next** (high-value,
reachable now from existing assets) · **○ medium** (write a model + correctness) ·
**△ large** (complexity/hierarchy, research-scale).

The arc to date: Def 3.2.1 (⌜p⌝), Thm 4.2.1 (self-interpreter), and Thm 5.2.1
(s-m-n) are the pillars already standing — and from them the classic
computability theorems (halting, recursion theorem, Rice) become reachable.

---

## A. Done — formalized or runnable here

| Book (number · label) | Statement | Repo asset |
|---|---|---|
| Def 2.2.4 `def-while-semantics` | WHILE semantics `⟦p⟧ : D → D⊥` | `src/core/EvalWhile.ml` (differential); Agda `UniversalInterp.eval` (= the I language) |
| Def 3.2.1 `def:concrete` | programs-as-data `⌜p⌝ ∈ D` | `proofs/agda/ProgEncoding.agda` (faithful `decode ⌜e⌝ ≡ just e`); `DesugarWhile --data` |
| Def 3.7.1 / Prop 3.7.4 `prop:many-one-var` | the **I language** (one-variable WHILE); WHILE → I | `--onevar`; `proofs/agda/OneVarTuring.agda` (one variable = ℕ-many independent registers); `docs/I-LANGUAGE.md` |
| **Thm 4.2.1** `thm-I-universal` | a **self-interpreter `i` for I** | `proofs/agda/SelfInterp.agda` (full self-interpreter `sint`, every constructor + recursion, verified on concrete programs); `SelfInterpRun.agda`; `examples/desugar/universal.while` |
| **Thm 5.2.1** `thm-s-1-1` | **Kleene s-m-n**: `⟦⟦spec⟧(p.s)⟧(d) = ⟦p⟧(s.d)` | meta-theorem stack: `FutamuraProjections.mix-eq` (abstract) + `FutamuraSmn.spec-correct` / `MixSpec.specE-sound` (concrete) |
| Futamura projections (ch.6 §`sec-futamura`, *displayed equations, not numbered*) | mix / cogen / the three projections | the whole stack (`FutamuraProjections`, `FutamuraSmn`, `FutamuraPartial`, `MixQuote`, `MixSpec`, `GenextSound`) — **note: the book states these as equations, so this repo's machine-checking is *more* formal than the book** |
| Def 6.1.1 / Def 19.1.1 / Prop 19.1.2 / Thm 19.1.3 / Def 19.2.1–Constr 19.2.3 / Lem 19.2.4 | timed language `time_p(d)`; efficient interpreter; **timed universal `tu`** for I | `--time`; `examples/desugar/timed-universal.while`; `docs/EFFICIENCY.md`; `examples/desugar/test-efficient-tu.sh` (runnable) |
| Fig. ch.19 `fig-diagonal-program` (`diag`) | the diagonalization program | `examples/desugar/diag.while` (**runnable demo, not a proof**) |

---

## B. Next — reachable now from the self-interpreter + s-m-n (high value)

These four form one coherent arc; each builds on the row above.

| Book | Theorem | Proof route from existing assets | Size |
|---|---|---|---|
| **Thm 5.3.1** `thm-halt-undecidable` (+ Prop 1.4.4 `prop-halting-problem`) | **the halting problem for WHILE is undecidable** | **✓ DONE** — `proofs/agda/HaltingUndecidable.agda` (abstract + constructive diagonal; concrete WHILE/I instance documented) | ✓ |
| **Thm 14.2.1** `thm-kleenes-2-recursion` | **Kleene's 2nd recursion theorem**: `∃q. ⟦q⟧(d) = ⟦p⟧(⌜q⌝.d)` | **✓ DONE** — `proofs/agda/RecursionTheorem.agda` (fixpoint `q = spec e ⌜e⌝` from our s-m-n) | ✓ |
| **Thm 5.4.2** `thm-rice` | **Rice's theorem**: every non-trivial extensional property is undecidable | **✓ DONE** — `proofs/agda/RiceTheorem.agda` (constructive recursion-theorem reduction) | ✓ |
| **Thm 5.6.1 / Cor 5.6.2** `thm-halt-semi` / `cor-halt-not-semidecidable` | HALT is semi-decidable; its complement is not | **✓ DONE** — `proofs/agda/SemiDecidable.agda` (Post's lemma + undecidability ⇒ `∁`self-halting not semi-decidable) | ✓ |
| **Thm 5.5.1 / 5.5.2** | closure of decidable / semi-decidable sets | **✓ DONE** — decidable part `proofs/agda/DecidableClosure.agda` (`∩`/`∪`/complement); semi-decidable `∩` fully + `∪` modulo a dovetail combinator `proofs/agda/SemiDecClosure.agda` (`sdec-∩` via `kcons`, `sdec-∪` via `Dovetail`) | ✓ |
| general **Thm 4.2.1** (∀-version) | `⟦sint⟧(⌜p⌝.d) = ⟦p⟧(d)` for *all* `p` | currently concrete-only; the pending "G4 4c general theorem" — a fuel-relative induction over `sint`'s behaviour on arbitrary ⌜p⌝ | ◎ medium–large |

---

## C. Medium — model + correctness

| Book | Statement | Size |
|---|---|---|
| Prop 3.7.4 `prop:many-one-var` | **prove** the WHILE → I (`--onevar`) translation correct (currently differential-tested) | ○ medium |
| Prop 8.2.1 / 8.2.2 `prop-wtog` / `prop-gtow` | **WHILE ≡ GOTO** (Böhm–Jacopini: one while-loop suffices) | ○ medium. **Framework done** — `proofs/agda/Compilation.agda` (the compilation preorder `_⟶_` + equivalence `_≃_`); remaining = the concrete GOTO↔WHILE compilers |
| **Thm 8.7.2 / Cor 8.7.3** `thm-2CM-universal` / `cor-2CM-universal` | **two-counter-machine universality** + 2CM halting undecidable | ○ medium–large. Cor 8.7.3's logical form = `ReductionUndecidable`; the framework is `Compilation`; remaining = the concrete 2-counter Gödel-coding simulation (the canonical single-theorem Turing-completeness witness) |
| Thm 13.4.3 `thm-normal-form` | Kleene Normal Form for WHILE | **✓ DONE** — `proofs/agda/KleeneNF.agda` (`T-dec` decidable bounded halting, `U`=value extraction, `eval⇒evalT` converse bridge, and the normal-form equivalence `knf`) |

---

## D. Large — complexity / hierarchy (ingredients exist; full proofs are research-scale)

| Book | Statement | Notes |
|---|---|---|
| **Thm 19.3.1** `thm-constant-hierarchy` | **constant time factors matter for I**: a linear-time hierarchy via `diag` (b = 249) | `diag.while` + `tu` are the ingredients; the set-theoretic complexity argument is the work |
| Thm 19.5.3 / 19.5.4 | time-constructible / asymptotic time hierarchy | △ large |
| Thm 18.3.3 `thm-goto-equiv-while` | WHILE ≡ I ≡ GOTO ≡ F up to linear-time | △ large (constant-factor simulations) |
| Thm 18.4.2 `thm-tm-speedup` | TM constant-factor speedup | △ large |
| Thm 20.1.2 / 20.3.1 / 20.4.1 | Levin / Blum speedup / Gap theorems | △ research-scale. **Gap theorem (20.4.1) DONE (for programs)** — `GapTheorem.agda` (`gap`: ladder + disjointness + `at-most-one` + pigeonhole), `EvalTFuelTime.agda` (`fuel-time`), `BoundedTime.agda` (`TimeLE?` decidable bounded-time), `GapPrograms.agda` (`gap-prog`: no program in `ps` halts on `x` in `(rung j, g(rung j)]`). **Levin's budget-doubling CORE is done** — `proofs/agda/LevinBudget.agda` (`gsum<2last`: the dovetailed search over phases `0..k` costs `< 2·2^k`, the quantitative heart of the constant `a_q`). Remaining for full 20.1.2: the overshoot bound, the efficient interpreter's cost theorem (Thm 19.1.3 — the I self-interpreter `SelfInterp` is the intended efficient interpreter), program enumeration, and the assembly |

---

## Recommended order

1. **Thm 5.3.1 (halting undecidable)** — best cost/benefit: the self-interpreter is in place, so this is a diagonalization in Agda.
2. **Thm 14.2.1 (Kleene recursion theorem)** — falls straight out of our s-m-n.
3. **Thm 5.4.2 (Rice)** — corollary of 5.3.1 + s-m-n.
4. then either the **general Thm 4.2.1** (close the G4 4c general theorem) or **Thm 8.7.2** (2CM universality — a clean Turing-completeness capstone).

(Orphan files in `comp2book2007` not part of the 2007 edition — `gap-and-speedup-thms.tex`, `linspeedup`, `TMchapter9.tex`, `hierarchy-nond/space.tex` — are excluded from `book-whole.tex`'s `\includeonly`; the live Blum/Gap material is ch.20.)
