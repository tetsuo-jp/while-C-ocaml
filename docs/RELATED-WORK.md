# Related work — existing research mapped to this repo

This file is the canonical bibliography for the partial-evaluation / Futamura side
of the project **and the Levin optimal-search / computability thread** (see the
Levin section below). It records, per existing result, **which component or open gap
of this repo it connects to** — so the paper (`PAPER-SKELETON.md`) can cite a single
place, and so the "classical vs novel" boundary stays explicit. Component/gap
labels are from `docs/FUTAMURA.md`, `docs/PAPER-SKELETON.md` (contributions
C1–C6), and `docs/AGDA_CORRESPONDENCE.md` (gaps G1–G3).

## Foundations (already classical; we instantiate, not claim)

- **Futamura 1971**, *Partial Evaluation of Computation Process — An Approach to a
  Compiler-Compiler.* The three projections. → all of C2 (`mix.while`,
  `mix_triv.while`, fp1/fp2/fp3).
- **Jones, Sestoft, Søndergaard 1985**, *An experiment in partial evaluation: the
  generation of a compiler generator* (RTA). The first self-applicable `mix`;
  source of the `mix`/`cogen` self-application equations. → C2, `spec.while`.
- **Jones, Gomard, Sestoft 1993**, *Partial Evaluation and Automatic Program
  Generation* (Prentice Hall) — "JGS". Offline PE, BTA, the flowchart `mix`
  (ch. 4) with **polyvariant program-point specialization** (pending-list + seen-set
  memoization of `(program-point, static-store)` configurations), and the
  definition of **Jones-optimality**. → BTA (`Bta.ml`), the offline `cogen`, and
  the open **item-3** (see below).

## item-3 — program-point memoization (control-bearing subjects) — DONE (M3a/b/c), machine-checked (M4)

`cogen --online` makes the *static-spine* self-interpreter `usi` loop-free on
straight-line subjects; **item-3 extends this to a *control-bearing* subject** with a
single-pass global-configuration-memo supercompiler driver (`Cogen.ml`, `COGEN_M3`),
so `usi`×rev compiles to the subject's own loop with zero interpreter dispatch
(`#while=1, #eq=0`). This is the classical polyvariant PP specialization in its online
form, and is now **implemented and machine-checked** (the one subject it does not reach
— the book's literal `universal` — is a binding-time wall, not a driver gap; see the
Jones-optimality section):

- **Glück 2012**, *A self-applicable online partial evaluator for recursive
  flowchart languages* (Software: Practice & Experience 42(6):649–673). **The
  closest existing work to our setting**: online × self-applicable × flowchart
  (= WHILE). **Recursive polyvariant specialization** lets an *online* PE produce
  generating extensions as efficient as offline ones — directly contradicting the
  assumption that online PE over-generalizes. → **realized in M3a**: the global memo
  `m3_seen : (Cd-position, static-store projection) ⇀ name`, each configuration
  specialized once, a re-reached config folding to its block. Its **memo transparency**
  (sharing configs ≡ non-memoized per-config specialization) is machine-checked
  (`MemoDriverSound.agda`).
- **JGS 1993, ch. 4** — the offline antecedent of the same algorithm (memoized PP
  specialization producing one residual block per reachable configuration). → the
  worklist `drive2` and the pc-dispatch fallback emit (`ITEM3-DESIGN.md` strategy (a)).
- **Sørensen, Glück, Jones 1996**, *A positive supercompiler* (JFP 6(6):811–838).
  Process-graph **folding**: a back-edge in the configuration graph becomes a loop
  in the residual. WHILE has no `goto`, so to emit a *loop-free-modulo-source*
  residual the memo cycle is re-expressed as the **subject's own** `while`/`if`. →
  **realized in M3b** (`m3_reconstruct_general`: dominators → back-edges → recursive
  region emit to structured WHILE). Its **reconstruction soundness** (the emitted
  `sH; while g {body; sH}; exit` equals the header flowchart loop, for partial bodies
  and exits, so nested loops compose) is machine-checked (`MemoReconstruct{,Gen}.agda`).
- **Glück 2012b**, *Bootstrapping Compiler Generators from Partial Evaluators*
  (PSI 2011, LNCS 7162). Relevant to fp3/`cogen` quality.

The concrete realization in `Cogen.ml` (configuration key, memo table, emit strategies,
milestones, tests) and the M4 machine-checking are in
[`ITEM3-DESIGN.md`](ITEM3-DESIGN.md); the proven-vs-tested map is
[`AGDA_CORRESPONDENCE.md`](AGDA_CORRESPONDENCE.md). This realized + verified driver is
contribution **C6** (`PAPER-SKELETON.md`).

## C5(i) — online termination (`BTAWiden` + the M3c homeomorphic-embedding whistle)

`Cogen.ml` bounds straight-line unrolling with `COGEN_UNROLL_BUDGET` plus a
generalization fallback (BTA-widening half machine-checked, `BTAWiden.agda`); the
*configuration* set of the global-memo driver is kept finite by a **homeomorphic-
embedding whistle** (M3c, `m3_specialize`):

- **Leuschel 2002**, *Homeomorphic Embedding for Online Termination of Symbolic
  Methods* (Neil Jones Festschrift, LNCS 2566:379–403). The homeomorphic-embedding
  **whistle**: generalize when the new configuration embeds an ancestor. The standard
  online-PE / supercompilation termination criterion. → **realized in M3c**: `emb` /
  `store_embeds` on the store projection (with `'D` as the embedding-bottom); a config
  that embeds an ancestor is generalized by dynamicizing the growing slots (an emitted
  fixup block materializes each, the proven `dyn1`/`dynList` step). *This is exactly
  where the whistle has teeth — over the program-point–indexed configurations `(pp,
  σ_s)`, not the flat store tuple (`ITEM3-DESIGN.md`).*
  - **Honest delta on the wqo.** Leuschel's HEm derives termination from the embedding
    being a **well-quasi-order** (Higman/Kruskal). That route is **inapplicable here**:
    our projection embedding tests `emb ('S u) ('S v) = (u ≡ v)` over the program's
    *unbounded* static values, and equality on an infinite set is not a wqo (a sequence
    of distinct `'S vᵢ` has no embedding pair). So we do **not** prove a wqo; instead we
    machine-check the implementation's **monotone-bounded** termination argument
    (`WhistleTerminates.agda`): a generalization dynamicizes ≥1 slot, a dynamicized slot
    is `'D` = terminal (cannot re-embed/grow), so the generalization count is bounded by
    the slot count. This closes the ad-hoc half of **G2** for the actual strategy, and
    documents precisely where the classical wqo criterion does and does not transfer.
- **Leuschel et al. 2008/09**, *Type-based Homeomorphic Embedding for Online
  Termination* (LOPSTR 2007, LNCS 4915; IPL 2009). TbHEm handles **infinite
  signatures** — the same problem our C-domain **widening** (`bt_maxdepth`,
  infinite ascending chains) solves by depth-capping, and the same unbounded-static-
  value issue the monotone-bounded argument sidesteps above. Re-presenting `widen` as a
  TbHEm instance grounds C5(i) in the literature.

## C5(ii) — partially-static store (our "spine-preserving emit")

Our spine-preserving emit (an emitted slot keeps the static cons spine with
dynamic leaves replaced by self-references; `SpinePreservingEmit.agda`) is the
WHILE-store realization of a known technique:

- **Mogensen 1988**, *Partially static structures in a self-applicable partial
  evaluator* (Partial Evaluation and Mixed Computation, IFIP TC2 Workshop Gammel
  Avernæs 1987, North-Holland, pp. 325–347). Partially-static data structures in
  `mix`. → the technique behind C5(ii). **Our delta:** a *machine-checked*
  (`--safe` Agda) realization for the WHILE variable store, with the explicit
  soundness side-condition (`cross-unsound`: self-references are sound, cross-
  references are not) — which is what makes the store-walk loops in `usi` unroll.
  This is the classical basis the paper should cite so C5(ii) reads as a
  *verified instantiation*, not a reinvention.

## Jones-optimality framing (the store-index "tag" problem)

The reason `universal` is not loop-free — the store **index** (inner program
pointer `Cd`) going dynamic — is the WHILE-store form of the well-studied **tag**
problem of Jones-optimal specialization:

- **Makholm 2000/2001**, *Tag Elimination and Jones-Optimality* (PADO-II 2001,
  LNCS 2053:257–275). Removing interpretive tag/untag is exactly what makes
  self-interpreter specialization optimal. Our store-index dynamicity is the tag
  to eliminate; `usi` + spine-preserving emit eliminate it for the straight-line
  case.
- **Danvy & López 2003**, *Tagging, Encoding, and Jones Optimality* (ESOP 2003,
  LNCS 2618). The encoding view of the same.
- **Brown & Palsberg 2018**, *Jones-Optimal Partial Evaluation by Specialization-
  Safe Normalization* (POPL 2018, PACMPL 2(POPL):14). A modern Jones-optimality
  construction; reference point for the optimality target.
- *An investigation of Jones optimality and BTI-universal specializers.* Uses the
  **BTI** (binding-time improvement) framing that matches this repo's documented
  "store-index BTI wall" (`docs/FUTAMURA.md`, `rwhile-C-ocaml/analysis_store_bti.md`);
  positions our independent rediscovery of that wall. **Confirmed empirically by M3**:
  the book's `universal` (`case Cd, St of`) does not converge under the global-memo
  driver because its dispatch conjoins the static code test with a *dynamic* value-stack
  test, so the configuration set diverges (measured) — the tag/BTI wall, not a memo gap.
  `usi` (`case Cd of`, dynamic stack read inside arms) is exactly the BTI that removes
  the tag; a mechanical *guard-only* BTI (`transCaseBTI`, irrefutable secondary patterns)
  is semantics-preserving but **necessary-yet-insufficient** — convergence also needs
  `usi`'s value-stack threading (`docs/ITEM3-DESIGN.md`, reverted experiment). So `usi`
  is the *whole* BTI, and "BTI-universal specialization" is the open route to making the
  literal `universal` optimal without the hand reformulation.

## G1 — end-to-end specializer correctness — CLOSED (+ M4 driver verification)

G1 (`[cogen(p,s)](d) = [p]((s,d))`) needed no new external research: the sibling
`rwhile-C-ocaml`'s `RWhileAVSound`/`RWhileAVSpec` (a congruence proof for its
reversible AV specializer) was the template, ported here to the classical AV
specializer. It is now **closed for the full WHILE language** — `skip`/`asgn`/`semi`/
`if`/`while` — built bottom-up (`AVExprSound` → `SpecExprStore` → `SpecCmdStore` →
`SpecAssembly{,If,While}`, all `--safe`). On top of G1, **M4** machine-checks the
global-memo driver itself (reconstruction `MemoReconstruct{,Gen}`, termination
`WhistleTerminates`, memo transparency `MemoDriverSound`). Together G1 + M4 are the
verification backing contribution **C6** (the end-to-end machine-checked specializer);
the proven-vs-tested boundary is in `docs/AGDA_CORRESPONDENCE.md`.

## Reversible side (C1, Theorem A) — see the other docs

The reversible information lower bound (C1) and its Bennett–Landauer counting are
covered in `docs/PAPER-SKELETON.md` §(b)/§(c) and `proofs/agda/RevSpecGarbage.agda`,
with the measured upper bound in `rwhile-C-ocaml` (`FINDINGS §6`,
`RWhileRevProjGen`). Not repeated here.

## Levin optimal search — the first mechanization (classical Levin track)

The classical Levin optimal-search result (the `proofs/agda` Levin chain +
`proofs/rocq`, double-verified, axiom-free) is, to our literature search (Semantic
Scholar / Crossref / arXiv, 2026-07; see `memory/levin-formalization-novelty.md`),
the **first machine-checked proof of Levin's optimal search theorem** (Jones's
*optimal program search*, Thm 20.1.2). No prior formalization of Levin search /
optimal program search was found; the nearest mechanized neighbours are two
**Kolmogorov-complexity** formalizations. We position our result against them **by
mirroring their own line of argument** (constructiveness / faithful mechanization of
a classically-argued result):

- **Forster, Kunze, Lauermann 2022**, *Synthetic Kolmogorov Complexity in Coq*
  (ITP 2022, LIPIcs vol. 237, 12:1–12:19, doi:10.4230/LIPIcs.ITP.2022.12).
  Mechanizes Kolmogorov complexity **synthetically** — rather than fixing a model of
  computation it *abstractly assumes a universal function*, and stresses
  **constructiveness** (isolate the one classical step as Markov's principle, then
  eliminate it). → **Our complementary delta.** We take the *opposite, deliberately
  concrete* route: we instantiate Jones's **actual WHILE self-interpreter** and
  **prove the interpretive-overhead constant** (`a = 73`, `IntEfficient`) that a
  synthetic treatment abstracts away — the computational counterpart to their assumed
  universal function. Their "constructiveness" hook is matched by our
  **axiom-freedom** (`--safe`, 0 postulates; Rocq `Print Assumptions` closed) and
  *doubled* across Agda **and** Rocq.
- **Catt, Norrish 2021**, *On the formalisation of Kolmogorov complexity* (CPP 2021,
  pp. 291–299, doi:10.1145/3437992.3439921; HOL4). A concrete-model KC formalization
  (invariance theorem). → Same delta: it formalizes complexity, not optimal *search*,
  in a single system. It also establishes the **precedent**: a faithful mechanization
  of a Kolmogorov/Levin-adjacent classical result is a recognized contribution at the
  top formalization venues (CPP/ITP) — which our Levin mechanization follows.

**The argument, in their idiom.** Textbook Levin/KC results carry an informal core.
For us it is concrete: the interpreter-overhead constant (the `a` in "optimal within
`a·(T_q+T_r)`") is *asserted to exist* but never computed, and the dovetail
schedule's optimality is argued by hand. Exactly as Forster et al. render the
classically-argued KC results *constructive and machine-checked*, we render Jones's
optimal-search proof **axiom-free and machine-checked, and turn its hand-waved
constant into a theorem** (`a = 73`) — while a second, independent **Rocq**
development rules out a single-prover artifact. Novelty type: *first mechanization*
(the theorem's mathematics is Levin's/Jones's); the contribution is the faithful
concrete formalization + the constant-as-theorem + double verification. The
**reversible Levin** extension (`RevLevinMain`, novel result) is the forward-looking
companion; see `docs/REV-LEVIN-SKELETON.md`.

## Partial-evaluation / Futamura *formalization* — methodological neighbours

Formalizations and mechanized experiments of the Futamura projections / PE are a
separate line, cited for the repo's PE side (C2–C6):

- **Latifi 2019**, *Practical second Futamura projection: partial evaluation for
  high-performance language interpreters* (SPLASH Companion / SRC 2019, pp. 29–31,
  doi:10.1145/3359061.3361077). The second projection realized in a production
  interpreter (Truffle/GraalVM) — the practical counterpart to our verified `fp2`.
- **Glück & Normann 2024**, *Inversion by Partial Evaluation: A Reversible
  Interpreter Experiment* (EPTCS vol. 413, pp. 1–14, doi:10.4204/EPTCS.413.1).
  **Closest on the reversible side**: PE of/for a reversible interpreter and program
  inversion — the neighbour of this repo's reversible PE (C1, Theorem A) and the
  reversible-Levin self-interpreter `rsint` (`RevRsint`).
