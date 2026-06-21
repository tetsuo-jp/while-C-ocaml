# Related work — existing research mapped to this repo

This file is the canonical bibliography for the partial-evaluation / Futamura side
of the project. It records, per existing result, **which component or open gap of
this repo it connects to** — so the paper (`PAPER-SKELETON.md`) can cite a single
place, and so the "classical vs novel" boundary stays explicit. Component/gap
labels are from `docs/FUTAMURA.md`, `docs/PAPER-SKELETON.md` (contributions
C1–C5), and `docs/AGDA_CORRESPONDENCE.md` (gaps G1–G3).

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

## item-3 — program-point memoization (loop-free `universal` / control-bearing subjects)

The headline open gap (`docs/FUTAMURA.md` "Item 2/3"): `cogen --online` makes the
*static-spine* self-interpreter `usi` loop-free on straight-line subjects, but the
book's `universal` and control-bearing subjects still residualize an interpreter
loop. The fix is the classical polyvariant PP specialization, in its online form:

- **Glück 2012**, *A self-applicable online partial evaluator for recursive
  flowchart languages* (Software: Practice & Experience 42(6):649–673). **The
  closest existing work to our setting**: online × self-applicable × flowchart
  (= WHILE). Shows that **recursive polyvariant specialization** lets an *online*
  PE produce generating extensions as efficient as offline ones — directly
  contradicting the assumption that online PE over-generalizes. → the design for
  **item-3** (a global memo table over `(Cd-position, static-store projection)`,
  each configuration residualized once) and a likely route to shrinking the large
  fp2/fp3 residuals (`comp2` 22.6 MB, `cogen` 29.6 MB).
- **JGS 1993, ch. 4** — the offline antecedent of the same algorithm (memoized PP
  specialization producing one residual block per reachable configuration).
- **Sørensen, Glück, Jones 1996**, *A positive supercompiler* (JFP 6(6):811–838).
  Process-graph **folding**: a back-edge in the configuration graph becomes a loop
  in the residual. WHILE has no `goto`, so to emit a *loop-free-modulo-source*
  residual the memo cycle must be re-expressed as the **subject's own** `while`/`if`
  — this is the folding step item-3 needs for Jones-optimal *structured* output
  (vs. a residual dispatch loop, which is not loop-free).
- **Glück 2012b**, *Bootstrapping Compiler Generators from Partial Evaluators*
  (PSI 2011, LNCS 7162). Relevant to fp3/`cogen` quality once item-3 lands.

## C5(i) — online termination (currently `BTAWiden` + ad-hoc unroll budget)

`Cogen.ml` bounds unrolling with `COGEN_UNROLL_BUDGET` (a constant) plus a
generalization fallback; only the BTA-widening half is machine-checked
(`BTAWiden.agda`). The principled, citable replacement:

- **Leuschel 2002**, *Homeomorphic Embedding for Online Termination of Symbolic
  Methods* (Neil Jones Festschrift, LNCS 2566:379–403). The homeomorphic-embedding
  **whistle** (a well-quasi-order): generalize when the new configuration embeds a
  previous one. Stronger than well-founded orders, the standard online-PE / super-
  compilation termination criterion. → replace the unroll budget; closes the
  ad-hoc half of **G2**.
- **Leuschel et al. 2008/09**, *Type-based Homeomorphic Embedding for Online
  Termination* (LOPSTR 2007, LNCS 4915; IPL 2009). TbHEm handles **infinite
  signatures** — the same problem our C-domain **widening** (`bt_maxdepth`,
  infinite ascending chains) solves by depth-capping. Re-presenting `widen` as a
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
  positions our independent rediscovery of that wall.

## G1 — end-to-end specializer correctness (in-house template, no external dep)

Closing G1 (`[cogen --online(p,s)](d) = [p]((s,d))` for the actual algorithm) does
not need new external research: the sibling `rwhile-C-ocaml`'s
`RWhileAVSound`/`RWhileAVSpec` (a congruence proof for its reversible AV
specializer) is the template; G1 ports it to the classical AV specializer here.

## Reversible side (C1, Theorem A) — see the other docs

The reversible information lower bound (C1) and its Bennett–Landauer counting are
covered in `docs/PAPER-SKELETON.md` §(b)/§(c) and `proofs/agda/RevSpecGarbage.agda`,
with the measured upper bound in `rwhile-C-ocaml` (`FINDINGS §6`,
`RWhileRevProjGen`). Not repeated here.
