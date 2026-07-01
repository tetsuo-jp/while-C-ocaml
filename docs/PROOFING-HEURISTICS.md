# Proofing heuristics — decisive prompts for when a formal proof is stuck

A checklist of *decisive* moves (and copy-pasteable prompts) for breaking through
stuck Agda/formal proofs.  Distilled from what actually unblocked this repo's
hard proofs (esp. **G1**, `SelfInterpCorrect.simF` — the general self-interpreter
correctness theorem).  The seed was one prompt — **「小さな問題に分割して」** — and
what made it decisive was that it *separated the mechanically-checkable part from
the human reasoning*.  Everything below pulls a similar lever.

## The golden pattern (worked best on G1)

> **D → A → B**: name the exact obstacle in one sentence → push the routine part
> out as a hypothesis → brute-force the rest.

Concretely on G1: (D) "the inner self-interp of the *symbolic* `⌜e⌝` is stuck, so
monad-associativity is not definitional" → (A) isolate a per-constructor
*dispatch lemma* that holds by `refl` → (B) grid-search the fuel constants.

---

## A. Decompose — separate the essential from the routine

- **「証明義務を仮説として外に出し，後で discharge せよ」**
  *Push the routine construction out as an explicit hypothesis; prove only the
  core.* — Halting (5.3.1), Rice (5.4.2), the recursion theorem (14.2.1) all fell
  once `build-diagonal` / `Post` (dovetailing) / the self-selecting `q` were made
  hypotheses and only the diagonal/fixpoint was proved.
- **「refl で閉じる最小の補題に切り出せ。IH は rewrite で後から差し込め」**
  *Carve out the smallest lemma that closes by `refl`; splice the IH in later with
  `rewrite`.* — G1's per-constructor dispatch lemmas + `mono-≤` + `trans`.

## B. Let the tool tell you — observe instead of derive

- **「手で導出する前に，型/ゴール/エラーを読んで“形”を機械に教われ」**
  *Before hand-writing the proof, read the goal / normal form / unification error
  to learn the shape.* — the `call`-dispatch form (a 3-level nested bind) was
  found by reading the `UnequalTerms` error, not by derivation.
- **「未知の定数は計算せず，小さな有限空間を総当たりで合わせよ」**
  *Don't compute an unknown constant — brute-force it over a small finite space.*
  — the fuel constants `D,J` (khd 12/6 … call 18/8/11) came from a grid search;
  it absorbs off-by-a-few errors in the hand count.

## C. Weaken first, then strengthen

- **「まず証明できる弱い版（部分フラグメント/具体例）で緑にし，段階的に一般化せよ」**
  *Get a provable weaker version green first (a sub-fragment, or concrete
  instances), then generalise.* — call-free 6 constructors → add `call` → full G1.
  `UniversalInterp`: concrete `reverse` by `refl` → the ∀ theorem.
- **「まず具体インスタンスで成立を確認してから ∀ 定理を書け」**
  *Validate on a concrete instance before writing the ∀ theorem* — abstract
  `mix-eq` → a faithful witness → …

## D. Name the exact obstacle (the most underrated move)

- **「今，厳密に何が原因で閉じないか，1文で述べよ」**
  *State, in one sentence, exactly why it won't close.* — "monad-assoc is not
  definitional on the stuck inner self-interp" *immediately* revealed the right
  (3-level) form.
- **「これが自明になる補助補題は何か。それを先に作れ」**
  *What single lemma would make this trivial? Build it first.* — `mono-≤`,
  `ifnil-just` were built first, collapsing the main proof to one line.

## E. Attack from the other side

- **「逆・対偶・双対・還元で攻めよ」**
  *Invert / contrapositive / duality / reduction.* — the general halting problem
  via a reduction from self-halting (`ReductionUndecidable`), not directly.

## F. Autonomous-loop prompts

- **「autonomous に緑にできる所は全部やり，非 autonomous は明示して残せ」**
  *Solve everything autonomously green-able; explicitly mark what needs manual
  work.* — sorts grid/abstract-tractable work from hand-construction.
- **「N 手で緑にならなければ弱い版に切替え，知見を記録して報告せよ」**
  *Time-box; on failure, fall back to a weaker version and record the finding.* —
  prevents sunk-cost thrashing (a probe that fails is still a finding worth
  recording, e.g. "a constant fuel offset does NOT close the call case").

---

## The four meta-principles behind all of these

1. **Separate human reasoning from machine search** (A, B).
2. **Observe before you prove** (B, C) — normal forms and errors are more reliable
   than a hand derivation.
3. **Naming the obstacle constrains the solution** (D).
4. **Build the scaffold first** (C, D) — a foothold or a trivialising lemma.

## Repo-specific reflexes (Agda + the green-per-step loop)

- Transform the goal into a `refl`-closable shape; use `rewrite <lemma> | <ih>`.
- If fuel / an index won't line up, **grid-search** it (a shell loop over a small
  range, break on first `OK`).
- Keep every committed step green (`--safe`); commit only fully-proved statements
  (no holes) — a partial result becomes a narrower *complete* theorem + a scoped
  TODO, never a hole.
