{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R1a'-guard: the GENERAL DATA-DEPENDENT reversible loop
--
--     from e do D loop L until f
--
-- and its inversion soundness -- the substantial reversible-CONTROL proof the
-- skeleton flagged as the missing prerequisite for R3c step 2 (an `RCmd` program
-- has no recursion; a reversible self-interpreter needs a reversible loop).
--
-- Operational reading (mirrors src/EvalRwhile.ml / src/InvRwhile.ml of the
-- R-WHILE implementation, and this project's rwhile-side Agda `RWhileRevFull`):
--   eval: assert entry test e TRUE; run D; then repeat:
--           if exit test f holds -> stop;
--           else run L, assert e FALSE (re-entry), run D, repeat.
--   inv : from f do (inv D) loop (inv L) until e    (entry/exit tests swapped).
--
-- The loop is the CRUX of reversibility: inverting it reverses the WHOLE
-- iteration chain, done by the accumulator induction `rev-rest` (the
-- assertion-inversion invariant over the loop trace).  Store-generic: any `S`
-- with `S → Bool` guards and reversible relational atoms.  `Examples` gives a
-- genuine TWO-TRIP data-dependent loop (ℕ×ℕ move-counter); `VLoop` instantiates
-- the atoms with the ENCODABLE language `RevInst.RCmd` (via `RevInst.fwd`),
-- connecting the reversible control to the self-interpreter's object language.
------------------------------------------------------------------------

module RevLoop where

open import Data.Bool using (Bool; true; false)
open import Data.Product using (Σ-syntax; _×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong₂; subst)

module Core (S : Set) where

  Rel : Set₁
  Rel = S → S → Set

  conv : Rel → Rel
  conv A = λ s t → A t s

  ------------------------------------------------------------------------
  -- Syntax

  data Cmd : Set₁ where
    atom : Rel → Cmd
    _⨾_  : Cmd → Cmd → Cmd
    cond : (S → Bool) → Cmd → Cmd → (S → Bool) → Cmd
    loop : (S → Bool) → Cmd → Cmd → (S → Bool) → Cmd   -- from e do D loop L until f

  infixr 5 _⨾_

  ------------------------------------------------------------------------
  -- Syntactic inversion (mirrors src/InvRwhile.ml)

  inv : Cmd → Cmd
  inv (atom A)       = atom (conv A)
  inv (c ⨾ d)        = inv d ⨾ inv c
  inv (cond e c d f) = cond f (inv c) (inv d) e
  inv (loop e D L f) = loop f (inv D) (inv L) e

  ------------------------------------------------------------------------
  -- Big-step semantics.  The loop uses an auxiliary "rest" relation:
  --   Rest e D L f w x : starting just after a D, at store w, iterate the
  --   "until f" loop, ending in store x.

  infix 3 _⊢_⇒_

  data _⊢_⇒_ : Cmd → S → S → Set₁
  data Rest (e : S → Bool) (D L : Cmd) (f : S → Bool) : S → S → Set₁

  data _⊢_⇒_ where
    e-atom : ∀ {A s t} → A s t → atom A ⊢ s ⇒ t
    e-seq  : ∀ {c d s t u} → c ⊢ s ⇒ t → d ⊢ t ⇒ u → (c ⨾ d) ⊢ s ⇒ u
    e-then : ∀ {e c d f s t}
           → e s ≡ true  → c ⊢ s ⇒ t → f t ≡ true  → cond e c d f ⊢ s ⇒ t
    e-else : ∀ {e c d f s t}
           → e s ≡ false → d ⊢ s ⇒ t → f t ≡ false → cond e c d f ⊢ s ⇒ t
    e-loop : ∀ {e D L f s t u}
           → e s ≡ true → D ⊢ s ⇒ t → Rest e D L f t u → loop e D L f ⊢ s ⇒ u

  data Rest e D L f where
    -- exit: the until-test f holds, stop here
    r-exit : ∀ {w} → f w ≡ true → Rest e D L f w w
    -- iterate: f fails, run L, the entry test e must be FALSE, run D, recurse
    r-iter : ∀ {w u v x}
           → f w ≡ false → L ⊢ w ⇒ u → e u ≡ false → D ⊢ u ⇒ v
           → Rest e D L f v x → Rest e D L f w x

  ------------------------------------------------------------------------
  -- MAIN THEOREM (with loop): inversion is sound.
  -- inv-sound and the chain-reversal rev-rest are mutually recursive.

  inv-sound : ∀ {c s t} → c ⊢ s ⇒ t → inv c ⊢ t ⇒ s

  -- rev-rest reverses a forward iteration chain into the inverse loop's rest,
  -- accumulating the already-built inverse rest (from q back to s).
  rev-rest : ∀ {e D L f s t u}
           → Rest e D L f t u
           → ∀ {q} → inv D ⊢ t ⇒ q → Rest f (inv D) (inv L) e q s
           → Σ[ m ∈ S ] (f u ≡ true) × (inv D ⊢ u ⇒ m) × Rest f (inv D) (inv L) e m s

  inv-sound (e-atom a)        = e-atom a
  inv-sound (e-seq cs ds)     = e-seq (inv-sound ds) (inv-sound cs)
  inv-sound (e-then es cs ft) = e-then ft (inv-sound cs) es
  inv-sound (e-else es ds ft) = e-else ft (inv-sound ds) es
  inv-sound (e-loop {e} {D} {L} {f} {s} es Dst rest)
    with rev-rest rest (inv-sound Dst) (r-exit es)
  ... | (m , fu , invDum , revrest) = e-loop fu invDum revrest

  -- base: forward rest exits at w=u; the accumulator IS the inverse rest to s.
  rev-rest (r-exit fw) {q} invDtq acc = (q , fw , invDtq , acc)
  -- step: prepend this iteration (reversed) to the accumulator, then recurse.
  rev-rest (r-iter fw Lwu eu Duv rest1) invDtq acc =
    rev-rest rest1 (inv-sound Duv)
      (r-iter eu (inv-sound Lwu) fw invDtq acc)

  ------------------------------------------------------------------------
  -- inv is a syntactic involution, giving the reverse direction too.

  inv-inv : ∀ c → inv (inv c) ≡ c
  inv-inv (atom A)       = refl
  inv-inv (c ⨾ d)        = cong₂ _⨾_ (inv-inv c) (inv-inv d)
  inv-inv (cond e c d f) = cong₂ (λ x y → cond e x y f) (inv-inv c) (inv-inv d)
  inv-inv (loop e D L f) = cong₂ (λ x y → loop e x y f) (inv-inv D) (inv-inv L)

  inv-complete : ∀ {c s t} → inv c ⊢ t ⇒ s → c ⊢ s ⇒ t
  inv-complete {c} {s} {t} d = subst (λ x → x ⊢ s ⇒ t) (inv-inv c) (inv-sound d)

------------------------------------------------------------------------
-- Test-first: a GENUINE data-dependent reversible loop over S = ℕ × ℕ.
-- State (remaining , done); D moves one unit remaining→done; the loop empties
-- `remaining`.  Entry test e = "done is 0" (true only initially); exit test
-- f = "remaining is 0".  A start of (2,0) runs exactly TWO trips to (0,2),
-- exercising e-loop / r-iter / r-exit; inv-sound runs it back.
------------------------------------------------------------------------

module Examples where
  open import Data.Nat using (ℕ; zero; suc)
  open Core (ℕ × ℕ)

  isZero : ℕ → Bool
  isZero zero    = true
  isZero (suc _) = false

  eP fP : ℕ × ℕ → Bool
  eP (_ , d) = isZero d    -- entry: nothing moved yet
  fP (r , _) = isZero r    -- exit: nothing left to move

  moveF : ℕ × ℕ → ℕ × ℕ
  moveF (suc r , d) = (r , suc d)
  moveF (zero  , d) = (zero , d)

  Dmove Lid : Rel
  Dmove s t = moveF s ≡ t
  Lid   s t = s ≡ t

  moveLoop : Cmd
  moveLoop = loop eP (atom Dmove) (atom Lid) fP

  -- two-trip forward run:  (2,0) → (1,1) → (0,2).
  ex-run : moveLoop ⊢ (2 , 0) ⇒ (0 , 2)
  ex-run =
    e-loop refl (e-atom refl)
      (r-iter refl (e-atom refl) refl (e-atom refl)
        (r-exit refl))

  -- inversion is involutive on this loop…
  ex-inv-inv : inv (inv moveLoop) ≡ moveLoop
  ex-inv-inv = refl

  -- …and the inverse loop runs the result back to the start (trip count
  -- recovered from the swapped assertions -- the R1a'-guard payoff).
  ex-rev : inv moveLoop ⊢ (0 , 2) ⇒ (2 , 0)
  ex-rev = inv-sound ex-run

------------------------------------------------------------------------
-- The reversible loop connected to the ENCODABLE language: its atoms are
-- `RevInst.RCmd` ops (here `swp`), read as the graph of `RevInst.fwd`.  This is
-- the reversible-control substrate the eventual `rsint : RCmd` (R3c step 2) is
-- built from -- loops whose bodies are honest, first-order reversible programs.
------------------------------------------------------------------------

module VLoop where
  open import UniversalInterp using (V; ⟨⟩; _∙_)
  import RevInst as R
  open Core V

  atomR : R.RCmd → Rel
  atomR c s t = R.fwd c s ≡ t

  allTrue : V → Bool
  allTrue _ = true

  -- a one-trip loop whose body is the encodable swap `swp`; on ⟨⟩∙(⟨⟩∙⟨⟩) it
  -- swaps the root's subtrees to (⟨⟩∙⟨⟩)∙⟨⟩.
  swpLoop : Cmd
  swpLoop = loop allTrue (atom (atomR R.swp)) (atom (atomR R.swp)) allTrue

  ex-vrun : swpLoop ⊢ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ⇒ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)
  ex-vrun = e-loop refl (e-atom refl) (r-exit refl)

  ex-vrev : inv swpLoop ⊢ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ⇒ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩))
  ex-vrev = inv-sound ex-vrun
