{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- REVERSIBLE LEVIN, step R1a: a minimal reversible core language (REXP).
--
-- Per docs/REV-LEVIN-SKELETON.md the R-track needs a small, SELF-CONTAINED
-- reversible language in Agda (NOT a model of full R-WHILE) on which to state
-- step count (R1b) and the Bennett lemma (R1c), ⇒ the reversible interpreter
-- overhead a-rev (R3, measured ≈ 364× in rwhile-C-ocaml/measure_ri_overhead.sh).
--
-- The essence of reversibility, captured minimally:
--   * atomic step = a SELF-INVERSE store update (an involution `f`, `f∘f = id`) --
--     the abstract content of R-WHILE's self-cancelling `X ^= E`.  Its syntactic
--     inverse is ITSELF (the hallmark of reversible assignment).
--   * `seq` inverts by reversing order and inverting each part.
--   * a structured reversible `cond` whose branches PRESERVE the guard (the
--     sufficient condition for invertibility without a separate exit assertion).
-- `fwd`/`inv`/`bwd` and the keystone `inv-sound : bwd (fwd s) ≡ s` (running the
-- inverse forward undoes the forward run) -- the reversible twin of a semantics'
-- determinism, and the substrate R1b/R1c/R3 build on.
--
-- Scope (honest): loops (`from E do C loop D until F`, needing exit assertions +
-- termination) and the GENERAL exit-assertion conditional are the next step
-- (R1a'); the swap-based instance below is arithmetic-free, in the Toffoli/Fredkin
-- spirit, and its full round-trip (both directions) is checked concretely.
------------------------------------------------------------------------

module RevCore where

open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; trans; cong; cong₂)

open import UniversalInterp using (V; ⟨⟩; _∙_)

------------------------------------------------------------------------
-- The language, parametric in the store type S.
------------------------------------------------------------------------

module Language (S : Set) where

  -- a SELF-INVERSE store update: `X ^= E` distilled to its algebra.
  Upd : Set
  Upd = Σ (S → S) (λ f → ∀ s → f (f s) ≡ s)

  data Cmd : Set where
    atom : Upd → Cmd
    seq  : Cmd → Cmd → Cmd
    cond : (S → Bool) → Cmd → Cmd → Cmd

  -- forward semantics (total)
  fwd : Cmd → S → S
  fwd (atom u)     s = proj₁ u s
  fwd (seq c d)    s = fwd d (fwd c s)
  fwd (cond p c d) s = if p s then fwd c s else fwd d s

  -- syntactic inversion: atoms are self-inverse; seq reverses; cond keeps the guard
  inv : Cmd → Cmd
  inv (atom u)     = atom u
  inv (seq c d)    = seq (inv d) (inv c)
  inv (cond p c d) = cond p (inv c) (inv d)

  -- backward = run the inverse forward
  bwd : Cmd → S → S
  bwd c = fwd (inv c)

  -- inversion is an involution on programs
  inv-inv : (c : Cmd) → inv (inv c) ≡ c
  inv-inv (atom u)     = refl
  inv-inv (seq c d)    = cong₂ seq (inv-inv c) (inv-inv d)
  inv-inv (cond p c d) = cong₂ (cond p) (inv-inv c) (inv-inv d)

  -- well-formedness: the reversibility discipline for the structured `cond`
  -- (each branch leaves the guard unchanged, so the SAME guard recovers the
  -- branch on the way back -- no separate exit assertion needed).
  WF : Cmd → Set
  WF (atom u)     = ⊤
  WF (seq c d)    = WF c × WF d
  WF (cond p c d) = WF c × WF d
                  × (∀ s → p (fwd c s) ≡ p s)
                  × (∀ s → p (fwd d s) ≡ p s)

  ----------------------------------------------------------------------
  -- KEYSTONE (R1a): running the inverse forward undoes the forward run.
  ----------------------------------------------------------------------
  inv-sound : (c : Cmd) → WF c → ∀ s → bwd c (fwd c s) ≡ s
  inv-sound (atom u) _ s = proj₂ u s
  inv-sound (seq c d) (wc , wd) s
    rewrite inv-sound d wd (fwd c s) = inv-sound c wc s
  inv-sound (cond p c d) (wc , wd , pc , pd) s with p s in eqs
  ... | true  rewrite trans (pc s) eqs = inv-sound c wc s
  ... | false rewrite trans (pd s) eqs = inv-sound d wd s

------------------------------------------------------------------------
-- A concrete, arithmetic-free reversible instance: store = two tree variables,
-- updates built from `swapTop` (swap a node's two subtrees -- self-inverse).
------------------------------------------------------------------------

swapTop : V → V
swapTop ⟨⟩      = ⟨⟩
swapTop (a ∙ b) = b ∙ a

swapTop-invol : (v : V) → swapTop (swapTop v) ≡ v
swapTop-invol ⟨⟩      = refl
swapTop-invol (a ∙ b) = refl

isNil : V → Bool
isNil ⟨⟩      = true
isNil (_ ∙ _) = false

open Language (V × V)

-- swapTop the first / second variable (each self-inverse)
u1 : Upd
u1 = (λ { (a , b) → (swapTop a , b) }) , λ { (a , b) → cong (λ z → z , b) (swapTop-invol a) }

u2 : Upd
u2 = (λ { (a , b) → (a , swapTop b) }) , λ { (a , b) → cong (λ z → a , z) (swapTop-invol b) }

-- a reversible conditional: if var1 is nil, swap var2's subtrees, else var1's.
-- Both branches leave `isNil var1` unchanged, so the program is well-formed.
prog : Cmd
prog = cond (λ { (a , _) → isNil a }) (atom u2) (atom u1)

wf-prog : WF prog
wf-prog = tt , tt
        , (λ _ → refl)                             -- u2 doesn't touch var1
        , (λ { (⟨⟩ , _) → refl ; ((_ ∙ _) , _) → refl })

-- reversibility of the concrete program, from the general theorem
prog-reversible : ∀ s → bwd prog (fwd prog s) ≡ s
prog-reversible = inv-sound prog wf-prog

-- concrete round-trips, both directions, by computation (non-vacuity):
-- guard true  (var1 = nil): the then-branch swaps var2
_ : fwd prog (⟨⟩ , ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ≡ (⟨⟩ , (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)
_ = refl
_ : bwd prog (⟨⟩ , (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ≡ (⟨⟩ , ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩))
_ = refl
-- guard false (var1 = cons): the else-branch swaps var1
_ : fwd prog (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩) , ⟨⟩) ≡ (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) , ⟨⟩)
_ = refl
_ : bwd prog ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩ , ⟨⟩) ≡ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩) , ⟨⟩)
_ = refl
