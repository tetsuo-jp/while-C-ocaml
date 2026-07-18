{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- CLOSURE OF SEMI-DECIDABLE SETS UNDER ∩ AND ∪
-- (Jones, Thm 5.5.2 `thm-decidable-semidecidable-closure-properties`, the
-- SEMI-decidable half).  `DecidableClosure` proved the decidable half and its
-- header promised "semi-decidable closure under ∩/∪ ... are in SemiDecidable";
-- but `SemiDecidable` only has Cor 5.6.2.  This module fills that gap.
--
-- MODEL.  A semi-decider is an I-language expression `e` (UniversalInterp.Exp),
-- run in a fixed ambient program environment `prog` (which resolves `call`) by
-- the step-counting interpreter `StepCount.evalT`.  Its HALTING predicate is
--
--     HaltsE e x  =  ∃ fuel n, the run `evalT n e x` returns a result,
--
-- and a set `S ⊆ D` is SEMI-DECIDABLE when some `e` halts on `x` exactly when
-- `x ∈ S`.  (This is the honest reading for combinators that share one recursion
-- environment; two semi-deciders combine inside the same `prog`.)
--
-- ∩ IS FULLY CONSTRUCTIVE, no hypothesis: `kcons e₁ e₂` runs BOTH e₁ and e₂ on
-- the same input and returns (hence halts) exactly when both halt
-- (`haltsE-kcons`), by the definition of `evalT` on `kcons` plus fuel-
-- monotonicity (`StepCountMono`).  So `sdec-∩` needs nothing external.
--
-- ∪ NEEDS DOVETAILING.  A single program halting iff e₁ OR e₂ halts must
-- interleave the two runs; in the strict, fuel-indexed semantics this is a
-- search loop (via `call`).  We package the required combinator as a `Dovetail`
-- record (its construction is the named remaining obligation) and prove `sdec-∪`
-- from it -- exactly as `SemiDecidable` takes the dovetailing `Post` as a
-- hypothesis.  --safe, axiom-free.
--
-- What this does NOT prove: the concrete construction of the `Dovetail`
-- combinator as an `Exp` (a fuel-search recursion) and its `evalT` correctness.
------------------------------------------------------------------------

module SemiDecClosure where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; _≤′_; ≤′-refl; ≤′-step)
open import Data.Nat.Properties using (≤⇒≤′; m≤m+n; m≤n+m)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; knil; kcons; khd; ktl; kifnil; call
        ; Maybe; just; nothing; nothing≢just; _>>=_)
import StepCount as SC
import StepCountMono

module _ (prog : Exp) where

  open SC.StepCount prog
  open StepCountMono.Mono prog using (evalT-mono; add-fuelT)

  ----------------------------------------------------------------------
  -- Fuel-monotonicity as a `≤` lifting (from `evalT-mono`, via `≤′` whose
  -- +1-steps match the interpreter's fuel recursion).

  evalT-le′ : ∀ {e x r} {n m} → n ≤′ m → evalT n e x ≡ just r → evalT m e x ≡ just r
  evalT-le′ ≤′-refl                     h = h
  evalT-le′ {e} {x} (≤′-step {n = k} p) h = evalT-mono k e x (evalT-le′ p h)

  evalT-le : ∀ {n m e x r} → n ≤ m → evalT n e x ≡ just r → evalT m e x ≡ just r
  evalT-le n≤m = evalT-le′ (≤⇒≤′ n≤m)

  ----------------------------------------------------------------------
  -- The halting predicate of an expression, and semi-decidability.

  HaltsE : Exp → V → Set
  HaltsE e x = Σ ℕ (λ n → Σ (V × ℕ) (λ r → evalT n e x ≡ just r))

  SemiDec : (V → Set) → Set
  SemiDec S = Σ Exp (λ e → (x : V) → (S x → HaltsE e x) × (HaltsE e x → S x))

  ----------------------------------------------------------------------
  -- KEYSTONE (∩): `kcons a b` halts iff both `a` and `b` halt.

  haltsE-kcons : ∀ a b x → (HaltsE (kcons a b) x → HaltsE a x × HaltsE b x)
                         × (HaltsE a x × HaltsE b x → HaltsE (kcons a b) x)
  haltsE-kcons a b x = fwd , bwd
    where
      fwd : HaltsE (kcons a b) x → HaltsE a x × HaltsE b x
      fwd (suc n , r , p) with evalT n a x in eqa
      ... | nothing = ⊥-elim (nothing≢just p)
      ... | just ra with evalT n b x in eqb
      ...   | nothing = ⊥-elim (nothing≢just p)
      ...   | just rb = (n , ra , eqa) , (n , rb , eqb)

      bwd : HaltsE a x × HaltsE b x → HaltsE (kcons a b) x
      bwd ((na , ra , pa) , (nb , rb , pb)) =
        suc (na + nb) , _ , goal
        where
          pa' : evalT (na + nb) a x ≡ just ra
          pa' = evalT-le (m≤m+n na nb) pa
          pb' : evalT (na + nb) b x ≡ just rb
          pb' = evalT-le (m≤n+m nb na) pb
          goal : evalT (suc (na + nb)) (kcons a b) x
               ≡ just (proj₁ ra ∙ proj₁ rb , suc (proj₂ ra + proj₂ rb))
          goal rewrite pa' | pb' = refl

  ----------------------------------------------------------------------
  -- ∩ closure: no hypothesis.

  sdec-∩ : ∀ {S T} → SemiDec S → SemiDec T → SemiDec (λ x → S x × T x)
  sdec-∩ {S} {T} (e₁ , f₁) (e₂ , f₂) = kcons e₁ e₂ , g
    where
      g : (x : V) → ((S x × T x) → HaltsE (kcons e₁ e₂) x)
                  × (HaltsE (kcons e₁ e₂) x → (S x × T x))
      g x = to , from
        where
          to : S x × T x → HaltsE (kcons e₁ e₂) x
          to (s , t) = proj₂ (haltsE-kcons e₁ e₂ x)
                         (proj₁ (f₁ x) s , proj₁ (f₂ x) t)
          from : HaltsE (kcons e₁ e₂) x → S x × T x
          from h with proj₁ (haltsE-kcons e₁ e₂ x) h
          ... | (h₁ , h₂) = proj₂ (f₁ x) h₁ , proj₂ (f₂ x) h₂

  ----------------------------------------------------------------------
  -- ∪ closure: relative to a dovetailing combinator (its construction is the
  -- named remaining obligation; see header).

  record Dovetail : Set where
    field
      _⊕_   : Exp → Exp → Exp
      ⊕-fwd : ∀ a b x → HaltsE (a ⊕ b) x → HaltsE a x ⊎ HaltsE b x
      ⊕-bwd : ∀ a b x → HaltsE a x ⊎ HaltsE b x → HaltsE (a ⊕ b) x

  sdec-∪ : ∀ {S T} → Dovetail → SemiDec S → SemiDec T → SemiDec (λ x → S x ⊎ T x)
  sdec-∪ {S} {T} dt (e₁ , f₁) (e₂ , f₂) = (e₁ ⊕ e₂) , g
    where
      open Dovetail dt
      g : (x : V) → ((S x ⊎ T x) → HaltsE (e₁ ⊕ e₂) x)
                  × (HaltsE (e₁ ⊕ e₂) x → (S x ⊎ T x))
      g x = to , from
        where
          to : S x ⊎ T x → HaltsE (e₁ ⊕ e₂) x
          to (inj₁ s) = ⊕-bwd e₁ e₂ x (inj₁ (proj₁ (f₁ x) s))
          to (inj₂ t) = ⊕-bwd e₁ e₂ x (inj₂ (proj₁ (f₂ x) t))
          from : HaltsE (e₁ ⊕ e₂) x → S x ⊎ T x
          from h with ⊕-fwd e₁ e₂ x h
          ... | inj₁ h₁ = inj₁ (proj₂ (f₁ x) h₁)
          ... | inj₂ h₂ = inj₂ (proj₂ (f₂ x) h₂)

  ----------------------------------------------------------------------
  -- Test-first: `var` is a total semi-decider (always halts), so it
  -- semi-decides the full set; `kcons var var` too, by ∩.

  module Examples where
    var-halts : ∀ x → HaltsE var x
    var-halts x = 1 , (x , 1) , refl

    Full : V → Set
    Full _ = V   -- always inhabited (e.g. by the input itself)

    sd-full : SemiDec Full
    sd-full = var , λ x → (λ _ → var-halts x) , (λ _ → x)

    sd-full∩full : SemiDec (λ x → Full x × Full x)
    sd-full∩full = sdec-∩ sd-full sd-full

    kcons-var-halts : ∀ x → HaltsE (kcons var var) x
    kcons-var-halts x = proj₂ (haltsE-kcons var var x) (var-halts x , var-halts x)
