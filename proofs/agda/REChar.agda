{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- r.e. CHARACTERIZATIONS (Jones, Thm 5.7.2 `thm-re-characterizations`), the
-- dovetail-free directions.
--
-- The theorem lists four equivalent conditions on a set `A ⊆ D`:
--   (1) A is WHILE-enumerable;   (2) A is semi-decidable;
--   (3) A is the RANGE of a computable partial function;
--   (4) A is the DOMAIN of a computable partial function.
--
-- In the fuel/`evalT` model semi-decidability of `S` by expression `e` IS
-- membership in the domain of `⟦e⟧` (`HaltsE e x = ∃ fuel, evalT halts`), so
-- (2)⇔(4) holds definitionally (`char-dom`).  (2)⇒(3) is constructive with no
-- dovetailing: `f = ktl (kcons e var)` runs `e` on its input for its halting
-- effect and returns the input unchanged, so `⟦f⟧(x) = x` exactly when `e`
-- halts on `x`; hence `range ⟦f⟧ = dom ⟦e⟧ = A` (`char-range`).
--
-- What this does NOT prove (the directions needing DOVETAILING, the same search
-- combinator deferred in `SemiDecClosure.sdec-∪`): (1)⇒(2) [search the
-- enumerator for a producer of x] and (3)⇒(1)/(3)⇒(2) [enumerate the domain,
-- collect the range].  --safe, axiom-free.
------------------------------------------------------------------------

module REChar where

open import Data.Nat using (ℕ; suc; _+_; _≤_)
open import Data.Nat.Properties using (≤-refl; n≤1+n)
open import Data.Product using (Σ; ∃; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; Exp; var; ktl; kcons; Maybe; just; nothing; nothing≢just; tl)
import StepCount as SC
import SemiDecClosure
import EvalTFuelTime as FT

module _ (prog : Exp) where

  open SC.StepCount prog using (evalT)

  private
    HaltsE  = SemiDecClosure.HaltsE  prog
    SemiDec = SemiDecClosure.SemiDec prog
    evalT-le = FT.evalT-le prog

    just-inj : ∀ {r r' : V × ℕ} → just r ≡ just r' → r ≡ r'
    just-inj refl = refl

  _⇔_ : Set → Set → Set
  A ⇔ B = (A → B) × (B → A)

  Dom : Exp → V → Set
  Dom e x = HaltsE e x

  Range : Exp → V → Set
  Range f y = ∃ (λ x → ∃ (λ n → ∃ (λ t → evalT n f x ≡ just (y , t))))

  ----------------------------------------------------------------------
  -- (2) ⇔ (4): semi-decidable ⇔ domain of a computable function.
  -- Definitional: `SemiDec S` already says `∀ x, S x ⇔ HaltsE e x` for some e.

  char-dom : ∀ {S} → SemiDec S ⇔ (Σ Exp (λ e → (x : V) → S x ⇔ Dom e x))
  char-dom = (λ z → z) , (λ z → z)

  ----------------------------------------------------------------------
  -- The producer `f = ktl (kcons e var)`: ⟦f⟧(x) = x iff e halts on x.

  evalT-var : ∀ n x {r} → evalT n var x ≡ just r → r ≡ (x , 1)
  evalT-var (suc n) x refl = refl

  -- kcons e var halts on x  ⇒  its value is (⟦e⟧x ∙ x), and e halts on x.
  kcons-value : ∀ e n x {vk tk} → evalT n (kcons e var) x ≡ just (vk , tk)
              → (∃ (λ ve → vk ≡ ve ∙ x)) × HaltsE e x
  kcons-value e (suc n) x h with evalT n e x in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , te) with evalT n var x in eqv
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rv with evalT-var n x eqv
  ...     | refl = (ve , sym (cong proj₁ (just-inj h))) , (n , (ve , te) , eqe)

  -- f halts on x  ⇒  its result value is x, and e halts on x.
  f-value : ∀ e n x {v t} → evalT n (ktl (kcons e var)) x ≡ just (v , t)
          → (v ≡ x) × HaltsE e x
  f-value e (suc n) x h with evalT n (kcons e var) x in eqK
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (vk , tk) with kcons-value e n x eqK
  ...   | (ve , refl) , halts = sym (cong proj₁ (just-inj h)) , halts

  -- e halts on x  ⇒  f halts on x returning x.
  f-halts : ∀ e x → HaltsE e x
          → ∃ (λ n → ∃ (λ t → evalT n (ktl (kcons e var)) x ≡ just (x , t)))
  f-halts e x (ne , (ve , te) , he) =
    suc (suc (suc ne)) , suc (suc (te + 1)) , goal
    where
      he′ : evalT (suc ne) e x ≡ just (ve , te)
      he′ = evalT-le (n≤1+n ne) he
      goal : evalT (suc (suc (suc ne))) (ktl (kcons e var)) x
           ≡ just (x , suc (suc (te + 1)))
      goal rewrite he′ = refl

  ----------------------------------------------------------------------
  -- (2) ⇒ (3): every semi-decidable set is the range of a computable function.

  char-range : ∀ {S} → SemiDec S → Σ Exp (λ f → (y : V) → S y ⇔ Range f y)
  char-range {S} (e , sd) = ktl (kcons e var) , λ y → to y , from y
    where
      to : ∀ y → S y → Range (ktl (kcons e var)) y
      to y sy with f-halts e y (proj₁ (sd y) sy)
      ... | n , t , h = y , n , t , h
      from : ∀ y → Range (ktl (kcons e var)) y → S y
      from y (x , n , t , h) with f-value e n x h
      ... | refl , halts = proj₂ (sd y) halts
