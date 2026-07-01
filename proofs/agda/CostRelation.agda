{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- A big-step COST RELATION for the object language (Levin plan B1.2, Phase 1).
--
-- `e , env ⇓ v # c` means: expression `e` (under the ambient program `prog`) on
-- input `env` evaluates to `v` using `c` unit-cost steps.  It is the fuel-free
-- characterisation of `StepCount.evalT`'s step count -- one rule per constructor,
-- with the SAME counts (one per node, summed over sub-derivations).  Because a
-- sub-computation enters a rule as an OPAQUE premise (a derivation, never a
-- reducing term), the cost proof for the self-interpreter (Phase 2/3) can splice a
-- symbolic sub-run without Agda trying (and failing) to reduce it -- the obstacle
-- that sank the `refl`-dispatch route (see docs/LEVIN-PLAN.md, B1.2 finding).
--
-- This module: the relation + `complete` (evalT ⇒ ⇓).  `sound` (⇓ ⇒ evalT) is
-- added next, needing evalT-monotonicity.
------------------------------------------------------------------------

module CostRelation where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst₂)

open import UniversalInterp
  using (V; ⟨⟩; _∙_; just; nothing; nothing≢just; hd; tl;
         Exp; var; knil; kcons; khd; ktl; kifnil; call)
open import StepCount using (module StepCount)

module _ (prog : Exp) where
  open StepCount.StepCount prog using (evalT)

  -- big-step cost relation.  `prog` is the ambient global program (`call`).
  infix 4 _,_⇓_#_
  data _,_⇓_#_ : Exp → V → V → ℕ → Set where
    ⇓var   : ∀ {env} → var , env ⇓ env # 1
    ⇓knil  : ∀ {env} → knil , env ⇓ ⟨⟩ # 1
    ⇓khd   : ∀ {e env v c} → e , env ⇓ v # c → khd e , env ⇓ hd v # suc c
    ⇓ktl   : ∀ {e env v c} → e , env ⇓ v # c → ktl e , env ⇓ tl v # suc c
    ⇓kcons : ∀ {a b env va vb ca cb} →
             a , env ⇓ va # ca → b , env ⇓ vb # cb →
             kcons a b , env ⇓ (va ∙ vb) # suc (ca + cb)
    ⇓then  : ∀ {g t e env vt cg ct} →
             g , env ⇓ ⟨⟩ # cg → t , env ⇓ vt # ct →
             kifnil g t e , env ⇓ vt # suc (cg + ct)
    ⇓else  : ∀ {g t e env vh vl ve cg ce} →
             g , env ⇓ (vh ∙ vl) # cg → e , env ⇓ ve # ce →
             kifnil g t e , env ⇓ ve # suc (cg + ce)
    ⇓call  : ∀ {e env arg v ca cv} →
             e , env ⇓ arg # ca → prog , arg ⇓ v # cv →
             call e , env ⇓ v # suc (ca + cv)

  private
    fst≡ : ∀ {v v' : V} {t t' : ℕ} → just (v , t) ≡ just (v' , t') → v ≡ v'
    fst≡ refl = refl
    snd≡ : ∀ {v v' : V} {t t' : ℕ} → just (v , t) ≡ just (v' , t') → t ≡ t'
    snd≡ refl = refl

  ------------------------------------------------------------------------
  -- completeness: a terminating `evalT` run yields a derivation with equal count.
  -- Induction on the fuel `G`, mirroring `evalT`; each case reads off the sub-runs
  -- and applies the matching rule, transporting value+count along `just`-injections.
  ------------------------------------------------------------------------

  complete : ∀ G e env {v c} → evalT G e env ≡ just (v , c) → e , env ⇓ v # c
  complete zero    e env ()
  complete (suc n) var  env h = subst₂ (λ w k → var  , env ⇓ w # k) (fst≡ h) (snd≡ h) ⇓var
  complete (suc n) knil env h = subst₂ (λ w k → knil , env ⇓ w # k) (fst≡ h) (snd≡ h) ⇓knil
  complete (suc n) (khd e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , ce) =
        subst₂ (λ w k → khd e , env ⇓ w # k) (fst≡ h) (snd≡ h) (⇓khd (complete n e env eqe))
  complete (suc n) (ktl e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (ve , ce) =
        subst₂ (λ w k → ktl e , env ⇓ w # k) (fst≡ h) (snd≡ h) (⇓ktl (complete n e env eqe))
  complete (suc n) (kcons a b) env h with evalT n a env in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (va , ca) with evalT n b env in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vb , cb) =
          subst₂ (λ w k → kcons a b , env ⇓ w # k) (fst≡ h) (snd≡ h)
                 (⇓kcons (complete n a env eqa) (complete n b env eqb))
  complete (suc n) (kifnil g t e) env h with evalT n g env in eqg
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (⟨⟩ , cg) with evalT n t env in eqt
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (vt , ct) =
          subst₂ (λ w k → kifnil g t e , env ⇓ w # k) (fst≡ h) (snd≡ h)
                 (⇓then (complete n g env eqg) (complete n t env eqt))
  complete (suc n) (kifnil g t e) env h | just ((vh ∙ vl) , cg) with evalT n e env in eqe
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (ve , ce) =
          subst₂ (λ w k → kifnil g t e , env ⇓ w # k) (fst≡ h) (snd≡ h)
                 (⇓else (complete n g env eqg) (complete n e env eqe))
  complete (suc n) (call e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (arg , ca) with evalT n prog arg in eqp
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just (rv , cv) =
          subst₂ (λ w k → call e , env ⇓ w # k) (fst≡ h) (snd≡ h)
                 (⇓call (complete n e env eqe) (complete n prog arg eqp))
