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

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; _∸_)
open import Data.Nat.Properties using (m∸n+n≡m; m≤m+n; m≤n+m)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst; subst₂; cong)

open import UniversalInterp
  using (V; ⟨⟩; _∙_; just; nothing; nothing≢just; _>>=_; hd; tl;
         Exp; var; knil; kcons; khd; ktl; kifnil; call)
open import StepCount using (module StepCount)

module Rel (prog : Exp) where
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

  ------------------------------------------------------------------------
  -- evalT monotonicity (more fuel preserves a defined result) -- needed by `sound`.
  ------------------------------------------------------------------------

  evalT-mono : ∀ n e env {r} → evalT n e env ≡ just r → evalT (suc n) e env ≡ just r
  evalT-mono (suc n) var  env h = h
  evalT-mono (suc n) knil env h = h
  evalT-mono (suc n) (khd e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just re rewrite evalT-mono n e env eqe = h
  evalT-mono (suc n) (ktl e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just re rewrite evalT-mono n e env eqe = h
  evalT-mono (suc n) (kcons a b) env h with evalT n a env in eqa
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ra with evalT n b env in eqb
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rb rewrite evalT-mono n a env eqa | evalT-mono n b env eqb = h
  evalT-mono (suc n) (kifnil g t e) env h with evalT n g env in eqg
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just (⟨⟩ , cg) with evalT n t env in eqt
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rt rewrite evalT-mono n g env eqg | evalT-mono n t env eqt = h
  evalT-mono (suc n) (kifnil g t e) env h | just ((vh ∙ vl) , cg) with evalT n e env in eqe
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just re rewrite evalT-mono n g env eqg | evalT-mono n e env eqe = h
  evalT-mono (suc n) (call e) env h with evalT n e env in eqe
  ... | nothing = ⊥-elim (nothing≢just h)
  ... | just ra with evalT n prog (proj₁ ra) in eqp
  ...   | nothing = ⊥-elim (nothing≢just h)
  ...   | just rr rewrite evalT-mono n e env eqe | evalT-mono n prog (proj₁ ra) eqp = h

  evalT-mono-+ : ∀ k n e env {r} → evalT n e env ≡ just r → evalT (k + n) e env ≡ just r
  evalT-mono-+ zero    n e env h = h
  evalT-mono-+ (suc k) n e env h = evalT-mono (k + n) e env (evalT-mono-+ k n e env h)

  evalT-mono-≤ : ∀ {n n'} → n ≤ n' → ∀ e env {r} →
                 evalT n e env ≡ just r → evalT n' e env ≡ just r
  evalT-mono-≤ {n} {n'} n≤n' e env h =
    subst (λ z → evalT z e env ≡ _) (m∸n+n≡m n≤n') (evalT-mono-+ (n' ∸ n) n e env h)

  ------------------------------------------------------------------------
  -- soundness: every derivation is realised by some `evalT` run (same value+count).
  -- Two-premise rules lift both sub-runs to the common fuel `Ga + Gb`.
  ------------------------------------------------------------------------

  sound : ∀ {e env v c} → e , env ⇓ v # c → Σ ℕ (λ G → evalT G e env ≡ just (v , c))
  sound ⇓var  = 1 , refl
  sound ⇓knil = 1 , refl
  sound (⇓khd d) with sound d
  ... | G , h = suc G , cong (λ m → m >>= λ r → just (hd (proj₁ r) , suc (proj₂ r))) h
  sound (⇓ktl d) with sound d
  ... | G , h = suc G , cong (λ m → m >>= λ r → just (tl (proj₁ r) , suc (proj₂ r))) h
  sound (⇓kcons {a} {b} {env} {va} {vb} {ca} {cb} da db) with sound da | sound db
  ... | Ga , ha | Gb , hb = suc (Ga + Gb) , eqn
    where
      ha' : evalT (Ga + Gb) a env ≡ just (va , ca)
      ha' = evalT-mono-≤ (m≤m+n Ga Gb) a env ha
      hb' : evalT (Ga + Gb) b env ≡ just (vb , cb)
      hb' = evalT-mono-≤ (m≤n+m Gb Ga) b env hb
      eqn : evalT (suc (Ga + Gb)) (kcons a b) env ≡ just (va ∙ vb , suc (ca + cb))
      eqn rewrite ha' | hb' = refl
  sound (⇓then {g} {t} {e} {env} {vt} {cg} {ct} dg dt) with sound dg | sound dt
  ... | Gg , hg | Gt , ht = suc (Gg + Gt) , eqn
    where
      hg' : evalT (Gg + Gt) g env ≡ just (⟨⟩ , cg)
      hg' = evalT-mono-≤ (m≤m+n Gg Gt) g env hg
      ht' : evalT (Gg + Gt) t env ≡ just (vt , ct)
      ht' = evalT-mono-≤ (m≤n+m Gt Gg) t env ht
      eqn : evalT (suc (Gg + Gt)) (kifnil g t e) env ≡ just (vt , suc (cg + ct))
      eqn rewrite hg' | ht' = refl
  sound (⇓else {g} {t} {e} {env} {vh} {vl} {ve} {cg} {ce} dg de) with sound dg | sound de
  ... | Gg , hg | Ge , he = suc (Gg + Ge) , eqn
    where
      hg' : evalT (Gg + Ge) g env ≡ just (vh ∙ vl , cg)
      hg' = evalT-mono-≤ (m≤m+n Gg Ge) g env hg
      he' : evalT (Gg + Ge) e env ≡ just (ve , ce)
      he' = evalT-mono-≤ (m≤n+m Ge Gg) e env he
      eqn : evalT (suc (Gg + Ge)) (kifnil g t e) env ≡ just (ve , suc (cg + ce))
      eqn rewrite hg' | he' = refl
  sound (⇓call {e} {env} {arg} {v} {ca} {cv} de dp) with sound de | sound dp
  ... | Ge , he | Gp , hp = suc (Ge + Gp) , eqn
    where
      he' : evalT (Ge + Gp) e env ≡ just (arg , ca)
      he' = evalT-mono-≤ (m≤m+n Ge Gp) e env he
      hp' : evalT (Ge + Gp) prog arg ≡ just (v , cv)
      hp' = evalT-mono-≤ (m≤n+m Gp Ge) prog arg hp
      eqn : evalT (suc (Ge + Gp)) (call e) env ≡ just (v , suc (ca + cv))
      eqn rewrite he' | hp' = refl

  ------------------------------------------------------------------------
  -- CANONICAL FUEL: fuel = the exact step count `c` already suffices to reproduce a
  -- derivation's run (each node spends ≥ 1 step, so depth ≤ count).  By induction on
  -- the derivation; two-premise rules lift each sub-run from its own count to the sum.
  -- (Levin B3b: lets the self-interpreter run be replayed at fuel = its step budget.)
  ------------------------------------------------------------------------

  evalT-count : ∀ {e env v c} → e , env ⇓ v # c → evalT c e env ≡ just (v , c)
  evalT-count ⇓var  = refl
  evalT-count ⇓knil = refl
  evalT-count (⇓khd d) rewrite evalT-count d = refl
  evalT-count (⇓ktl d) rewrite evalT-count d = refl
  evalT-count (⇓kcons {ca = ca} {cb = cb} da db)
    rewrite evalT-mono-≤ (m≤m+n ca cb) _ _ (evalT-count da)
          | evalT-mono-≤ (m≤n+m cb ca) _ _ (evalT-count db) = refl
  evalT-count (⇓then {cg = cg} {ct = ct} dg dt)
    rewrite evalT-mono-≤ (m≤m+n cg ct) _ _ (evalT-count dg)
          | evalT-mono-≤ (m≤n+m ct cg) _ _ (evalT-count dt) = refl
  evalT-count (⇓else {cg = cg} {ce = ce} dg de)
    rewrite evalT-mono-≤ (m≤m+n cg ce) _ _ (evalT-count dg)
          | evalT-mono-≤ (m≤n+m ce cg) _ _ (evalT-count de) = refl
  evalT-count (⇓call {ca = ca} {cv = cv} de dp)
    rewrite evalT-mono-≤ (m≤m+n ca cv) _ _ (evalT-count de)
          | evalT-mono-≤ (m≤n+m cv ca) _ _ (evalT-count dp) = refl
