{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- The operational phase oracle for Levin's search, over an ABSTRACT budgeted runner
-- (Levin plan Layer B3b, made small).
--
-- The dovetail is the Jones STAGE model: stage `i` runs programs `0..i`, giving program
-- `m` a budget of `pow2 (i ∸ m)` steps; `done i` is true iff some program in the batch
-- halts within its budget with a `chk`-verified output.  The construction is parameterised
-- by an abstract `run : ℕ → Exp → V → Maybe V` ("run p on x within budget b").  We prove
-- GREEN, over this abstraction:
--   * `done-sound`  -- a true phase yields a `chk`-verified witness;
--   * `done-intro`  -- a hitting program in the batch makes the phase true;
--   * `finishing`   -- given the solver `q = enum j` reaches its output `yq` within any
--                      budget `≥ 73·Tq` (`run-hits`), some stage `k = j + d` has
--                      `done k = true` and `gsum k < a_q·(Tq+Tr)` (via `overshoot′`,
--                      `pow2-+`, and the ring solver).
--
-- So B3b is reduced to ONE remaining obligation: instantiate `run := runFor` (the real
-- `sint`/`StepCount` runner) and prove `run-hits` from `IntEfficient.int-efficient-time`
-- -- a single, isolated, design-risk-free StepCount lemma.  See docs/LEVIN-B3-DESIGN.md.
------------------------------------------------------------------------

module LevinDovetailRun where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _≤_; _<_; s≤s; z≤n)
open import Data.Nat.Properties
  using (≤-refl; ≤-reflexive; ≤-trans; <-≤-trans; <-cmp; +-mono-≤; *-monoʳ-≤;
         m≤m+n; m≤n+m; m≤n⇒m≤1+n; m+n∸m≡n; <⇒≱)
open import Data.Nat.Solver using (module +-*-Solver)
open import Data.Bool using (Bool; true; false; _∨_)
open import Data.Product using (Σ; _×_; _,_)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; cong; subst)

open import UniversalInterp using (V; ⟨⟩; Exp; Maybe; just; nothing)
open import ProgramDecode using (enum)
open import LevinBudget using (pow2; gsum; gsum<2last; pow2-+; overshoot′)
open import LevinOptimal using (aq; S)
open import LevinGrounded using (search)

open +-*-Solver

private
  f≢t : false ≡ true → ⊥
  f≢t ()

  -- 1 ≤ (suc k) * n  for n ≥ 1  (used to feed overshoot′ with N = 73·Tq).
  1≤sk*n : ∀ k n → 1 ≤ n → 1 ≤ suc k * n
  1≤sk*n k (suc _) _ = s≤s z≤n
  1≤sk*n k zero    ()

  -- Bool ∨ introductions.
  ∨-introˡ : ∀ {a} b → a ≡ true → a ∨ b ≡ true
  ∨-introˡ b refl = refl
  ∨-introʳ : ∀ a {b} → b ≡ true → a ∨ b ≡ true
  ∨-introʳ true  _    = refl
  ∨-introʳ false refl = refl

------------------------------------------------------------------------
-- The oracle, over an abstract budgeted runner.
------------------------------------------------------------------------

module Oracle (run : ℕ → Exp → V → Maybe V) (chk : V → V → Bool) (x : V) where

  hitOn : Maybe V → Bool
  hitOn (just y) = chk x y
  hitOn nothing  = false

  -- program `m` at stage `i`: run enum m on x within budget pow2 (i ∸ m); verify.
  hit : ℕ → ℕ → Bool
  hit i m = hitOn (run (pow2 (i ∸ m)) (enum m) x)

  -- stage i succeeds if some program 0..bound hits.
  anyHit : ℕ → ℕ → Bool
  anyHit i zero    = hit i 0
  anyHit i (suc m) = hit i (suc m) ∨ anyHit i m

  done : ℕ → Bool
  done i = anyHit i i

  ------------------------------------------------------------------------
  -- SOUNDNESS.
  ------------------------------------------------------------------------

  hit-sound : ∀ i m → hit i m ≡ true → Σ V (λ y → chk x y ≡ true)
  hit-sound i m h with run (pow2 (i ∸ m)) (enum m) x
  ... | just y  = y , h
  ... | nothing = ⊥-elim (f≢t h)

  anyHit-sound : ∀ i b → anyHit i b ≡ true → Σ ℕ (λ m → (m ≤ b) × (hit i m ≡ true))
  anyHit-sound i zero    h = 0 , ≤-refl , h
  anyHit-sound i (suc b) h with hit i (suc b) in hb
  ... | true  = suc b , ≤-refl , hb
  ... | false with anyHit-sound i b h
  ...   | m , m≤b , hm = m , m≤n⇒m≤1+n m≤b , hm

  done-sound : ∀ i → done i ≡ true → Σ V (λ y → chk x y ≡ true)
  done-sound i h with anyHit-sound i i h
  ... | m , _ , hm = hit-sound i m hm

  ------------------------------------------------------------------------
  -- INTRODUCTION: a hitting program in the batch makes the phase true.
  ------------------------------------------------------------------------

  anyHit-intro : ∀ i b m → m ≤ b → hit i m ≡ true → anyHit i b ≡ true
  anyHit-intro i zero    .zero z≤n     hm = hm
  anyHit-intro i (suc b) m    m≤sb     hm with <-cmp m (suc b)
  ... | tri< (s≤s m≤b) _ _ = ∨-introʳ (hit i (suc b)) (anyHit-intro i b m m≤b hm)
  ... | tri≈ _ m≡sb _      = ∨-introˡ (anyHit i b) (subst (λ z → hit i z ≡ true) m≡sb hm)
  ... | tri> _ _ sb<m      = ⊥-elim (<⇒≱ sb<m m≤sb)

  done-intro : ∀ i m → m ≤ i → hit i m ≡ true → done i ≡ true
  done-intro i m m≤i hm = anyHit-intro i i m m≤i hm

  ------------------------------------------------------------------------
  -- FINISHING: the solver `q = enum j` (reaching `yq` within budget ≥ 73·Tq) makes some
  -- stage `k = j + d` succeed, with `gsum k < a_q·(Tq+Tr)`.
  ------------------------------------------------------------------------

  module _ (j Tq Tr : ℕ) (yq : V)
           (chk-yq   : chk x yq ≡ true)
           (run-hits : ∀ b → 73 * Tq ≤ b → run b (enum j) x ≡ just yq)
           (1≤Tq     : 1 ≤ Tq) where

    -- the polynomial identity behind the bound: 2·(P·2^d) = a_q · Tq  when 2^d = 2·73·Tq.
    -- proved abstractly (variables P, T) by the ring solver, then applied at P = pow2 j.
    polyId : ∀ P T → P * (73 * T + 73 * T) + P * (73 * T + 73 * T)
                 ≡ ((73 * P + 73 * P) + (73 * P + 73 * P)) * T
    polyId = solve 2 (λ P T →
             P :* (con 73 :* T :+ con 73 :* T) :+ P :* (con 73 :* T :+ con 73 :* T)
          := ((con 73 :* P :+ con 73 :* P) :+ (con 73 :* P :+ con 73 :* P)) :* T) refl

    finishing : Σ ℕ (λ k → (done k ≡ true) × (gsum k < aq (search j Tq Tr) * S (search j Tq Tr)))
    finishing with overshoot′ (73 * Tq) (1≤sk*n 72 Tq 1≤Tq)
    ... | d , 73Tq≤pow2d , pow2d≤2·73Tq = j + d , done-k , bound
      where
        P = pow2 j

        -- program j's budget at stage k = j+d is pow2 ((j+d) ∸ j) = pow2 d ≥ 73·Tq,
        -- so run returns yq and the batch hits.
        run-j : run (pow2 ((j + d) ∸ j)) (enum j) x ≡ just yq
        run-j = subst (λ z → run (pow2 z) (enum j) x ≡ just yq)
                      (sym (m+n∸m≡n j d)) (run-hits (pow2 d) 73Tq≤pow2d)

        hit-kj : hit (j + d) j ≡ true
        hit-kj = subst (λ w → hitOn w ≡ true) (sym run-j) chk-yq

        done-k : done (j + d) ≡ true
        done-k = done-intro (j + d) j (m≤m+n j d) hit-kj

        -- the bound: gsum k < pow2 (suc k) ≤ P·2^d + P·2^d ≤ ... = a_q·Tq ≤ a_q·(Tq+Tr).
        bound : gsum (j + d) < aq (search j Tq Tr) * S (search j Tq Tr)
        bound = <-≤-trans (gsum<2last (j + d))
          (≤-trans (≤-reflexive (cong (λ z → z + z) (pow2-+ j d)))
          (≤-trans (+-mono-≤ (*-monoʳ-≤ P pow2d≤2·73Tq) (*-monoʳ-≤ P pow2d≤2·73Tq))
          (≤-trans (≤-reflexive (polyId P Tq))
                   (*-monoʳ-≤ (aq (search j Tq Tr)) (m≤m+n Tq Tr)))))

------------------------------------------------------------------------
-- Non-vacuity: a trivial runner (always halts, checker always accepts) fires
-- `finishing` -- so some stage `k` has `done k = true` with the Levin bound.
------------------------------------------------------------------------

private
  module Ex where
    run₀ : ℕ → Exp → V → Maybe V
    run₀ _ _ _ = just ⟨⟩
    chk₀ : V → V → Bool
    chk₀ _ _ = true
    open Oracle run₀ chk₀ ⟨⟩
    _ : Σ ℕ (λ k → (done k ≡ true) × (gsum k < aq (search 0 1 0) * S (search 0 1 0)))
    _ = finishing 0 1 0 ⟨⟩ refl (λ b _ → refl) (s≤s z≤n)
