{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- LEVIN'S OPTIMALITY -- the budget-doubling core (toward Jones Thm 20.1.2
-- `thm-levin`).
--
-- Levin's optimal search `opt` dovetails ALL programs, running program `i` under
-- a TIME BUDGET that doubles across phases, simulated by an EFFICIENT interpreter
-- (here the one-variable I self-interpreter `SelfInterp.sint`, which runs an
-- encoded program with only a constant per-step overhead).  Its optimality
-- `time_opt(x) ≤ a_q · (time_q(x) + time_r(...))` rests on two quantitative
-- facts; this module machine-checks the first (the doubling core):
--
--   THE TOTAL COST OF ALL PHASES `0..k` IS LESS THAN TWICE THE LAST PHASE.
--
-- i.e. `Σ_{i=0}^{k} 2^i = 2^{k+1} − 1 < 2·2^k`.  So once the optimal solver `q`
-- finishes in some phase `k` (when the budget `2^k` first exceeds what `q` needs),
-- the WHOLE dovetailed search up to that phase has cost < 2·2^k -- a constant
-- factor over `q`'s phase budget.  Combined with (i) the efficient interpreter's
-- constant per-step overhead and (ii) the doubling-overshoot bound (the least
-- `2^k ≥ N` is `< 2N` -- the companion fact), this yields Levin's constant `a_q`.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/BOOK-ROADMAP.md (Thm 20.1.2).
------------------------------------------------------------------------

module LevinBudget where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-suc; +-mono-≤; m≤m+n; m≤n+m; m≤n⇒m≤1+n; <⇒≱; <-cmp;
         *-identityˡ; *-distribʳ-+)
open import Data.Product using (Σ; _×_; _,_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.Definitions using (tri<; tri≈; tri>)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; sym; trans; subst)

------------------------------------------------------------------------
-- powers of two (by doubling, no multiplication needed) and the running total.
------------------------------------------------------------------------

pow2 : ℕ → ℕ
pow2 zero    = 1
pow2 (suc k) = pow2 k + pow2 k          -- 2^(k+1) = 2^k + 2^k

-- gsum k = Σ_{i=0}^{k} 2^i  (total budget spent over phases 0..k).
gsum : ℕ → ℕ
gsum zero    = 1
gsum (suc k) = pow2 (suc k) + gsum k

------------------------------------------------------------------------
-- Exact geometric sum: Σ_{i=0}^{k} 2^i = 2^{k+1} − 1.
------------------------------------------------------------------------

gsum-eq : ∀ k → suc (gsum k) ≡ pow2 (suc k)
gsum-eq zero    = refl
gsum-eq (suc k) =
  trans (sym (+-suc (pow2 (suc k)) (gsum k)))
        (cong (pow2 (suc k) +_) (gsum-eq k))

------------------------------------------------------------------------
-- THE DOUBLING CORE: the whole search up to phase k costs strictly less than
-- twice the last phase's budget (`pow2 (suc k) = 2 · pow2 k`).
------------------------------------------------------------------------

gsum<2last : ∀ k → gsum k < pow2 (suc k)
gsum<2last k = subst (λ m → gsum k < m) (gsum-eq k) ≤-refl

------------------------------------------------------------------------
-- THE OVERSHOOT (companion): the least power of two ≥ N is ≤ 2·N, so the phase
-- whose budget first covers the optimal solver `q` overshoots `q`'s need by at
-- most a factor 2.  Together with `gsum<2last` this gives Levin's constant `a_q`.
------------------------------------------------------------------------

private
  sm≤ssm : ∀ m → suc m ≤ suc (suc m)
  sm≤ssm m = m≤n⇒m≤1+n ≤-refl

  dbl-mono : ∀ {a b} → a ≤ b → a + a ≤ b + b
  dbl-mono a≤b = +-mono-≤ a≤b a≤b

  ssm≤dbl : ∀ m → suc (suc m) ≤ suc m + suc m
  ssm≤dbl m = s≤s (m≤n+m (suc m) m)

-- for N = suc n ≥ 1, some power of two lies in [N , 2·N].
overshoot : ∀ n → Σ ℕ (λ k → (suc n ≤ pow2 k) × (pow2 k ≤ suc n + suc n))
overshoot zero = zero , (s≤s z≤n , s≤s z≤n)
overshoot (suc m) with overshoot m
... | k' , sm≤pk , pk≤2sm with <-cmp (suc m) (pow2 k')
...   | tri< sm<pk _ _ = k' , sm<pk , ≤-trans pk≤2sm (dbl-mono (sm≤ssm m))
...   | tri> _ _ pk<sm = ⊥-elim (<⇒≱ pk<sm sm≤pk)
...   | tri≈ _ sm≡pk _ = suc k' , g1 , g2
        where
          g1 : suc (suc m) ≤ pow2 k' + pow2 k'
          g1 = subst (λ z → suc (suc m) ≤ z + z) sm≡pk (ssm≤dbl m)
          g2 : pow2 k' + pow2 k' ≤ suc (suc m) + suc (suc m)
          g2 = subst (λ z → z + z ≤ suc (suc m) + suc (suc m)) sm≡pk
                     (dbl-mono (sm≤ssm m))

------------------------------------------------------------------------
-- Monotonicity of pow2 and gsum (Levin B3: the operational finishing stage `k_op`
-- is only `≤` the abstract bound stage `k` -- a faster program may verify earlier --
-- so `gsum k_op ≤ gsum k` is what carries the optimality bound down to `k_op`).
------------------------------------------------------------------------

1≤pow2 : ∀ n → 1 ≤ pow2 n
1≤pow2 zero    = s≤s z≤n
1≤pow2 (suc n) = ≤-trans (1≤pow2 n) (m≤m+n (pow2 n) (pow2 n))

pow2-mono : ∀ {m n} → m ≤ n → pow2 m ≤ pow2 n
pow2-mono {n = n} z≤n = 1≤pow2 n
pow2-mono (s≤s m≤n)   = +-mono-≤ (pow2-mono m≤n) (pow2-mono m≤n)

1≤gsum : ∀ n → 1 ≤ gsum n
1≤gsum zero    = s≤s z≤n
1≤gsum (suc n) = ≤-trans (1≤gsum n) (m≤n+m (gsum n) (pow2 (suc n)))

gsum-mono : ∀ {m n} → m ≤ n → gsum m ≤ gsum n
gsum-mono {n = n} z≤n = 1≤gsum n
gsum-mono (s≤s m≤n)   = +-mono-≤ (pow2-mono (s≤s m≤n)) (gsum-mono m≤n)

-- pow2 is a homomorphism (+ ↦ *): 2^(m+n) = 2^m · 2^n  (Levin B3: stage k = j + d, so
-- pow2 k = pow2 j · pow2 d relates the solver's index and its budget offset).
pow2-+ : ∀ m n → pow2 (m + n) ≡ pow2 m * pow2 n
pow2-+ zero    n = sym (*-identityˡ (pow2 n))
pow2-+ (suc m) n =
  trans (cong (λ z → z + z) (pow2-+ m n))
        (sym (*-distribʳ-+ (pow2 n) (pow2 m) (pow2 m)))

-- `overshoot` phrased for a positive target `N ≥ 1`: some power of two lies in [N, 2N].
overshoot′ : ∀ N → 1 ≤ N → Σ ℕ (λ k → (N ≤ pow2 k) × (pow2 k ≤ N + N))
overshoot′ (suc n) _ = overshoot n
overshoot′ zero    ()

------------------------------------------------------------------------
-- Concrete check: phases 0..3 cost 1+2+4+8 = 15 = 2^4 − 1 < 16.
------------------------------------------------------------------------

_ : gsum 3 ≡ 15
_ = refl
