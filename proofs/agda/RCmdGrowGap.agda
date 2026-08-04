{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4, cost side, part 3): the BOUNDARY of
-- the affine traversal-charge law.
--
-- `RCmdDataCost` proved the data walk affine,
-- `dcostD c s ≤ (1 + N) · ocostD c s`, and recorded in its header that the
-- law holds because every atom there is SIZE-PRESERVING, so the data bound
-- N is a run invariant.  It also recorded that the law must FAIL for a
-- size-changing atom, citing the paper's (D1″): k copies of `X := (X.X)`
-- allocate 2^k nodes on k commands, which is why the fuel of the timed
-- interpreter is node-granular rather than per-command.  That direction was
-- an English remark with a one-atom illustration.  Here it is a theorem.
--
-- The growing atom is `double v = v ∙ v`.  It is a legitimate reversible
-- replacement, not a cheat: `double-inj` shows it is injective, so it
-- computes a partial injective function exactly as (A1) requires; what it
-- is not is size-preserving.
--
-- `pow k` is k+1 doublings in sequence.  Its unit cost is linear,
-- `ocostG (pow k) v ≡ suc (k + k)` (`ocost-pow`), while the value it
-- leaves is exponential, `2 ^ k ≤ suc (sizeV (run (pow k) v))`
-- (`pow-size`).  Running ONE more atom on that value then costs
--
--     ∀ K → Σ[ w ] (K · ocostG g w < dcostG g w)   (`no-affine-bound`)
--
-- a single command, of unit cost 1, whose data charge exceeds any constant
-- multiple of it, on a value the language itself reaches in K steps.
-- Stating it at one atom is what keeps the arithmetic input down to
-- `n < 2 ^ n`; the sequence-level version follows but needs an asymptotic
-- comparison that adds nothing to the point.
--
-- No constant, however large, makes the data charge affine in the unit
-- cost once atoms may grow their data.  Together with `RCmdDataCost`'s
-- `tcostD-affine` this brackets the traversal charge exactly: affine for
-- size-preserving atoms, unboundedly worse than affine as soon as one atom
-- is not.  That bracket is the formal content of (D1″).
--
-- --safe, axiom-free; existing modules untouched.
------------------------------------------------------------------------

module RCmdGrowGap where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-trans; ≤-refl; <-transʳ; +-comm; +-suc; +-identityʳ;
         *-comm; *-identityʳ; +-mono-≤; ≤-step; m≤m+n; m≤n+m; n≤1+n)
open import Data.Product using (Σ; _,_; Σ-syntax)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; cong₂; subst)
open import UniversalInterp using (V; ⟨⟩; _∙_)
open import RCmdDataCost using (sizeV)

------------------------------------------------------------------------
-- the growing atom: injective (so it is a legal reversible replacement)
-- but not size-preserving (so the affine law cannot survive it).

double : V → V
double v = v ∙ v

double-inj : (v w : V) → double v ≡ double w → v ≡ w
double-inj v .v refl = refl

double-size : (v : V) → sizeV (double v) ≡ suc (sizeV v + sizeV v)
double-size v = refl

------------------------------------------------------------------------
-- a one-value command language with sequencing and that atom only.

data Grow : Set where
  g    : Grow
  _⨟_  : Grow → Grow → Grow

infixl 5 _⨟_

run : Grow → V → V
run g       v = double v
run (c ⨟ d) v = run d (run c v)

-- fair unit cost: 1 per executed node, exactly as `RCmdDataCost.ocostD`
ocostG : Grow → V → ℕ
ocostG g       v = 1
ocostG (c ⨟ d) v = 1 + ocostG c v + ocostG d (run c v)

-- the data charge: an atom pays for every node of the value it walks
dcostG : Grow → V → ℕ
dcostG g       v = suc (sizeV v)
dcostG (c ⨟ d) v = 1 + dcostG c v + dcostG d (run c v)

pow : ℕ → Grow
pow zero    = g
pow (suc k) = pow k ⨟ g

------------------------------------------------------------------------
-- the unit cost is linear in k.

ocost-pow : (k : ℕ) (v : V) → ocostG (pow k) v ≡ suc (k + k)
ocost-pow zero    v = refl
ocost-pow (suc k) v rewrite ocost-pow k v =
  cong (λ z → suc (suc z)) (trans (+-comm (k + k) 1) (sym (+-suc k k)))

------------------------------------------------------------------------
-- the data grows exponentially: 2 ^ k ≤ suc (size after k+1 doublings).

size-double : (v : V) → suc (sizeV v) + suc (sizeV v) ≡ suc (sizeV (double v))
size-double v = cong suc (+-suc (sizeV v) (sizeV v))

pow-size : (k : ℕ) (v : V) → 2 ^ k ≤ suc (sizeV (run (pow k) v))
pow-size zero v = s≤s z≤n
pow-size (suc k) v = step
  where
  ih : 2 ^ k ≤ suc (sizeV (run (pow k) v))
  ih = pow-size k v
  w : V
  w = run (pow k) v
  dbl : 2 ^ k + 2 ^ k ≤ suc (sizeV w) + suc (sizeV w)
  dbl = +-mono-≤ ih ih
  step : 2 ^ suc k ≤ suc (sizeV (run (pow (suc k)) v))
  step = subst (λ z → 2 ^ suc k ≤ z) (size-double w)
               (subst (λ z → z ≤ suc (sizeV w) + suc (sizeV w))
                      (sym (two-pow k)) dbl)
    where
    two-pow : (k : ℕ) → 2 ^ suc k ≡ 2 ^ k + 2 ^ k
    two-pow k = cong (2 ^ k +_) (+-identityʳ (2 ^ k))

------------------------------------------------------------------------
-- the only arithmetic input: 2 ^ n outgrows n.  (stdlib has no n<2^n.)

1≤2^ : (n : ℕ) → 1 ≤ 2 ^ n
1≤2^ zero    = ≤-refl
1≤2^ (suc n) =
  ≤-trans (1≤2^ n)
          (subst (2 ^ n ≤_) (sym (two-pow n)) (m≤m+n (2 ^ n) (2 ^ n)))
  where
  two-pow : (k : ℕ) → 2 ^ suc k ≡ 2 ^ k + 2 ^ k
  two-pow k = cong (2 ^ k +_) (+-identityʳ (2 ^ k))

n<2^n : (n : ℕ) → n < 2 ^ n
n<2^n zero    = ≤-refl
n<2^n (suc n) =
  subst (suc (suc n) ≤_) (sym (two-pow n)) (+-mono-≤ (1≤2^ n) (n<2^n n))
  where
  two-pow : (k : ℕ) → 2 ^ suc k ≡ 2 ^ k + 2 ^ k
  two-pow k = cong (2 ^ k +_) (+-identityʳ (2 ^ k))

unsuc : {m n : ℕ} → suc m ≤ suc n → m ≤ n
unsuc (s≤s p) = p

------------------------------------------------------------------------
-- THE THEOREM: one atom, unit cost 1, data charge above any constant.

grow-gap : (K : ℕ) (v : V)
  → K * ocostG g (run (pow K) v) < dcostG g (run (pow K) v)
grow-gap K v = s≤s (subst (_≤ sizeV (run (pow K) v))
                          (sym (*-identityʳ K))
                          (unsuc (≤-trans (n<2^n K) (pow-size K v))))

no-affine-bound : (K : ℕ) (v : V)
  → Σ[ w ∈ V ] (K * ocostG g w < dcostG g w)
no-affine-bound K v = run (pow K) v , grow-gap K v

------------------------------------------------------------------------
-- Test-first: the gap at small constants, and the atom's legitimacy.

module Examples where
  x : V
  x = ⟨⟩

  ex-inj : (v w : V) → double v ≡ double w → v ≡ w
  ex-inj = double-inj

  ex-ocost : ocostG (pow 3) x ≡ 7          -- linear: suc (3 + 3)
  ex-ocost = refl

  ex-size : sizeV (run (pow 3) x) ≡ 15     -- exponential: 2^4 - 1
  ex-size = refl

  -- one atom on that value pays 16 while its unit cost is 1
  ex-one-atom : dcostG g (run (pow 3) x) ≡ 16
  ex-one-atom = refl

  ex-unit : ocostG g (run (pow 3) x) ≡ 1
  ex-unit = refl

  ex-gap : 3 * ocostG g (run (pow 3) x) < dcostG g (run (pow 3) x)
  ex-gap = grow-gap 3 x
