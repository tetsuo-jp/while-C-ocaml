{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4, cost side, part 2): the OTHER half of
-- the traversal charge -- the Θ(|v|) DATA walk.
--
-- `RCmdVWalkCost` charged the length-L VARIABLE-list walk and proved it
-- affine: `wcostV c s ≤ (1 + L) · ocostV c s`.  main.tex's cost convention
-- has a second component ("a primitive that walks data is charged the size
-- of the data it walks"; pattern-build/match, ⊙-set/clear, dup/eq are each
-- Θ(|v|)), and the paper's mechanization paragraph records it as NOT yet
-- modelled -- for a good reason: `RCmdV`'s atoms are `swpV` (swap the root's
-- two subtrees) and `ifnV` (test the root for nil), whose data work is
-- genuinely O(1).  There was nothing of size Θ(|v|) to charge.
--
-- This module supplies one.  `mirD k` deep-mirrors variable k's value
-- (`mirror (a ∙ b) = mirror b ∙ mirror a`): the smallest reversible atom
-- that must visit EVERY node.  It is self-inverse (`mirror-invol`),
-- nil-preserving (`mirror-nil`, so the `ifnD` guard still survives
-- inversion and `inv-soundD` goes through) and size-preserving
-- (`mirror-size`, so the data bound N is a run invariant just as L is).
-- `swpD` is kept alongside it precisely to keep the CONTRAST inside one
-- language: an O(1)-data atom and a Θ(|v|)-data atom, charged by the same
-- cost function.
--
-- Two laws, both with the walks CHARGED rather than assumed:
--
--     dcostD c s ≤ (1 + N) · ocostD c s          N = storeMax s   (data only)
--     tcostD c s ≤ (1 + L + N) · ocostD c s      L = length s     (both)
--
-- `tcostD-affine` is the object-level traversal charge of §cost-model in
-- full: dispatch, the variable-list walk, and the data walk, in one affine
-- constant.  At N = 0 it collapses to `RCmdVWalkCost.a-revW L = 1 + L`
-- (`ex-N0-is-walk`), and at L = N = 0 to 1, the O(1)-dispatch driver-level
-- constant -- so the existing constants are the degenerate fibres of this one.
--
-- SCOPE (stated because it is the interesting boundary).  The affine law
-- holds here because every atom is SIZE-PRESERVING, so N is invariant.  It
-- would FAIL for a size-changing atom: main.tex (D1″) notes that k copies of
-- `X := (X.X)` allocate 2^k nodes on k commands, so a data-charged cost is
-- not within any constant factor of a per-command count.  That is exactly
-- why the paper's fuel is node-granular.  `ex-growth-gap` below exhibits the
-- gap concretely (mirroring one big value: unit cost 1, data cost |v|), which
-- is the same phenomenon in miniature.  Extending to size-changing atoms is
-- what a data-charged INTERPRETER bound would have to face.
--
-- --safe, axiom-free; existing modules untouched.
------------------------------------------------------------------------

module RCmdDataCost where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _⊔_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-trans; +-mono-≤; +-comm; m≤m⊔n; m≤n⊔m; m≤m+n)
open import Data.Bool using (Bool; true; false)
open import Data.List using (List; []; _∷_; length)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; cong₂; subst)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevStore
  using (Store; lookup; lookupC; lookupC-bound;
         mapAt; mapAtC; mapAtC-bound; mapAt-invol; mapAt-length; eqℕ)
open import RevLinearOverhead using (lin-leaf; lin-un; lin-bin)
open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; aq; c; S)
import RevLevinAbstract as RLA
import RevInst as R

------------------------------------------------------------------------
-- the size-Θ(|v|) reversible atom: deep mirror.

mirror : V → V
mirror ⟨⟩      = ⟨⟩
mirror (a ∙ b) = mirror b ∙ mirror a

mirror-invol : (v : V) → mirror (mirror v) ≡ v
mirror-invol ⟨⟩      = refl
mirror-invol (a ∙ b) = cong₂ _∙_ (mirror-invol a) (mirror-invol b)

mirror-nil : (v : V) → R.isNil (mirror v) ≡ R.isNil v
mirror-nil ⟨⟩      = refl
mirror-nil (a ∙ b) = refl

-- the same fact for the O(1) atom (RCmdV proves its own copy; kept local so
-- this module stays a leaf).
swapTop-nil : (v : V) → R.isNil (R.swapTop v) ≡ R.isNil v
swapTop-nil ⟨⟩      = refl
swapTop-nil (a ∙ b) = refl

------------------------------------------------------------------------
-- data size = node count; both atoms preserve it, so N is a run invariant.

sizeV : V → ℕ
sizeV ⟨⟩      = 0
sizeV (a ∙ b) = suc (sizeV a + sizeV b)

mirror-size : (v : V) → sizeV (mirror v) ≡ sizeV v
mirror-size ⟨⟩      = refl
mirror-size (a ∙ b) =
  cong suc (trans (cong₂ _+_ (mirror-size b) (mirror-size a))
                  (+-comm (sizeV b) (sizeV a)))

swapTop-size : (v : V) → sizeV (R.swapTop v) ≡ sizeV v
swapTop-size ⟨⟩      = refl
swapTop-size (a ∙ b) = cong suc (+-comm (sizeV b) (sizeV a))

storeMax : Store → ℕ
storeMax []            = 0
storeMax ((k , v) ∷ s) = sizeV v ⊔ storeMax s

lookup-size : (k : ℕ) (s : Store) → sizeV (lookup k s) ≤ storeMax s
lookup-size k []             = z≤n
lookup-size k ((k' , v) ∷ s) with eqℕ k k'
... | true  = m≤m⊔n (sizeV v) (storeMax s)
... | false = ≤-trans (lookup-size k s) (m≤n⊔m (sizeV v) (storeMax s))

mapAt-max : (f : V → V) → ((v : V) → sizeV (f v) ≡ sizeV v)
          → (k : ℕ) (s : Store) → storeMax (mapAt f k s) ≡ storeMax s
mapAt-max f fs k []             = refl
mapAt-max f fs k ((k' , v) ∷ s) with eqℕ k k'
... | true  = cong (_⊔ storeMax s) (fs v)
... | false = cong (sizeV v ⊔_) (mapAt-max f fs k s)

------------------------------------------------------------------------
-- syntax, semantics, inversion.  `mirD` is the Θ(|v|) atom; `swpD` the O(1)
-- one, kept for contrast.

data RCmdD : Set where
  swpD : ℕ → RCmdD
  mirD : ℕ → RCmdD
  sqD  : RCmdD → RCmdD → RCmdD
  ifnD : ℕ → RCmdD → RCmdD → RCmdD

fwdD : RCmdD → Store → Store
fwdD (swpD k)     s = mapAt R.swapTop k s
fwdD (mirD k)     s = mapAt mirror k s
fwdD (sqD c d)    s = fwdD d (fwdD c s)
fwdD (ifnD k c d) s = ifnil (lookup k s) (fwdD c s) (fwdD d s)

invD : RCmdD → RCmdD
invD (swpD k)     = swpD k
invD (mirD k)     = mirD k
invD (sqD c d)    = sqD (invD d) (invD c)
invD (ifnD k c d) = ifnD k (invD c) (invD d)

invD-invol : (c : RCmdD) → invD (invD c) ≡ c
invD-invol (swpD k)     = refl
invD-invol (mirD k)     = refl
invD-invol (sqD c d)    = cong₂ sqD (invD-invol c) (invD-invol d)
invD-invol (ifnD k c d) = cong₂ (ifnD k) (invD-invol c) (invD-invol d)

------------------------------------------------------------------------
-- nil-preservation (generic in the atom), hence the `ifnD` guard survives.

lookup-mapAt-nil : (f : V → V) → ((v : V) → R.isNil (f v) ≡ R.isNil v)
  → (j k : ℕ) (s : Store)
  → R.isNil (lookup j (mapAt f k s)) ≡ R.isNil (lookup j s)
lookup-mapAt-nil f fn j k []             = refl
lookup-mapAt-nil f fn j k ((k' , v) ∷ s) with eqℕ k k'
... | true  with eqℕ j k'
...   | true  = fn v
...   | false = refl
lookup-mapAt-nil f fn j k ((k' , v) ∷ s) | false with eqℕ j k'
...   | true  = refl
...   | false = lookup-mapAt-nil f fn j k s

preservesNilD : (c : RCmdD) (j : ℕ) (s : Store)
  → R.isNil (lookup j (fwdD c s)) ≡ R.isNil (lookup j s)
preservesNilD (swpD k)     j s = lookup-mapAt-nil R.swapTop swapTop-nil j k s
preservesNilD (mirD k)     j s = lookup-mapAt-nil mirror mirror-nil j k s
preservesNilD (sqD c d)    j s =
  trans (preservesNilD d j (fwdD c s)) (preservesNilD c j s)
preservesNilD (ifnD k c d) j s with lookup k s
... | ⟨⟩    = preservesNilD c j s
... | _ ∙ _ = preservesNilD d j s

------------------------------------------------------------------------
-- inversion is sound (same shape as RCmdV.inv-soundV, one extra atom).

inv-soundD : (c : RCmdD) (s : Store) → fwdD (invD c) (fwdD c s) ≡ s
inv-soundD (swpD k)  s = mapAt-invol R.swapTop R.swapTop-invol k s
inv-soundD (mirD k)  s = mapAt-invol mirror mirror-invol k s
inv-soundD (sqD c d) s =
  trans (cong (fwdD (invD c)) (inv-soundD d (fwdD c s))) (inv-soundD c s)
inv-soundD (ifnD k c d) s with lookup k s in eq
... | ⟨⟩ =
  trans (R.ifnil-then (lookup k (fwdD c s))
                      (fwdD (invD c) (fwdD c s)) (fwdD (invD d) (fwdD c s))
                      (trans (preservesNilD c k s) (cong R.isNil eq)))
        (inv-soundD c s)
... | (a ∙ b) =
  trans (R.ifnil-else (lookup k (fwdD d s))
                      (fwdD (invD c) (fwdD d s)) (fwdD (invD d) (fwdD d s))
                      (trans (preservesNilD d k s) (cong R.isNil eq)))
        (inv-soundD d s)

------------------------------------------------------------------------
-- run invariants: neither L nor N changes during a run.

fwdD-length : (c : RCmdD) (s : Store) → length (fwdD c s) ≡ length s
fwdD-length (swpD k)     s = mapAt-length R.swapTop k s
fwdD-length (mirD k)     s = mapAt-length mirror k s
fwdD-length (sqD c d)    s = trans (fwdD-length d (fwdD c s)) (fwdD-length c s)
fwdD-length (ifnD k c d) s with lookup k s
... | ⟨⟩    = fwdD-length c s
... | _ ∙ _ = fwdD-length d s

fwdD-max : (c : RCmdD) (s : Store) → storeMax (fwdD c s) ≡ storeMax s
fwdD-max (swpD k)     s = mapAt-max R.swapTop swapTop-size k s
fwdD-max (mirD k)     s = mapAt-max mirror mirror-size k s
fwdD-max (sqD c d)    s = trans (fwdD-max d (fwdD c s)) (fwdD-max c s)
fwdD-max (ifnD k c d) s with lookup k s
... | ⟨⟩    = fwdD-max c s
... | _ ∙ _ = fwdD-max d s

------------------------------------------------------------------------
-- costs: fair unit cost, the data charge alone, and the full charge.

ocostD : RCmdD → Store → ℕ
ocostD (swpD k)     s = 1
ocostD (mirD k)     s = 1
ocostD (sqD c d)    s = 1 + ocostD c s + ocostD d (fwdD c s)
ocostD (ifnD k c d) s = 1 + ifnil (lookup k s) (ocostD c s) (ocostD d s)

-- data charge only: `mirD` pays every node of the value it walks.
dcostD : RCmdD → Store → ℕ
dcostD (swpD k)     s = 1
dcostD (mirD k)     s = suc (sizeV (lookup k s))
dcostD (sqD c d)    s = 1 + dcostD c s + dcostD d (fwdD c s)
dcostD (ifnD k c d) s = 1 + ifnil (lookup k s) (dcostD c s) (dcostD d s)

-- the full traversal charge: dispatch + variable-list walk + data walk.
tcostD : RCmdD → Store → ℕ
tcostD (swpD k)     s = suc (mapAtC k s)
tcostD (mirD k)     s = suc (mapAtC k s + sizeV (lookup k s))
tcostD (sqD c d)    s = 1 + tcostD c s + tcostD d (fwdD c s)
tcostD (ifnD k c d) s =
  suc (lookupC k s) + ifnil (lookup k s) (tcostD c s) (tcostD d s)

------------------------------------------------------------------------
-- THE THEOREMS.

a-revD : ℕ → ℕ
a-revD N = suc N

dcostD-affine : (c : RCmdD) (s : Store)
              → dcostD c s ≤ a-revD (storeMax s) * ocostD c s
dcostD-affine (swpD k) s = lin-leaf {K = a-revD (storeMax s)} (s≤s z≤n)
dcostD-affine (mirD k) s = lin-leaf {K = a-revD (storeMax s)} (s≤s (lookup-size k s))
dcostD-affine (sqD c d) s =
  lin-bin {κ = 1} {a = dcostD c s} {b = dcostD d (fwdD c s)}
          {x = ocostD c s} {y = ocostD d (fwdD c s)} {K = a-revD (storeMax s)}
          (s≤s z≤n) (dcostD-affine c s)
          (subst (λ n → dcostD d (fwdD c s) ≤ suc n * ocostD d (fwdD c s))
                 (fwdD-max c s) (dcostD-affine d (fwdD c s)))
dcostD-affine (ifnD k c d) s with lookup k s
... | ⟨⟩    = lin-un {K = a-revD (storeMax s)} (s≤s z≤n) (dcostD-affine c s)
... | _ ∙ _ = lin-un {K = a-revD (storeMax s)} (s≤s z≤n) (dcostD-affine d s)

a-revLN : ℕ → ℕ → ℕ
a-revLN L N = suc (L + N)

tcostD-affine : (c : RCmdD) (s : Store)
              → tcostD c s ≤ a-revLN (length s) (storeMax s) * ocostD c s
tcostD-affine (swpD k) s =
  lin-leaf {K = a-revLN (length s) (storeMax s)}
           (s≤s (≤-trans (mapAtC-bound k s) (m≤m+n (length s) (storeMax s))))
tcostD-affine (mirD k) s =
  lin-leaf {K = a-revLN (length s) (storeMax s)}
           (s≤s (+-mono-≤ (mapAtC-bound k s) (lookup-size k s)))
tcostD-affine (sqD c d) s =
  lin-bin {κ = 1} {a = tcostD c s} {b = tcostD d (fwdD c s)}
          {x = ocostD c s} {y = ocostD d (fwdD c s)}
          {K = a-revLN (length s) (storeMax s)}
          (s≤s z≤n) (tcostD-affine c s)
          (subst (λ l → tcostD d (fwdD c s)
                      ≤ suc (l + storeMax s) * ocostD d (fwdD c s))
                 (fwdD-length c s)
            (subst (λ n → tcostD d (fwdD c s)
                        ≤ suc (length (fwdD c s) + n) * ocostD d (fwdD c s))
                   (fwdD-max c s) (tcostD-affine d (fwdD c s))))
tcostD-affine (ifnD k c d) s with lookup k s
... | ⟨⟩    = lin-un {K = a-revLN (length s) (storeMax s)}
                     (s≤s (≤-trans (lookupC-bound k s)
                                   (m≤m+n (length s) (storeMax s))))
                     (tcostD-affine c s)
... | _ ∙ _ = lin-un {K = a-revLN (length s) (storeMax s)}
                     (s≤s (≤-trans (lookupC-bound k s)
                                   (m≤m+n (length s) (storeMax s))))
                     (tcostD-affine d s)

-- grounding proposition at fixed (L, N), the RevLevinGrounded twin.
DataOverhead : ℕ → ℕ → Set
DataOverhead L N = (c : RCmdD) (s : Store) → length s ≡ L → storeMax s ≡ N
                 → tcostD c s ≤ a-revLN L N * ocostD c s

data-grounded : (L N : ℕ) → DataOverhead L N
data-grounded L N c s eqL eqN =
  subst (λ l → tcostD c s ≤ suc (l + N) * ocostD c s) eqL
    (subst (λ n → tcostD c s ≤ suc (length s + n) * ocostD c s) eqN
           (tcostD-affine c s))

------------------------------------------------------------------------
-- Layer-A hookup: `levin-optimal` is parametric in the overhead field, so
-- the optimality inequality fires verbatim at the FULL traversal charge.

rev-search-data : (L N j Tq Tr : ℕ) → LevinSearch
rev-search-data L N j Tq Tr = RLA.RevSearch.rev-search (a-revLN L N) j Tq Tr

overhead-is-data : (L N j Tq Tr : ℕ)
  → LevinSearch.overhead (rev-search-data L N j Tq Tr) ≡ a-revLN L N
overhead-is-data _ _ _ _ _ = refl

rev-levin-data : (L N j Tq Tr : ℕ)
  → 1 ≤ c (rev-search-data L N j Tq Tr) * S (rev-search-data L N j Tq Tr)
  → Σ ℕ (λ k → gsum k < aq (rev-search-data L N j Tq Tr)
                        * S (rev-search-data L N j Tq Tr))
rev-levin-data L N j Tq Tr = RLA.RevSearch.rev-levin-optimal (a-revLN L N) j Tq Tr

------------------------------------------------------------------------
-- Test-first: the contrast, the bounds, and the degenerate fibres.

module Examples where
  -- a skewed 3-node value in variable 0, nil in variable 1: L = 2, N = 3.
  big : V
  big = (⟨⟩ ∙ ⟨⟩) ∙ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩)

  s1 : Store
  s1 = (0 , big) ∷ (1 , ⟨⟩) ∷ []

  ex-size : sizeV big ≡ 4
  ex-size = refl

  ex-L : length s1 ≡ 2
  ex-L = refl

  ex-N : storeMax s1 ≡ 4
  ex-N = refl

  -- mirror really is a deep walk: it reverses at every level.
  ex-mirror : mirror big ≡ (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ∙ (⟨⟩ ∙ ⟨⟩)
  ex-mirror = refl

  ex-mirror-invol : mirror (mirror big) ≡ big
  ex-mirror-invol = mirror-invol big

  -- THE CONTRAST, in one language: both atoms are ONE executed node, but
  -- the O(1) atom pays 1 of data work and the Θ(|v|) atom pays |v|.
  ex-unit-swp : ocostD (swpD 0) s1 ≡ 1
  ex-unit-swp = refl

  ex-unit-mir : ocostD (mirD 0) s1 ≡ 1
  ex-unit-mir = refl

  ex-data-swp : dcostD (swpD 0) s1 ≡ 1
  ex-data-swp = refl

  ex-growth-gap : dcostD (mirD 0) s1 ≡ 5      -- 1 dispatch + 4 nodes walked
  ex-growth-gap = refl

  -- the data law, and the full law, on a real two-atom program.
  prog : RCmdD
  prog = sqD (mirD 0) (ifnD 1 (swpD 0) (mirD 0))

  ex-rev : fwdD (invD prog) (fwdD prog s1) ≡ s1
  ex-rev = inv-soundD prog s1

  ex-ocost : ocostD prog s1 ≡ 4
  ex-ocost = refl

  ex-dcost : dcostD prog s1 ≡ 8
  ex-dcost = refl

  ex-tcost : tcostD prog s1 ≡ 12
  ex-tcost = refl

  ex-data-bound : dcostD prog s1 ≤ a-revD (storeMax s1) * ocostD prog s1
  ex-data-bound = dcostD-affine prog s1        -- 8 ≤ 5·4 = 20

  ex-full-bound : tcostD prog s1 ≤ a-revLN (length s1) (storeMax s1) * ocostD prog s1
  ex-full-bound = tcostD-affine prog s1        -- 12 ≤ 7·4 = 28

  -- degenerate fibres: the existing constants are special cases of this one.
  -- N = 0 (all values nil) recovers RCmdVWalkCost's a-revW L = 1 + L.
  ex-N0-is-walk : (L : ℕ) → a-revLN L 0 ≡ suc L
  ex-N0-is-walk zero    = refl
  ex-N0-is-walk (suc L) = cong suc (ex-N0-is-walk L)

  -- L = N = 0 collapses to 1: the O(1)-dispatch driver-level constant.
  ex-LN0 : a-revLN 0 0 ≡ 1
  ex-LN0 = refl

  -- slope in each argument: one more variable, or one more data node,
  -- costs exactly one more walk step per access.
  ex-slope-N : (L N : ℕ) → a-revLN L (suc N) ≡ suc (a-revLN L N)
  ex-slope-N zero    N = refl
  ex-slope-N (suc L) N = cong suc (ex-slope-N L N)

  ex-slope-L : (L N : ℕ) → a-revLN (suc L) N ≡ suc (a-revLN L N)
  ex-slope-L L N = refl
