{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4, cost side): the TRAVERSAL CHARGE,
-- folded in -- the walk-charged cost of the multi-variable language `RCmdV`
-- is affine in the store size L, with the walks CHARGED, not assumed.
--
-- `RevStoreOverhead.AffineInL` (the abstract route) obtained
-- `a-rev(L) = base + 2L` by ASSUMING every per-op charge is ≤ base + 2L and
-- firing S2's generic lemma -- over `RCmd`, which has no variables.  This
-- module discharges that assumption over the real multi-variable language:
-- `wcostV` charges each executed `RCmdV` node its ACTUAL store walk
-- (`swpV k` pays the `mapAt` walk to variable k; `ifnV k` pays the `lookup`
-- walk to its guard; `sqV` pays dispatch only), the walk lengths taken from
-- `RevStore`'s cost-instrumented operations at the store the run actually
-- reaches.  The paper's traversal-charged convention (main.tex
-- §cost-model: every variable access walks the length-L variable list) is
-- thus a definition here, and the affine bound a THEOREM:
--
--     wcostV c s ≤ (1 + L) · ocostV c s ,   L = length s ,
--
-- (`wcostV-affine`), using `fwdV-length` (a run never changes L, so the
-- constant is stable).  Each `RCmdV` op makes exactly ONE walk, hence
-- 1 + L here; a read+write op costs ≤ 1 + 2L and stays inside the
-- `RevStoreOverhead` envelope.  At L = 0 the constant collapses to 1 --
-- exactly the O(1)-dispatch driver-level constant the paper reports
-- (`ex-L0`); the measured ≈364× of `ri.rwhile` is this law read at L ≈ 75
-- with a larger per-access unit (`RevCostReconcile`).
--
-- Feeding a-revW(L) = 1 + L into Layer A (parametric in the overhead field,
-- so it fires verbatim -- no re-verification) gives reversible Levin
-- optimality UNDER THE TRAVERSAL CHARGE: `rev-levin-walk`, with
-- `gsum k < 4·(1+L)·2^j·(Tq+Tr)`.  What remains of S4 proper is the
-- interpreter side: an encoding `⌜_⌝V : RCmdV → V` and a store-carrying
-- self-interpreter `usintV` whose OWN step count is bounded by
-- a-revW(L) · ocostV -- this module pins the constant that bound targets.
-- --safe, axiom-free.
------------------------------------------------------------------------

module RCmdVWalkCost where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _<_; z≤n; s≤s)
open import Data.List using (List; []; _∷_; length)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; subst)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevStore
  using (Store; lookup; lookupC; lookupC-bound;
         mapAt; mapAtC; mapAtC-bound; mapAt-length)
open import RCmdV using (RCmdV; swpV; sqV; ifnV; fwdV; ocostV)
open import RevLinearOverhead using (lin-leaf; lin-un; lin-bin)
open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; aq; c; S)
import RevLevinAbstract as RLA
import RevInst as R

------------------------------------------------------------------------
-- L is a run invariant: no RCmdV command changes the store length.
-- (This is what makes "the" L of a search probe well-defined.)

fwdV-length : (c : RCmdV) (s : Store) → length (fwdV c s) ≡ length s
fwdV-length (swpV k)     s = mapAt-length R.swapTop k s
fwdV-length (sqV c d)    s = trans (fwdV-length d (fwdV c s)) (fwdV-length c s)
fwdV-length (ifnV k c d) s with lookup k s
... | ⟨⟩    = fwdV-length c s
... | _ ∙ _ = fwdV-length d s

------------------------------------------------------------------------
-- the walk-charged cost: 1 dispatch per executed node PLUS the actual
-- variable-list walk of that node's store access, at the actual store.

wcostV : RCmdV → Store → ℕ
wcostV (swpV k)     s = suc (mapAtC k s)
wcostV (sqV c d)    s = 1 + wcostV c s + wcostV d (fwdV c s)
wcostV (ifnV k c d) s = suc (lookupC k s) + ifnil (lookup k s) (wcostV c s) (wcostV d s)

------------------------------------------------------------------------
-- THE THEOREM: the traversal charge is affine in L -- walk-charged cost is
-- at most (1 + L) times the fair unit cost, L = store size, walks charged.

a-revW : ℕ → ℕ
a-revW L = suc L

wcostV-affine : (c : RCmdV) (s : Store) → wcostV c s ≤ a-revW (length s) * ocostV c s
wcostV-affine (swpV k) s = lin-leaf (s≤s (mapAtC-bound k s))
wcostV-affine (sqV c d) s =
  lin-bin {κ = 1} {a = wcostV c s} {b = wcostV d (fwdV c s)}
          {x = ocostV c s} {y = ocostV d (fwdV c s)} {K = a-revW (length s)}
          (s≤s z≤n) (wcostV-affine c s)
          (subst (λ l → wcostV d (fwdV c s) ≤ suc l * ocostV d (fwdV c s))
                 (fwdV-length c s) (wcostV-affine d (fwdV c s)))
wcostV-affine (ifnV k c d) s with lookup k s
... | ⟨⟩    = lin-un (s≤s (lookupC-bound k s)) (wcostV-affine c s)
... | _ ∙ _ = lin-un (s≤s (lookupC-bound k s)) (wcostV-affine d s)

-- the grounding proposition at a fixed store size L (the RevLevinGrounded
-- `LinearOverheadRev` twin, with the constant a FUNCTION of L).
WalkOverhead : ℕ → Set
WalkOverhead L = (c : RCmdV) (s : Store) → length s ≡ L
               → wcostV c s ≤ a-revW L * ocostV c s

walk-grounded : (L : ℕ) → WalkOverhead L
walk-grounded L c s eq =
  subst (λ l → wcostV c s ≤ suc l * ocostV c s) eq (wcostV-affine c s)

------------------------------------------------------------------------
-- Feed a-revW(L) into Layer A.  `LevinOptimal.levin-optimal` is parametric
-- in the overhead field, so the optimality inequality fires verbatim at the
-- traversal-charged constant: gsum k < 4·(1+L)·2^j·(Tq+Tr).

rev-search-walk : (L j Tq Tr : ℕ) → LevinSearch
rev-search-walk L j Tq Tr = RLA.RevSearch.rev-search (a-revW L) j Tq Tr

-- the overhead field is literally the traversal-charged constant.
overhead-is-walk : ∀ L j Tq Tr
  → LevinSearch.overhead (rev-search-walk L j Tq Tr) ≡ a-revW L
overhead-is-walk _ _ _ _ = refl

rev-levin-walk : ∀ L j Tq Tr
  → 1 ≤ c (rev-search-walk L j Tq Tr) * S (rev-search-walk L j Tq Tr)
  → Σ ℕ (λ k → gsum k < aq (rev-search-walk L j Tq Tr) * S (rev-search-walk L j Tq Tr))
rev-levin-walk L j Tq Tr = RLA.RevSearch.rev-levin-optimal (a-revW L) j Tq Tr

------------------------------------------------------------------------
-- Test-first: concrete walk counts, the bound, and the shape of a-revW.

module Examples where
  open RCmdV.Examples using (s0; prog)   -- L = 2, prog = sqV (swpV 0) (ifnV 1 …)

  -- walk-charged vs unit cost on the concrete two-variable run:
  -- swpV 0 pays walk 1, the ifnV 1 guard pays walk 2 (full list), etc.
  ex-wcost : wcostV prog s0 ≡ 8
  ex-wcost = refl

  ex-ocost : ocostV prog s0 ≡ 4
  ex-ocost = refl

  -- … and the affine bound holds (8 ≤ (1+2)·4 = 12).
  ex-bound : wcostV prog s0 ≤ a-revW (length s0) * ocostV prog s0
  ex-bound = wcostV-affine prog s0

  -- L = 0 fibre: the constant collapses to 1 -- the O(1)-dispatch
  -- driver-level constant of the paper's mechanization paragraph.
  ex-L0 : a-revW 0 ≡ 1
  ex-L0 = refl

  -- affinity as a slope check: one more variable costs exactly one more
  -- walk step per access.
  ex-slope : ∀ L → a-revW (suc L) ≡ suc (a-revW L)
  ex-slope L = refl

  -- L = 75 (the ri.rwhile Vl size): a-revW 75 = 76 -- the one-walk-per-op
  -- fibre of the measured ≈364× (read+write ops and a larger per-access
  -- unit account for the rest; see RevCostReconcile).
  ex-75 : a-revW 75 ≡ 76
  ex-75 = refl
