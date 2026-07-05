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
open import Data.Bool using (Bool; true; false; if_then_else_; _xor_)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm)
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

  ----------------------------------------------------------------------
  -- STEP COUNT (R1b): unit-cost executed-command count (one per atom, one
  -- per conditional dispatch), the twin of StepCount.evalT for a total
  -- semantics.  `fwdT` is the instrumented interpreter (store + count).
  ----------------------------------------------------------------------
  cost : Cmd → S → ℕ
  cost (atom u)     s = 1
  cost (seq c d)    s = cost c s + cost d (fwd c s)
  cost (cond p c d) s = suc (if p s then cost c s else cost d s)

  fwdT : Cmd → S → S × ℕ
  fwdT c s = (fwd c s , cost c s)

  -- the instrumented interpreter computes the same store (twin of evalT⇒eval)
  fwdT-value : (c : Cmd) (s : S) → proj₁ (fwdT c s) ≡ fwd c s
  fwdT-value c s = refl

  ----------------------------------------------------------------------
  -- REVERSIBLE TIME-SYMMETRY (R1b keystone): the backward run costs EXACTLY
  -- as many steps as the forward run.  This is the operational content behind
  -- "uncomputing is as cheap as computing" (Bennett), and the reason a-rev has
  -- a forward/backward-symmetric budget.  Proof mirrors `inv-sound`: the seq
  -- case flips the two summands (backward runs the parts in reverse order), so
  -- it closes by commutativity of +.
  ----------------------------------------------------------------------
  cost-sym : (c : Cmd) → WF c → ∀ s → cost (inv c) (fwd c s) ≡ cost c s
  cost-sym (atom u) _ s = refl
  cost-sym (seq c d) (wc , wd) s
    rewrite inv-sound d wd (fwd c s)
          | cost-sym d wd (fwd c s)
          | cost-sym c wc s = +-comm (cost d (fwd c s)) (cost c s)
  cost-sym (cond p c d) (wc , wd , pc , pd) s with p s in eqs
  ... | true  rewrite trans (pc s) eqs | cost-sym c wc s = refl
  ... | false rewrite trans (pd s) eqs | cost-sym d wd s = refl

  ----------------------------------------------------------------------
  -- BOUNDED REVERSIBLE ITERATION (R1a', bounded): repeat a body `c` exactly
  -- `n` times, as a DERIVED combinator (n nested `seq`s ending in `skip`).  Its
  -- reversibility and time-symmetry come FOR FREE from `inv-sound`/`cost-sym` on
  -- `seq`+`atom` -- the R1a/R1b substrate composes into loops with no new proof.
  -- (The data-dependent guard loop `from e do c until e'`, whose trip count is
  -- recovered on reversal via exit assertions, is R1a'-guard -- see RevLoop.)
  ----------------------------------------------------------------------
  skip : Cmd
  skip = atom ((λ s → s) , λ s → refl)

  iterC : ℕ → Cmd → Cmd
  iterC zero    c = skip
  iterC (suc n) c = seq c (iterC n c)

  iterC-wf : (n : ℕ) {c : Cmd} → WF c → WF (iterC n c)
  iterC-wf zero    _  = tt
  iterC-wf (suc n) wc = wc , iterC-wf n wc

  -- reversibility of the bounded loop, inherited from inv-sound
  iterC-sound : (n : ℕ) {c : Cmd} → WF c → ∀ s → bwd (iterC n c) (fwd (iterC n c) s) ≡ s
  iterC-sound n {c} wc = inv-sound (iterC n c) (iterC-wf n wc)

  -- time-symmetry of the bounded loop, inherited from cost-sym
  iterC-cost-sym : (n : ℕ) {c : Cmd} → WF c → ∀ s →
    cost (inv (iterC n c)) (fwd (iterC n c) s) ≡ cost (iterC n c) s
  iterC-cost-sym n {c} wc = cost-sym (iterC n c) (iterC-wf n wc)

------------------------------------------------------------------------
-- BENNETT (R1c, value level): realize the abstract input-preserving realization
-- (RevLevinAbstract.Bennett, garbage = input) as an actual REVERSIBLE PROGRAM, so
-- it inherits `cost` and `cost-sym`.  The reversible "copy" is R-WHILE's own
-- `out ^= f(in)`: over an output algebra with a self-inverse `⊕` (a GF(2) space),
-- it is an involution -- a legitimate atom -- BECAUSE the input `in` it reads is
-- kept, so running it again recomputes `f in` and cancels (Bennett's uncompute is
-- automatic here).  From a zeroed output it computes `(x , f x)`.
--
-- The output algebra is a parameter: `Bool`/`Vec Bool n`/any GF(2) space work
-- immediately.  Connecting `f := ⟦p⟧` for a classical `Exp p` over trees needs an
-- involutive xor on `V` (or the general exit-assertion atom, R1a') -- deferred to
-- R1c-full / R3; here `f` is any total function into the output algebra.
------------------------------------------------------------------------

module Bennett
  (A I : Set)
  (_⊕_ : A → A → A) (zero : A)
  (⊕-invol : (a b : A) → (a ⊕ b) ⊕ b ≡ a)   -- self-inverse output algebra ...
  (⊕-unitˡ : (a : A) → zero ⊕ a ≡ a)          -- ... with a unit (the empty output)
  (f : I → A)                                  -- the function to Bennett-simulate
  where
  open Language (I × A) public                 -- store = (input , output accumulator)

  -- the reversible copy  out ^= f(in)  (self-inverse: `in` is kept, so re-reading
  -- `f in` and ⊕-ing again cancels -- Bennett uncompute, for free)
  copy : Upd
  copy = (λ { (i , o) → (i , o ⊕ f i) })
       , λ { (i , o) → cong (λ z → i , z) (⊕-invol o (f i)) }

  B : Cmd
  B = atom copy

  -- VALUE-LEVEL BENNETT: from a zeroed output, B computes (x , f x), keeping x.
  bennett-value : (x : I) → fwd B (x , zero) ≡ (x , f x)
  bennett-value x = cong (λ z → x , z) (⊕-unitˡ (f x))

  -- reversible (from inv-sound; `atom` is always WF) and cost-symmetric, cost 1
  bennett-rev  : (s : I × A) → bwd B (fwd B s) ≡ s
  bennett-rev  = inv-sound B tt
  bennett-cost : (s : I × A) → cost B s ≡ 1
  bennett-cost _ = refl

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

-- cost: one conditional dispatch + one atom = 2 steps, either branch
_ : cost prog (⟨⟩ , ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)) ≡ 2
_ = refl
_ : cost prog (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩) , ⟨⟩) ≡ 2
_ = refl

-- reversible time-symmetry, concretely: undoing costs the same as doing
prog-cost-sym : ∀ s → cost (inv prog) (fwd prog s) ≡ cost prog s
prog-cost-sym = cost-sym prog wf-prog

-- bounded reversible iteration: swap var1's subtrees 3 times, reversibly.
iter3 : Cmd
iter3 = iterC 3 (atom u1)

-- reversibility of the whole loop, from the general combinator lemma
iter3-rev : ∀ s → bwd iter3 (fwd iter3 s) ≡ s
iter3-rev = iterC-sound 3 {atom u1} tt

-- concrete: 3 swaps of a subtree = 1 swap (swapTop is an involution)
_ : fwd iter3 (⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩) , ⟨⟩) ≡ ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩ , ⟨⟩)
_ = refl

------------------------------------------------------------------------
-- A concrete Bennett instance: reversibly copy the 1-bit predicate `isNil`
-- into a Bool slot (a GF(2)¹ output), keeping the input tree.  (`Bool`/xor is
-- the smallest GF(2) space; `Vec Bool n` scales this to n-bit outputs identically.)
------------------------------------------------------------------------

xor-invol : (a b : Bool) → (a xor b) xor b ≡ a
xor-invol false false = refl
xor-invol false true  = refl
xor-invol true  false = refl
xor-invol true  true  = refl

xor-unitˡ : (a : Bool) → false xor a ≡ a
xor-unitˡ false = refl
xor-unitˡ true  = refl

module BitBennett = Bennett Bool V _xor_ false xor-invol xor-unitˡ isNil

-- value level: from an empty (false) bit, B computes (x , isNil x), keeping x
_ : BitBennett.fwd BitBennett.B (⟨⟩ , false) ≡ (⟨⟩ , true)
_ = refl
_ : BitBennett.fwd BitBennett.B (⟨⟩ ∙ ⟨⟩ , false) ≡ (⟨⟩ ∙ ⟨⟩ , false)
_ = refl
-- the general value lemma, reversibility, and unit cost, at this instance
bit-value : (x : V) → BitBennett.fwd BitBennett.B (x , false) ≡ (x , isNil x)
bit-value = BitBennett.bennett-value
bit-rev   : (s : V × Bool) → BitBennett.bwd BitBennett.B (BitBennett.fwd BitBennett.B s) ≡ s
bit-rev   = BitBennett.bennett-rev
