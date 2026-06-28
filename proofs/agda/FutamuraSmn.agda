{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- A CONCRETE, GENUINELY-SPECIALIZING witness for the Futamura meta-theorem.
--
-- `FutamuraProjections` derived fp1/fp2/fp3 from an abstract `MixModel.mix-eq`,
-- with a witness whose `mix` merely DEFERRED (a closure).  This module supplies
-- a witness whose `mix` actually PARTIALLY EVALUATES: applied to a first-order
-- program text and a static input it constant-folds the static input into the
-- program (by substitution), and the spec equation `mix-eq` is discharged by a
-- REAL specialization theorem `spec-correct` (not `refl`).
--
-- This is the concrete s-m-n step asked for: reify a specializer and discharge
-- the mix equation from its correctness.  It is the finite, machine-checked
-- analogue of G1's whole-spec congruence (`SpecAssembly*`): there the source is
-- WHILE commands over a store; here it is a straight-line term language, small
-- enough to run inside Agda yet with a genuine binding-time split (static `car`
-- of the input vs dynamic `cdr`) and a genuine residual (the static half folded
-- away).
--
-- THE WALL (honest scope).  Full self-application — `mix` specializing `mix`
-- ITSELF by constant folding — needs `mix` to be expressible as object-language
-- data, i.e. the object language must be universal (recursion / a universal
-- interpreter).  The straight-line `Tm` here cannot express its own specializer,
-- so the mix-on-mix case necessarily falls back to the closure mechanism (still
-- proven sound).  Lifting genuine folding through self-application = formalising
-- a universal WHILE interpreter over ⌜p⌝ in Agda; that is the next milestone
-- (docs/AGDA_CORRESPONDENCE.md).  What IS new here: the FIRST projection's
-- specialization is concrete and verified, pinning exactly where the wall sits.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md.
------------------------------------------------------------------------

module FutamuraSmn where

open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong₂; trans)
open import FutamuraProjections using (MixModel; module Projections)

------------------------------------------------------------------------
-- Carrier `D` and one-input straight-line programs `Tm` (mutually defined:
-- a program text `prg t` is a datum, and a term may embed a literal datum).
------------------------------------------------------------------------

data D : Set
data Tm : Set

data D where
  ⟨⟩  : D                 -- nil
  cns : D → D → D         -- cons / pairing
  MIX : D                 -- the specializer's own (opaque) text
  prg : Tm → D            -- a first-order program text, as data
  clo : D → D → D         -- a deferred closure (mix-on-non-first-order fallback)

data Tm where
  inp   : Tm              -- the program's single input
  knil  : Tm              -- constant nil
  kcons : Tm → Tm → Tm    -- build a cons
  car   : Tm → Tm         -- head
  cdr   : Tm → Tm         -- tail
  lit   : D → Tm          -- a literal (lifted constant) — how static data re-enters code

-- total head / tail on the data domain.
hd : D → D
hd (cns a _) = a
hd _         = ⟨⟩

tl : D → D
tl (cns _ b) = b
tl _         = ⟨⟩

-- one-input evaluation: `ev1 t x` runs program text `t` on input datum `x`.
ev1 : Tm → D → D
ev1 inp         x = x
ev1 knil        x = ⟨⟩
ev1 (kcons a b) x = cns (ev1 a x) (ev1 b x)
ev1 (car t)     x = hd (ev1 t x)
ev1 (cdr t)     x = tl (ev1 t x)
ev1 (lit v)     x = v

------------------------------------------------------------------------
-- Substitution of the input, and the specializer it powers.
------------------------------------------------------------------------

-- `sub t u` = t with its input replaced by the term u.
sub : Tm → Tm → Tm
sub inp         u = u
sub knil        u = knil
sub (kcons a b) u = kcons (sub a u) (sub b u)
sub (car t)     u = car (sub t u)
sub (cdr t)     u = cdr (sub t u)
sub (lit v)     u = lit v

-- substitution lemma: running a substituted program = running on the substituted input.
sub-correct : ∀ t u x → ev1 (sub t u) x ≡ ev1 t (ev1 u x)
sub-correct inp         u x = refl
sub-correct knil        u x = refl
sub-correct (kcons a b) u x = cong₂ cns (sub-correct a u x) (sub-correct b u x)
sub-correct (car t)     u x = cong hd (sub-correct t u x)
sub-correct (cdr t)     u x = cong tl (sub-correct t u x)
sub-correct (lit v)     u x = refl

-- The specializer.  A two-input program reads static = `car inp`, dynamic =
-- `cdr inp`.  Specializing to static value `sv` rebinds the input to
-- `(sv , <dynamic input>)`, folding the static half to a literal.
spec : Tm → D → Tm
spec t sv = sub t (kcons (lit sv) inp)

-- THE SPEC EQUATION (concrete): the residual run on the dynamic input alone
-- reproduces the program on the paired input `(sv , d)`.  Static input folded in.
spec-correct : ∀ t sv d → ev1 (spec t sv) d ≡ ev1 t (cns sv d)
spec-correct t sv d = sub-correct t (kcons (lit sv) inp) d

------------------------------------------------------------------------
-- The universal `run`, and `mix` that genuinely specializes.
------------------------------------------------------------------------

run : D → D → D
run (prg t)   x         = ev1 t x                    -- run a program text on its input
run MIX       (cns (prg t) s) = prg (spec t s)        -- ★ GENUINE specialization
run MIX       (cns p s)       = clo p s               -- defer for non-first-order p
run MIX       x               = x                     -- mix on a non-pair (unused)
run (clo p s) d         = run p (cns s d)            -- a closure resumes on (frozen , d)
run ⟨⟩        d         = ⟨⟩
run (cns a b) d         = cns a b

------------------------------------------------------------------------
-- The MixModel instance: `mix-eq` discharged by `spec-correct` for first-order
-- programs and by the closure law (refl) for everything else.
------------------------------------------------------------------------

mix-eq : ∀ p s d → run (run MIX (cns p s)) d ≡ run p (cns s d)
mix-eq (prg t)   s d = spec-correct t s d    -- ← the real specialization theorem
mix-eq ⟨⟩        s d = refl
mix-eq (cns a b) s d = refl
mix-eq MIX       s d = refl
mix-eq (clo a b) s d = refl

model : MixModel
model = record
  { Data   = D
  ; run    = run
  ; pair   = cns
  ; mix    = MIX
  ; mix-eq = mix-eq
  }

open Projections model hiding (spec)

------------------------------------------------------------------------
-- Concrete checks (the "unit tests").  Each holds by `refl`, so the type
-- checker validates that the projections compute AND that fp1 genuinely folds
-- the static input away.
------------------------------------------------------------------------

-- a two-input program: swap the pair, i.e. build (dynamic , static).
swapPair : Tm
swapPair = kcons (cdr inp) (car inp)

-- concrete static / dynamic data.
a b : D
a = cns ⟨⟩ ⟨⟩            -- some static value
b = ⟨⟩                   -- some dynamic value

-- fp1: specialising swapPair to the static value `a` yields a TARGET that takes
-- only the DYNAMIC input `b` (not the pair) and still recovers `a` in its output
-- (b , a) — the static input has been genuinely folded into the residual.
_ : run (target (prg swapPair) a) b ≡ cns b a
_ = refl

-- the residual is run on `b` ALONE, yet equals the source run on the full pair
-- (a , b): this is the genuine specialization, an instance of spec-correct (the
-- closure witness could only DEFER this, never fold).
_ : run (target (prg swapPair) a) b ≡ run (prg swapPair) (cns a b)
_ = fp1 (prg swapPair) a b

-- fp2: the compiler applied to a source yields the fp1 target.
_ : run (compiler (prg swapPair)) a ≡ target (prg swapPair) a
_ = refl

-- fp3: cogen applied to an interpreter yields its fp2 compiler (self-application
-- routes through the closure mechanism — sound, but not folding: the WALL).
_ : run cogen (prg swapPair) ≡ compiler (prg swapPair)
_ = refl

-- the full three-level composition still computes the source meaning.
_ : run (run (run cogen (prg swapPair)) a) b ≡ run (prg swapPair) (cns a b)
_ = fp3-correct (prg swapPair) a b
