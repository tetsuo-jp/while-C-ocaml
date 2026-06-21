{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- Design spec for the EXPRESSION-level congruence of cogen's specializer
-- (`spec_exp` + `lift` in Cogen.ml).  A building block of G1 (whole-spec
-- congruence); the loop level is `MemoFold`, this is the base (expressions).
-- Referenced by docs/AGDA_CORRESPONDENCE.md (gap G1).
--
-- Context.  cogen evaluates expressions over ANNOTATED VALUES av = Sv | Dv | Cv:
--   Sv v        static value v
--   Dv e        dynamic; e is residual EXPRESSION code, value = [e] in the env
--   Cv a b      partially-static cons of two avs
-- `lift : av → Expr` turns an av back into residual code; `concretize av env` is
-- the actual value the av denotes once the dynamic environment env is known.
-- The expression specializer applies av_hd/av_tl/av_cons mirroring hd/tl/cons.
--
-- We prove the expression-level congruence:
--   * lift-sound : evaluating the lifted code reproduces the denoted value,
--                  `eval (lift av) env ≡ concretize av env`;
--   * hd-sound / tl-sound / cons-sound : the av operations COMMUTE with
--                  concretisation (av_hd av denotes vhd of what av denotes, etc.)
-- Together: specializing an expression and running its residual gives the same
-- value as the source expression -- the base case the command/loop congruence
-- (G1) is built on (MemoFold supplies the loop step).
------------------------------------------------------------------------

module AVExprSound where

open import Data.Nat using (ℕ)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong₂)

-- core WHILE values (atoms as ℕ tags); hd/tl total as in EvalWhile/Cogen
data Val : Set where
  vnil  : Val
  vatom : ℕ → Val
  vcons : Val → Val → Val

vhd : Val → Val
vhd (vcons a _) = a
vhd _           = vnil

vtl : Val → Val
vtl (vcons _ b) = b
vtl _           = vnil

-- residual expression code (the ⌜ ⌝ form cogen emits)
data Expr : Set where
  var   : ℕ → Expr
  cst   : Val → Expr
  hd    : Expr → Expr
  tl    : Expr → Expr
  cons  : Expr → Expr → Expr

eval : Expr → (ℕ → Val) → Val
eval (var k)    env = env k
eval (cst v)    env = v
eval (hd e)     env = vhd (eval e env)
eval (tl e)     env = vtl (eval e env)
eval (cons a b) env = vcons (eval a env) (eval b env)

-- annotated values
data AV : Set where
  Sv : Val → AV
  Dv : Expr → AV
  Cv : AV → AV → AV

-- lift : av → residual code  (Cogen.ml `lift`)
lift : AV → Expr
lift (Sv v)   = cst v
lift (Dv e)   = e
lift (Cv a b) = cons (lift a) (lift b)

-- concretize : the value an av denotes once the dynamic env is known
concretize : AV → (ℕ → Val) → Val
concretize (Sv v)   env = v
concretize (Dv e)   env = eval e env
concretize (Cv a b) env = vcons (concretize a env) (concretize b env)

-- av operations (Cogen.ml av_hd / av_tl / Cv for cons)
av_hd : AV → AV
av_hd (Cv x _) = x
av_hd (Sv v)   = Sv (vhd v)
av_hd (Dv e)   = Dv (hd e)

av_tl : AV → AV
av_tl (Cv _ y) = y
av_tl (Sv v)   = Sv (vtl v)
av_tl (Dv e)   = Dv (tl e)

av_cons : AV → AV → AV
av_cons = Cv

----------------------------------------------------------------------
-- lift-sound: running the lifted code reproduces the denoted value.
lift-sound : ∀ av env → eval (lift av) env ≡ concretize av env
lift-sound (Sv v)   env = refl
lift-sound (Dv e)   env = refl
lift-sound (Cv a b) env = cong₂ vcons (lift-sound a env) (lift-sound b env)

----------------------------------------------------------------------
-- the av operations commute with concretisation.
hd-sound : ∀ av env → concretize (av_hd av) env ≡ vhd (concretize av env)
hd-sound (Sv v)   env = refl
hd-sound (Dv e)   env = refl
hd-sound (Cv a b) env = refl

tl-sound : ∀ av env → concretize (av_tl av) env ≡ vtl (concretize av env)
tl-sound (Sv v)   env = refl
tl-sound (Dv e)   env = refl
tl-sound (Cv a b) env = refl

cons-sound : ∀ a b env
           → concretize (av_cons a b) env ≡ vcons (concretize a env) (concretize b env)
cons-sound a b env = refl
