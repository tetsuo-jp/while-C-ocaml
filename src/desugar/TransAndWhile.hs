module TransAndWhile(transAnd) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

-- and をコア言語に置き換える
--   and A B ≡ =? (cons (=? A nil) (=? B nil)) (cons nil nil)
-- (A, B とも非 nil ⟺ 両方の =? が nil ⟺ ペアが (nil.nil) に一致)

transAnd :: Data a => a -> a
transAnd = everywhere (mkT tAnd)

tAnd :: Exp -> Exp
tAnd (EAnd exp1 exp2) =
      let a = EEq exp1 (EVal VNil)
          b = EEq exp2 (EVal VNil)
      in EEq (ECons a b) (ECons (EVal VNil) (EVal VNil))
tAnd exp = exp
