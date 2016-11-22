module TransAndWhile(transAnd) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

-- and をコア言語に置き換える

transAnd :: Data a => a -> a
transAnd = everywhere (mkT tAnd)

tAnd :: Exp -> Exp
tAnd (EAnd exp1 exp2) =
      let a = EEq (tAnd exp1) (EVal VNil)
          b = EEq (tAnd exp2) (EVal VNil)
      in EEq a (EEq a b)
tAnd exp = exp
