module TransConspWhile(transConsp) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

-- cons?, atom? をコア言語に置き換える pp.32-33
--   cons? E ≡ =? E (cons (hd E) (tl E))
--   atom? E ≡ =? nil (cons? E)

transConsp :: Data a => a -> a
transConsp = everywhere (mkT tConsp)

tConsp :: Exp -> Exp
tConsp x = case x of
    EConsp exp -> consp exp
    EAtomp exp -> EEq (EVal VNil) (consp exp)
    _ -> x
  where consp exp = EEq exp (ECons (EHd exp) (ETl exp))
