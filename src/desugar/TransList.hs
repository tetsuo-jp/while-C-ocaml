module TransList(transList) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

transList :: Data a => a -> a
transList = everywhere (mkT tList)

tList :: Exp -> Exp
tList x = case x of
    EListRep exps tl -> case tl of
      NoTail -> foldr ECons (EVal VNil) exps
      Tail atom -> foldr ECons (EVal (VAtom atom)) exps
    EConsStar exps -> foldr1 ECons exps
    EList exps -> foldr ECons (EVal VNil) exps
    _ -> x
