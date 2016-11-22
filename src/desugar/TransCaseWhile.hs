module TransCaseWhile(transCase) where

import Data.Data(Data)
import AbsWhile
import Lib(true,skip)
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

transCase :: Data a => a -> a
transCase = everywhere (mkT tCase)

tCase (CCase exp patcoms) =
  CBlk (CAsn (Ident "_X") exp : transPatcoms patcoms : [])
tCase exp = exp

transPatcoms :: [PatComT] -> Com
transPatcoms [] = error "impossible happened"
transPatcoms (PatCom pat com : pcs) =
  let x = EVar (Ident "_X")
      pcs' = case pcs of
        [] -> ElseNone
        _  -> ElseOne [transPatcoms pcs]
  in CIf (match x pat) [assign x pat,com] pcs'

match exp pat = case pat of
  PVal val -> EEq exp (EVal val)
  PVar ident -> true
  PCons pat1 pat2 ->
    EAnd (EConsp exp) (EAnd (match (EHd exp) pat1) (match (ETl exp) pat2))

assign exp pat = case pat of
  PVal val -> skip
  PVar ident -> CAsn ident exp
  PCons pat1 pat2 -> CBlk [assign (EHd exp) pat1,assign (ETl exp) pat2]
