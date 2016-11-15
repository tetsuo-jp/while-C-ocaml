module TransCaseWhile(transCase) where

import Data.Data(Data,gmapT)
import AbsWhile
import Lib(true,skip)

-- 数値 n を nil^n に置き換える

transCase :: TransCase a => a -> a
transCase = trans

class TransCase a where
  trans :: a -> a

instance TransCase Ident where
  trans x = case x of
    Ident string -> Ident string

instance TransCase Atom where
  trans x = case x of
    Atom string -> Atom string

instance TransCase Program where
  trans (Prog procs) = Prog (map trans procs)

instance TransCase Proc where
  trans x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop (trans ident1) (map trans coms) (trans ident2)

instance TransCase PNameOp where
  trans x = case x of
    Name ident -> Name ident
    NoName -> NoName

instance TransCase Com where
  trans x = case x of
    CAsn ident exp -> CAsn (trans ident) (trans exp)
    CProc ident1 ident2 ident3 ->
      CProc (trans ident1) (trans ident2) (trans ident3)
    CLoop exp coms -> CLoop (trans exp) (map trans coms)
    CIf exp coms cElseOp -> CIf exp (map trans coms) (trans cElseOp)
    CCase exp patcoms ->
      CBlk (CAsn (Ident "_X") exp : transPatcoms patcoms : [])
    CShow exp -> CShow (trans exp)

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

instance TransCase CElseOp where
  trans x = case x of
    ElseNone -> ElseNone
    ElseOne coms -> ElseOne coms

instance TransCase Exp where
  trans x = case x of
    ECons exp1 exp2 -> ECons (trans exp1) (trans exp2)
    EConsp exp -> EConsp exp
    EAtomp exp -> EAtomp exp
    EHd exp -> EHd (trans exp)
    ETl exp -> ETl (trans exp)
    EEq exp1 exp2 -> EEq (trans exp1) (trans exp2)
    EListRep exps tl -> case tl of
      NoTail -> foldr ECons (EVal VNil) (map trans exps)
      Tail atom -> foldr ECons (EVal (VAtom (trans atom))) (map trans exps)
    EAnd exp1 exp2 -> EAnd (trans exp1) (trans exp2)
    EVar ident -> EVar (trans ident)
    EVal val -> EVal (trans val)
    EConsStar exps -> foldr1 ECons $ map trans exps
    EList exps -> foldr ECons (EVal VNil) $ map trans exps

instance TransCase Val where
  trans x = case x of
    VNil -> VNil
    VFalse -> VNil
    VTrue -> VCons VNil VNil
    VAtom atom -> VAtom (trans atom)
    VCons val1 val2 -> VCons (trans val1) (trans val2)
    VInt n -> VInt n
