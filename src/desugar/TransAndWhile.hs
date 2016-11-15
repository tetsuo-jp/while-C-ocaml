module TransAndWhile(transAnd) where

import AbsWhile

-- 数値 n を nil^n に置き換える

transAnd :: TransAnd a => a -> a
transAnd = trans

class TransAnd a where
  trans :: a -> a

instance TransAnd Ident where
  trans x = case x of
    Ident string -> Ident string

instance TransAnd Atom where
  trans x = case x of
    Atom string -> Atom string

instance TransAnd Program where
  trans x = case x of
    Prog procs -> Prog (map trans procs)

instance TransAnd Proc where
  trans x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop (trans ident1) (map trans coms) (trans ident2)

instance TransAnd PNameOp where
  trans x = case x of
    Name ident -> Name ident
    NoName -> NoName

instance TransAnd Com where
  trans x = case x of
    CAsn ident exp -> CAsn (trans ident) (trans exp)
    CProc ident1 ident2 ident3 ->
      CProc (trans ident1) (trans ident2) (trans ident3)
    CLoop exp coms -> CLoop (trans exp) (map trans coms)
    CIf exp coms cElseOp ->
      CIf (trans exp) (map trans coms) (trans cElseOp)
    CCase exp patComTs ->
      CCase (trans exp) (map trans patComTs)
    CBlk coms -> CBlk (map trans coms)
    CShow exp -> CShow (trans exp)

instance TransAnd CElseOp where
  trans x = case x of
    ElseNone -> ElseNone
    ElseOne coms -> ElseOne (map trans coms)

instance TransAnd PatComT where
  trans (PatCom pat com) = PatCom (trans pat) (trans com)

instance TransAnd Pat where
  trans x = x

instance TransAnd Exp where
  trans x = case x of
    ECons exp1 exp2 -> ECons (trans exp1) (trans exp2)
    EConsp exp -> EEq exp (ECons (EHd exp) (ETl exp))
    EAtomp ident -> EEq (trans (EVal VFalse)) (trans (EConsp ident))
    EHd exp -> EHd (trans exp)
    ETl exp -> ETl (trans exp)
    EEq exp1 exp2 -> EEq (trans exp1) (trans exp2)
    EListRep exps tl -> case tl of
      NoTail -> foldr ECons (EVal VNil) (map trans exps)
      Tail atom -> foldr ECons (EVal (VAtom (trans atom))) (map trans exps)
    EAnd exp1 exp2 ->
      let a = EEq (trans exp1) (EVal VNil)
          b = EEq (trans exp2) (EVal VNil)
      in EEq a (EEq a b)
    EVar ident -> EVar (trans ident)
    EVal val -> EVal (trans val)
    EConsStar exps -> foldr1 ECons $ map trans exps
    EList exps -> foldr ECons (EVal VNil) $ map trans exps

instance TransAnd Val where
  trans x = case x of
    VNil -> VNil
    VFalse -> VNil
    VTrue -> VCons VNil VNil
    VAtom atom -> VAtom (trans atom)
    VCons val1 val2 -> VCons (trans val1) (trans val2)
    VInt n -> VInt n
