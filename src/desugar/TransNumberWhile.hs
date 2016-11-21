module TransNumberWhile(transNumber) where

import AbsWhile

-- 数値 n を nil^n に置き換える

transNumber :: TransNumber a => a -> a
transNumber = trans

class TransNumber a where
  trans :: a -> a

instance TransNumber Ident where
  trans x = case x of
    Ident string -> Ident string

instance TransNumber Atom where
  trans x = case x of
    Atom string -> Atom string

instance TransNumber Program where
  trans x = case x of
    Prog procs -> Prog (map trans procs)

instance TransNumber Proc where
  trans x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop (trans ident1) (map trans coms) (trans ident2)

instance TransNumber PNameOp where
  trans x = case x of
    Name ident -> Name ident
    NoName -> NoName

instance TransNumber Com where
  trans x = case x of
    CAsn ident exp -> CAsn (trans ident) (trans exp)
    CProc ident1 ident2 ident3 -> CProc (trans ident1) (trans ident2) (trans ident3)
    CLoop exp coms -> CLoop (trans exp) (map trans coms)
    CShow exp -> CShow (trans exp)

instance TransNumber Exp where
  trans x = case x of
    ECons exp1 exp2 -> ECons (trans exp1) (trans exp2)
    EConsp exp -> EEq exp (ECons (EHd exp) (ETl exp))
    EAtomp exp -> EEq (trans (EVal VFalse)) (trans (EConsp exp))
    EHd exp -> EHd (trans exp)
    ETl exp -> ETl (trans exp)
    EEq exp1 exp2 -> EEq (trans exp1) (trans exp2)
    EListRep exps tl -> case tl of
      NoTail -> foldr ECons (EVal VNil) (map trans exps)
      Tail atom -> foldr ECons (EVal (VAtom (trans atom))) (map trans exps)
    EVar ident -> EVar (trans ident)
    EVal val -> EVal (trans val)
    EConsStar exps -> foldr1 ECons $ map trans exps
    EList exps -> foldr ECons (EVal VNil) $ map trans exps

instance TransNumber Val where
  trans x = case x of
    VNil -> VNil
    VFalse -> VNil
    VTrue -> VCons VNil VNil
    VAtom atom -> VAtom (trans atom)
    VCons val1 val2 -> VCons (trans val1) (trans val2)
    VInt n -> foldr VCons VNil $ replicate (fromInteger n) VNil
