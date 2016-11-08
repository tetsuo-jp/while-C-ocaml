module InlineWhile where

import AbsWhile

class Inline a where
  inline :: ([String],Int) -> a -> a
  -- inline :: ([String],Int) -> a -> (a,Int)

instance Inline Ident where
  inline s x = x

instance Inline Atom where
  inline _ x = x

instance Inline Program where
  inline s x = case x of
    Prog procs -> Prog (map inline s procs)

instance Inline Proc where
  inline s x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop ident1 (map inline s coms) ident2

instance Inline PNameOp where
  inline s x = case x of
    Name ident -> Name ident
    NoName -> NoName

instance Inline Com where
  inline s x = case x of
    CAsn ident exp -> CAsn ident (inline s exp)
    CProc ident1 ident2 ident3 -> CProc ident1 ident2 ident3
    CLoop exp coms -> CLoop (inline s exp) (map inline s coms)
    CShow exp -> CShow (inline s exp)

instance Inline Exp where
  inline s x = case x of
    ECons exp1 exp2 -> ECons (inline s exp1) (inline s exp2)
    EConsp ident -> let x = EVar ident in EEq x (ECons (EHd x) (ETl x))
    EAtomp ident -> EEq (inline s (EVal VFalse)) (inline s (EConsp ident))
    EHd exp -> EHd (inline s exp)
    ETl exp -> ETl (inline s exp)
    EEq exp1 exp2 -> EEq (inline s exp1) (inline s exp2)
    EListRep exps tl -> case tl of 
      NoTail -> foldr ECons (EVal VNil) (map inline s exps)
      Tail atom -> foldr ECons (EVal (VAtom (inline s atom))) (map inline s exps)
    EVar ident -> EVar ident
    EVal val -> EVal (inline s val)
    EConsStar exps -> foldr1 ECons $ map inline s exps
    EList exps -> foldr ECons (EVal VNil) $ map inline s exps

instance Inline Val where
  inline s x = case x of
    VNil -> VNil
    VFalse -> VNil
    VTrue -> VCons VNil VNil
    VAtom atom -> VAtom (inline s atom)
    VCons val1 val2 -> VCons (inline s val1) (inline s val2)
    VInt n -> foldr VCons VNil $ replicate (fromInteger n) VNil
