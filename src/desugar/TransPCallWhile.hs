module TransPCallWhile where

import AbsWhile
import Control.Monad.State
import Data.List(insert,nub)
import Data.Maybe(fromMaybe)
import Lib(true,false)
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT
-- import qualified Data.Map.Strict as Map


------------------------------------------------------------------------------
-- Inline procedure calls
------------------------------------------------------------------------------

-- mapping from procedure names to their bodies
-- type Penv = Map.Map String (Ident,[Com],Ident)
type Penv = [(String,(Ident,[Com],Ident))]
type Rename a = State ([(String,String)],Penv,Int) a

mkPenv :: Program -> Penv
mkPenv (Prog procs) = f procs
 where f (AProc pNameOp ident1 coms ident2 : procs) = case pNameOp of
          NoName -> f procs
          Name (Ident name) -> (name,(ident1,coms,ident2)) : f procs
       f [] = error "No procedure has a procedure name."

doInline :: Program -> Program
doInline prog = evalState (inline prog) ([],mkPenv prog,0)

class RenameC a where
  inline :: a -> Rename a

instance RenameC Program where
  inline (Prog procs) = liftM Prog (mapM inline procs)

instance RenameC Proc where
  inline (AProc pnameop ident1 coms ident2) =
    do coms' <- mapM inline coms
       return $ AProc pnameop ident1 coms' ident2

instance RenameC Com where
  inline x = case x of
      CAsn ident exp -> return $ CAsn ident exp
      CProc (Ident s2) (Ident pname) (Ident s1) ->
        do (env,penv,n) <- get
           let Just (Ident s1',coms,Ident s2') = lookup pname penv
           let vs = varsComs coms
           let env' = (s1',s1) : (s2',s2) : zip vs (map (++"'"++show n) vs)
           put (env',penv,n+1)
           result <- mapM inline coms
           (_,_,n') <- get
           put (env,penv,n')
           return $ CBlk result
      CLoop exp coms ->
        do coms' <- mapM inline coms
           return $ CLoop exp coms'
      CIf exp coms cElseOp ->
        do coms' <- mapM inline coms
           cElseOp' <- inline cElseOp
           return $ CIf exp coms' cElseOp'
      CCase exp patComTs ->
        do patComTs' <- mapM inline patComTs
           return $ CCase exp patComTs'
      CBlk coms ->
        do coms' <- mapM inline coms
           return $ CBlk coms'
      CShow exp -> return (CShow exp)

instance RenameC PatComT where
  inline (PatCom pat com) = do
    com' <- inline com
    return $ PatCom pat com'

instance RenameC CElseOp where
  inline x = case x of
    ElseNone -> return ElseNone
    ElseOne coms -> do coms' <- mapM inline coms
                       return $ ElseOne coms'


------------------------------------------------------------------------------
-- 変数をリストに集める
------------------------------------------------------------------------------

-- vars :: Data a => a -> [String]
-- vars = everything (++) (mkQ [] varsCom)

varsComs :: [Com] -> [String]
varsComs coms = nub $ concatMap varsCom coms

varsCom :: Com -> [String]
varsCom x = case x of
  CAsn (Ident str) exp -> str : varsExp exp
  CProc (Ident str1) ident2 (Ident str2) -> [str1,str2]
  CLoop exp coms -> varsExp exp ++ concatMap varsCom coms
  CShow exp -> varsExp exp

varsExp :: Exp -> [String]
varsExp x = case x of
    ECons exp1 exp2 -> varsExp exp1 ++ varsExp exp2
    EConsp exp -> varsExp exp
    EAtomp ident -> []
    EHd exp -> varsExp exp
    ETl exp -> varsExp exp
    EEq exp1 exp2 -> varsExp exp1 ++ varsExp exp2
    EListRep exps tl -> concatMap varsExp exps
    EVar (Ident str) -> [str]
    EVal val -> []
    EConsStar exps -> concatMap varsExp exps
    EList exps -> concatMap varsExp exps


------------------------------------------------------------------------------
-- Expand if, PDF pp.34-35
------------------------------------------------------------------------------

type RenameIf a = State Int a

expandIf :: Data a => a -> a
expandIf = everywhere (mkT tIf)

tIf :: Com -> Com
tIf x = case x of
    CLoop exp coms -> CLoop exp (map tIf coms)
    CIf exp coms celseop ->
      let comsNoIf = map tIf coms in
      case celseop of
        ElseNone ->
          CBlk [CAsn (Ident "_Z") exp,
                CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : comsNoIf)]
        ElseOne coms' ->
          CBlk [CAsn (Ident "_Z") exp,
                CAsn (Ident "_W") true,
                CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : comsNoIf ++ [CAsn (Ident "_W") false]),
                CLoop (EVar (Ident "_W")) (CAsn (Ident "_W") false : map tIf coms')]
    _ -> x
