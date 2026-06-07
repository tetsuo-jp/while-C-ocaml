module TransPCallWhile where

import AbsWhile
import Control.Monad
import Control.Monad.State
import Data.List(nub)
import Data.Maybe(fromMaybe)
import Lib(true,false)
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere, everywhereM, everything
import Data.Generics.Aliases    -- mkT, mkM, mkQ


------------------------------------------------------------------------------
-- Inline procedure calls
------------------------------------------------------------------------------

-- mapping from procedure names to their bodies (with their variables)
type Penv = [(String,(Ident,[Com],Ident,[String]))]
type Rename a = State ([(String,String)],Penv,Int) a

mkPenv :: Program -> Penv
mkPenv (Prog procs) = f procs
 where f (AProc pNameOp ident1 coms ident2 : procs) = case pNameOp of
          NoName -> f procs
          Name (Ident name) -> (name,(ident1,coms,ident2,varsComs coms)) : f procs
       f [] = error "No procedure has a procedure name."

doInline :: Program -> Program
doInline prog = evalState (inline prog) ([],mkPenv prog,0)

-- 現在のリネーム環境を識別子に適用する
renameIdent :: Ident -> Rename Ident
renameIdent (Ident s) = do
  (env,_,_) <- get
  return $ Ident (fromMaybe s (lookup s env))

-- 式・パターン中のすべての識別子にリネーム環境を適用する
rename :: Data a => a -> Rename a
rename = everywhereM (mkM renameIdent)

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
      CAsn ident exp -> liftM2 CAsn (renameIdent ident) (rename exp)
      CProc ident2 (Ident pname) ident1 ->
        do Ident s2 <- renameIdent ident2
           Ident s1 <- renameIdent ident1
           (env,penv,n) <- get
           let Just (Ident s1',coms,Ident s2',vs) = lookup pname penv
           let env' = (s1',s1) : (s2',s2) : zip vs (map (++"'"++show n) vs)
           put (env',penv,n+1)
           result <- mapM inline coms
           (_,_,n') <- get
           put (env,penv,n')
           return $ CBlk result
      CLoop exp coms ->
        do exp' <- rename exp
           coms' <- mapM inline coms
           return $ CLoop exp' coms'
      CIf exp coms cElseOp ->
        do exp' <- rename exp
           coms' <- mapM inline coms
           cElseOp' <- inline cElseOp
           return $ CIf exp' coms' cElseOp'
      CCase exps patComTs ->
        do exps' <- rename exps
           patComTs' <- mapM inline patComTs
           return $ CCase exps' patComTs'
      CBlk coms ->
        do coms' <- mapM inline coms
           return $ CBlk coms'
      CShow exp -> liftM CShow (rename exp)

instance RenameC PatComT where
  inline (PatCom pats com) = do
    pats' <- rename pats
    com' <- inline com
    return $ PatCom pats' com'

instance RenameC CElseOp where
  inline x = case x of
    ElseNone -> return ElseNone
    ElseOne coms -> do coms' <- mapM inline coms
                       return $ ElseOne coms'


------------------------------------------------------------------------------
-- 変数をリストに集める
------------------------------------------------------------------------------

varsComs :: [Com] -> [String]
varsComs = nub . everything (++) (mkQ [] (\(Ident s) -> [s]))


------------------------------------------------------------------------------
-- Expand if, PDF pp.34-35
------------------------------------------------------------------------------

expandIf :: Data a => a -> a
expandIf = everywhere (mkT tIf)

-- everywhere がボトムアップに子を変換済みなので、tIf 自身は再帰しない
tIf :: Com -> Com
tIf x = case x of
    CIf exp coms celseop ->
      case celseop of
        ElseNone ->
          CBlk [CAsn (Ident "_Z") exp,
                CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : coms)]
        ElseOne coms' ->
          CBlk [CAsn (Ident "_Z") exp,
                CAsn (Ident "_W") true,
                CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : coms ++ [CAsn (Ident "_W") false]),
                CLoop (EVar (Ident "_W")) (CAsn (Ident "_W") false : coms')]
    _ -> x
