module Hascaf.Passes.Stack where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map as M
import           Hascaf.Types.AST
import           Lens.Micro.Platform

data StackState
    = StackState
    { _st_curStack :: Int
    , _st_maxStack :: Int
    , _st_vars :: [M.Map Ident Int]
    }

type StackM = State StackState

stackProgram :: Program Tc -> Program Sta
stackProgram (Program tops) =
    Program $ stackTopLevel <$> tops

stackTopLevel :: TopLevel Tc -> TopLevel Sta
stackTopLevel (FunctionTop func) =
    FunctionTop $ stackFunction func

stackFunction :: Function Tc -> Function Sta
stackFunction (Function () typ ident compound) =
    let (compound', st) = runState (stackCompound compound) initial
        initial = StackState
            { _st_curStack = 0
            , _st_maxStack = 0
            , _st_vars = [M.empty]
            }
        fi = FunctionInfo
            { f_localSize = st ^. st_maxStack
            }
    in Function fi typ ident compound'

stackCompound :: Compound Tc -> StackM (Compound Sta)
stackCompound (Compound stmts) =
    Compound <$> traverse stackStmt stmts

stackStmt :: Stmt Tc -> StackM (Stmt Sta)
stackStmt (ReturnS expr) =
    ReturnS <$> stackExpr expr
stackStmt (ExprS expr) =
    ExprS <$> stackExpr expr
stackStmt (DeclS dty var initial) =
    DeclS dty <$> stackVarDef var <*> traverse stackExpr initial
stackStmt EmptyS = pure EmptyS
stackStmt (CompoundS compound) = do
    savedStackSize <- use st_curStack
    st_vars %= (M.empty :)
    res <- CompoundS <$> stackCompound compound
    st_curStack .= savedStackSize
    st_vars %= tail
    pure res
stackStmt (IfS () cond t e) =
    IfS () <$> stackExpr cond <*> stackStmt t <*> traverse stackStmt e

stackExpr :: Expr Tc -> StackM (Expr Sta)
stackExpr (IntLit lit) =
    pure $ IntLit lit
stackExpr (Unary op expr) =
    Unary op <$> stackExpr expr
stackExpr (Binary op lhs rhs) =
    Binary op <$> stackExpr lhs <*> stackExpr rhs
stackExpr (Assignment lvalue expr) =
    Assignment <$> stackLValue lvalue <*> stackExpr expr
stackExpr (VarRef var) =
    VarRef <$> stackVarUse var
stackExpr (Ternary () c t e) = do
    Ternary () <$> stackExpr c <*> stackExpr t <*> stackExpr e

stackLValue :: LValue Tc -> StackM (LValue Sta)
stackLValue (VarL var) = VarL <$> stackVarUse var

stackVarUse :: Var Tc -> StackM (Var Sta)
stackVarUse (Var typ v) = do
    vdecls <- use st_vars
    pure $ case asum (M.lookup v <$> vdecls) of
        Nothing -> error "Impossible"
        Just pos ->
            let vi = VarInfo { v_type = typ, v_loc = pos }
            in Var vi v

stackVarDef :: Var Tc -> StackM (Var Sta)
stackVarDef (Var typ v) = do
    pos <- use st_curStack
    size <- st_curStack <%= (+ 1)
    st_maxStack %= max size
    let vi = VarInfo { v_type = typ, v_loc = pos }
    st_vars . _head . at v ?= pos
    pure $ Var vi v

-- Lenses

st_curStack :: Lens' StackState Int
st_curStack f x =
    (\u' -> x { _st_curStack = u' }) <$> f (_st_curStack x)

st_maxStack :: Lens' StackState Int
st_maxStack f x =
    (\u' -> x { _st_maxStack = u' }) <$> f (_st_maxStack x)

st_vars :: Lens' StackState [M.Map Ident Int]
st_vars f x =
    (\u' -> x { _st_vars = u' }) <$> f (_st_vars x)
