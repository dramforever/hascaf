module Hascaf.Passes.Label where

import           Control.Monad.State
import qualified Data.Text as T
import           Hascaf.Types.AST

type LabelM = State Int

labelProgram :: Program Sta -> Program Lbl
labelProgram (Program tops) =
    Program $ evalState (traverse labelTopLevel tops) 0

labelTopLevel :: TopLevel Sta -> LabelM (TopLevel Lbl)
labelTopLevel (FunctionTop func) =
    FunctionTop <$> labelFunction func

labelFunction :: Function Sta -> LabelM (Function Lbl)
labelFunction (Function fi typ ident compound) =
    Function fi typ ident <$> labelCompound compound

labelCompound :: Compound Sta -> LabelM (Compound Lbl)
labelCompound (Compound stmts) =
    Compound <$> traverse labelStmt stmts

labelStmt :: Stmt Sta -> LabelM (Stmt Lbl)
labelStmt (ReturnS expr) =
    ReturnS <$> labelExpr expr
labelStmt (ExprS expr) =
    ExprS <$> labelExpr expr
labelStmt (DeclS dty var initial) =
    DeclS dty <$> labelVar var <*> traverse labelExpr initial
labelStmt EmptyS = pure EmptyS
labelStmt (CompoundS compound) = CompoundS <$> labelCompound compound
labelStmt (IfS () cond t e) = do
    next <- get <* modify (+ 1)
    let prefix = T.pack $ "__if_" ++ show next
    IfS prefix <$> labelExpr cond <*> labelStmt t <*> traverse labelStmt e

labelExpr :: Expr Sta -> LabelM (Expr Lbl)
labelExpr (IntLit lit) =
    pure $ IntLit lit
labelExpr (Unary op expr) =
    Unary op <$> labelExpr expr
labelExpr (Binary op lhs rhs) =
    Binary op <$> labelExpr lhs <*> labelExpr rhs
labelExpr (Assignment lvalue expr) =
    Assignment <$> labelLValue lvalue <*> labelExpr expr
labelExpr (VarRef var) =
    VarRef <$> labelVar var
labelExpr (Ternary () c t e) = do
    next <- get <* modify (+ 1)
    let prefix = T.pack $ "_tern_" ++ show next
    Ternary prefix <$> labelExpr c <*> labelExpr t <*> labelExpr e

labelLValue :: LValue Sta -> LabelM (LValue Lbl)
labelLValue (VarL v) = VarL <$> labelVar v

labelVar :: Var Sta -> LabelM (Var Lbl)
labelVar (Var vi ident) = pure $ Var vi ident
