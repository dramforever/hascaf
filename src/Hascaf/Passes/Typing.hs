{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Hascaf.Passes.Typing where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import           Hascaf.Types.AST
import           Hascaf.Utils.Errors
import           Lens.Micro.Platform

type TypingE = Errors [TypeError]

data TypeError
    = UndeclaredVariable Ident
    | RedefinedFunction Ident Typ Typ
    | RedefinedVariable Ident
    | ReturnTypeMismatch Typ Typ
    | AssignmentTypeMismatch Ident Typ Typ
    | AssignmentNotLValue
    | InvalidUnaryType UnaryOp Typ
    | InvalidBinaryType BinaryOp Typ Typ
    | TernaryTypeMismatch Typ Typ

type FunctionTable = M.Map Ident Typ

data StatementState
    = StatementState
    { _ss_curStack :: Int
    , _ss_maxStack :: Int
    , _ss_vars :: M.Map Ident VarInfo
    , _ss_supply :: Int
    }

prettyTypeError :: TypeError -> T.Text
prettyTypeError (UndeclaredVariable (Ident name)) =
    "Undeclared variable " <> name
prettyTypeError (RedefinedFunction (Ident name) t0 t1) =
    "Redefined function " <> name
    <> " with incompatible type " <> T.pack (show t1) <> ","
    <> " was " <> T.pack (show t0) <> ","
prettyTypeError (RedefinedVariable (Ident name)) =
    "Redefined variable " <> name
prettyTypeError (ReturnTypeMismatch expected actual) =
    "Return type incorrect,"
    <> " expected " <> T.pack (show expected)
    <> " actual " <> T.pack (show actual)
prettyTypeError (AssignmentTypeMismatch (Ident name) expected actual) =
    "Variable " <> name <> " assigned to incorrect type,"
    <> " expected " <> T.pack (show expected)
    <> " actual " <> T.pack (show actual)
prettyTypeError AssignmentNotLValue =
    "Assignment to non-lvalue"
prettyTypeError (InvalidUnaryType op ty) =
    "Unary operator " <> T.pack (show op)
    <> " cannot have operand type " <> T.pack (show ty)
prettyTypeError (InvalidBinaryType op tl tr) =
    "Binary operator " <> T.pack (show op)
    <> " cannot have operand types " <> T.pack (show tl)
    <> " and " <> T.pack (show tr)
prettyTypeError (TernaryTypeMismatch tty ety) =
    "Ternary type mismatch,"
    <> " true branch is " <> T.pack (show tty) <> ","
    <> " false branch is " <> T.pack (show ety)

checkProgram :: Program Syn -> TypingE (Program Tc)
checkProgram (Program tops) =
    let act = Program <$.> traverseC checkTopLevel tops
    in evalState act M.empty

checkTopLevel :: TopLevel Syn -> State FunctionTable (TypingE (TopLevel Tc))
checkTopLevel (FunctionTop func) =
    FunctionTop <$.> checkFunction func

checkFunction :: Function Syn -> State FunctionTable (TypingE (Function Tc))
checkFunction (Function () typ ident compound) = do
    t0 <- use (at ident)
    errs <- case t0 of
        Just typ0 | typ0 /= typ ->
            pure $ mkError [RedefinedFunction ident typ0 typ]
        _ -> do
            at ident ?= typ
            pureC ()

    let initialSS = StatementState
            { _ss_curStack = 0
            , _ss_maxStack = 0
            , _ss_vars = M.empty
            , _ss_supply = 0
            }

    table <- get
    let (res, ss) = runState (checkCompound table typ compound) initialSS
        fi = FunctionInfo
            { f_localSize = ss ^. ss_maxStack
            }
    pure $ errs *> (Function fi typ ident <$> res)

checkCompound :: FunctionTable -> Typ -> Compound Syn -> State StatementState (TypingE (Compound Tc))
checkCompound table ret (Compound stmts) =
    Compound <$.> traverseC (checkStmt table ret) stmts

checkStmt :: FunctionTable -> Typ -> Stmt Syn -> State StatementState (TypingE (Stmt Tc))
checkStmt table ret (ReturnS expr) =
    (>>=? \(e, ty) ->
        if ty == ret
        then pure $ ReturnS e
        else mkError [ReturnTypeMismatch ret ty])
    <$> checkExpr table expr

checkStmt table _ret (ExprS expr) = ExprS . fst <$.> checkExpr table expr

checkStmt table _ret (DeclS dty var@(Var () v) initial) = do
    let checkInitial expr =
            (>>=? \(e, ety) ->
                if ety == dty
                then pure e
                else mkError [AssignmentTypeMismatch v dty ety])
            <$> checkExpr table expr
    DeclS dty <$.> checkVarDef dty var <*.> traverseC checkInitial initial

checkStmt _table _ret EmptyS = pureC EmptyS

checkStmt table ret (CompoundS compound) =
    CompoundS <$.> checkCompound table ret compound

checkStmt table ret (IfS () cond t e) = do
    next <- ss_supply <<%= (+ 1)
    IfS (T.pack $ "__if_" ++ show next)
        <$.> (fst <$.> checkExpr table cond)
        <*.> checkStmt table ret t
        <*.> traverseC (checkStmt table ret) e

checkExpr :: FunctionTable -> Expr Syn -> State StatementState (TypingE (Expr Tc, Typ))
checkExpr _table (IntLit lit) = pureC (IntLit lit, IntTyp)

-- TODO Replace with real checks when more types are added
checkExpr table (Unary op expr) =
    (\e -> (Unary op e, IntTyp))
    <$.> (fst <$.> checkExpr table expr)
checkExpr table (Binary op lhs rhs) =
    (\l r -> (Binary op l r, IntTyp))
    <$.> (fst <$.> checkExpr table lhs)
    <*.> (fst <$.> checkExpr table rhs)

checkExpr table (Assignment (VarRef (Var () v)) expr) =
    (>>=? \((var, tv), (e, te)) ->
        if tv == te
            then pure (Assignment (VarL var) e, tv)
            else mkError [AssignmentTypeMismatch v tv te])
    <$> ((,) <$.> checkVarUse v <*.> checkExpr table expr)

checkExpr _table (Assignment _ _expr) = pure $ mkError [AssignmentNotLValue]

checkExpr _table (VarRef (Var () v)) =
    (_1 %~ VarRef) <$.> checkVarUse v

checkExpr table (Ternary () c t e) = do
    next <- ss_supply <<%= (+1)
    let prefix = T.pack $ "_tern_" ++ show next
    (>>=? \((cc, _cty), (ct, tty), (ce, ety)) ->
        if tty == ety
            then pure (Ternary prefix cc ct ce, tty)
            else mkError [TernaryTypeMismatch tty ety])
        <$> ((,,) <$.> checkExpr table c <*.> checkExpr table t <*.> checkExpr table e)

checkVarUse :: Ident -> State StatementState (TypingE (Var Tc, Typ))
checkVarUse v = do
    vdecl <- use (ss_vars . at v)
    pure $ case vdecl of
        Nothing -> mkError [UndeclaredVariable v]
        Just vi -> pure (Var vi v, v_type vi)

checkVarDef :: Typ -> Var Syn -> State StatementState (TypingE (Var Tc))
checkVarDef typ (Var () var) = do
    vdecl <- use (ss_vars . at var)
    case vdecl of
        Just _ -> do
            pure $ mkError [RedefinedVariable var]
        _ -> do
            pos <- use ss_curStack
            size <- ss_curStack <%= (+ 1)
            ss_maxStack %= max size
            let vi = VarInfo { v_type = typ, v_loc = pos }
            ss_vars . at var ?= vi
            pureC $ Var vi var

-- Lenses

ss_curStack :: Lens' StatementState Int
ss_curStack f x =
    (\u' -> x { _ss_curStack = u' }) <$> f (_ss_curStack x)

ss_maxStack :: Lens' StatementState Int
ss_maxStack f x =
    (\u' -> x { _ss_maxStack = u' }) <$> f (_ss_maxStack x)

ss_vars :: Lens' StatementState (M.Map Ident VarInfo)
ss_vars f x =
    (\u' -> x { _ss_vars = u' }) <$> f (_ss_vars x)

ss_supply :: Lens' StatementState Int
ss_supply f x =
    (\u' -> x { _ss_supply = u' }) <$> f (_ss_supply x)

-- Double bagging

pureC :: forall f g a. (Applicative f, Applicative g) => a -> f (g a)
pureC = pure . pure

(<$.>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$.>) = fmap . fmap

infixl 4 <$.>

(<*.>) :: forall f g a b. (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<*.>) = liftA2 (<*>)

infixl 4 <*.>

traverseC
    :: forall f g t a b. (Applicative f, Applicative g, Traversable t)
    => (a -> f (g b)) -> t a -> f (g (t b))
traverseC f t = sequenceA <$> traverse f t