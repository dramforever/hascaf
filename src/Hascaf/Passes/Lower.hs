module Hascaf.Passes.Lower where

import Hascaf.Types.AST
import Hascaf.Types.IR

lowerProgram :: Program Tc -> [IR]
lowerProgram (Program tops) = tops >>= lowerTopLevel

lowerTopLevel :: TopLevel Tc -> [IR]
lowerTopLevel (FunctionTop func) = lowerFunction func

lowerFunction :: Function Tc -> [IR]
lowerFunction (Function finfo _ (Ident name) (Compound stmts)) =
    [ Fun name (f_localSize finfo) ]
    ++ (stmts >>= lowerStmt)
    ++ [ Push 0, EndFun (f_localSize finfo) ]

lowerStmt :: Stmt Tc -> [IR]
lowerStmt (ReturnS expr) = lowerExpr expr ++ [ Ret ]
lowerStmt (ExprS expr) = lowerExpr expr ++ [ Pop ]
lowerStmt (DeclS _ var initial) =
    case initial of
        Nothing -> []
        Just expr -> varAddr var ++ lowerExpr expr ++ [ Store, Pop ]
lowerStmt EmptyS = []
lowerStmt (CompoundS (Compound ss)) = ss >>= lowerStmt
lowerStmt (IfS prefix cond t Nothing) =
    lowerExpr cond
    ++ [ Beqz $ prefix <> "_end" ]
    ++ lowerStmt t
    ++ [ Loc $ prefix <> "_end" ]
lowerStmt (IfS prefix cond t (Just e)) =
    lowerExpr cond
    ++ [ Beqz $ prefix <> "_else" ]
    ++ lowerStmt t
    ++
    [ Br $ prefix <> "_end"
    , Loc $ prefix <> "_else"
    ] ++ lowerStmt e
    ++ [ Loc $ prefix <> "_end" ]

lowerExpr :: Expr Tc -> [IR]
lowerExpr (Unary op x) = lowerExpr x ++ lowerUnaryOp op
lowerExpr (IntLit x) = [ Push (fromInteger x) ]
lowerExpr (Binary op x y) = lowerExpr x ++ lowerExpr y ++ lowerBinaryOp op
lowerExpr (VarRef var) = varAddr var ++ [ Load ]
lowerExpr (Assignment (VarL (Var vi _)) expr) =
    [ FrameAddr (v_loc vi) ] ++ lowerExpr expr ++ [ Store ]
lowerExpr (Ternary prefix c t e) =
    lowerExpr c
    ++ [ Beqz $ prefix <> "_else" ]
    ++ lowerExpr t
    ++
    [ Br $ prefix <> "_end"
    , Loc $ prefix <> "_else"
    ] ++ lowerExpr e
    ++ [ Loc $ prefix <> "_end" ]

varAddr :: Var Tc -> [IR]
varAddr (Var vinfo _) = [ FrameAddr (v_loc vinfo) ]

lowerUnaryOp :: UnaryOp -> [IR]
lowerUnaryOp Neg = [NegI]
lowerUnaryOp Not = [NotI]
lowerUnaryOp LNot = [LNotI]

lowerBinaryOp :: BinaryOp -> [IR]
lowerBinaryOp Add = [AddI]
lowerBinaryOp Sub = [SubI]
lowerBinaryOp Mul = [MulI]
lowerBinaryOp Div = [DivI]
lowerBinaryOp Mod = [ModI]
lowerBinaryOp Eq = [EqI]
lowerBinaryOp Ne = [NeI]
lowerBinaryOp Le = [LeI]
lowerBinaryOp Ge = [GeI]
lowerBinaryOp Lt = [LtI]
lowerBinaryOp Gt = [GtI]
lowerBinaryOp LAnd = [LAndI]
lowerBinaryOp LOr = [LOrI]
