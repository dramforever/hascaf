module Hascaf.Passes.Lower where

import Hascaf.Types.AST
import Hascaf.Types.IR

lowerProgram :: Program -> [IR]
lowerProgram (Program tops) = tops >>= lowerTopLevel

lowerTopLevel :: TopLevel -> [IR]
lowerTopLevel (FunctionTop func) = lowerFunction func

lowerFunction :: Function -> [IR]
lowerFunction (Function _ (Ident name) comp) =
    [ Fun name ] ++ (comp >>= lowerStmt) ++ [ Trap ]

lowerStmt :: Stmt -> [IR]
lowerStmt (ReturnS expr) = lowerExpr expr ++ [ Ret ]

lowerExpr :: Expr -> [IR]
lowerExpr (Unary op x) = lowerExpr x ++ lowerUnaryOp op
lowerExpr (IntLit x) = [ Push (fromInteger x) ]
lowerExpr (Binary op x y) = lowerExpr x ++ lowerExpr y ++ lowerBinaryOp op

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
