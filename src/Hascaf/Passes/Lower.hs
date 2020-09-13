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
lowerExpr (IntLit x) = [ Push (fromInteger x) ]
