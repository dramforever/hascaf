module Hascaf.Passes.ASTCheck where

import qualified Data.Text as T
import           Hascaf.Types.AST

data ASTError
    = ASTNoMain

prettyASTError :: ASTError -> T.Text
prettyASTError ASTNoMain = "No main function"

checkProgramAST :: Program -> [ASTError]
checkProgramAST (Program tops) =
    if any isMain tops
        then []
        else [ASTNoMain]

    where isMain (FunctionTop (Function _ name _)) = name == Ident "main"
