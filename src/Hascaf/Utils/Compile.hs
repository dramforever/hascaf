module Hascaf.Utils.Compile where

import           Data.Bifunctor
import qualified Data.Text as T
import           Hascaf.Passes.ASTCheck
import           Hascaf.Passes.Asm
import           Hascaf.Passes.Label
import           Hascaf.Passes.Lower
import           Hascaf.Passes.Parser
import           Hascaf.Passes.Typing
import           Hascaf.Utils.Errors
import           Text.Megaparsec

compile :: FilePath -> T.Text -> Either T.Text T.Text
compile filePath contents = do
    parsed <- first (T.pack . errorBundlePretty) $
        parseProgram filePath contents
    case checkProgramAST parsed of
        [] -> pure ()
        errs -> Left (T.unlines $ ("AST error: " <>) . prettyASTError <$> errs)

    case runErrors (checkProgram parsed) of
        Left errs -> Left (T.unlines $ ("Type error: " <>) . prettyTypeError <$> errs)
        Right checked -> pure $ (assemble . lowerProgram . labelProgram $ checked)
