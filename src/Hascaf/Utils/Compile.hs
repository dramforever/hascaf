module Hascaf.Utils.Compile where

import           Data.Bifunctor
import qualified Data.Text as T
import           Hascaf.Passes.ASTCheck
import           Hascaf.Passes.Asm
import           Hascaf.Passes.Lower
import           Hascaf.Passes.Parser
import           Text.Megaparsec

compile :: FilePath -> T.Text -> Either T.Text T.Text
compile filePath contents = do
    parsed <- first (T.pack . errorBundlePretty) $
        parseProgram filePath contents
    case checkProgramAST parsed of
        [] -> pure ()
        errs -> Left (T.unlines $ ("error: " <>) . prettyASTError <$> errs)
    pure $ (assemble . lowerProgram $ parsed)
