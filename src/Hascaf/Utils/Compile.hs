module Hascaf.Utils.Compile where

import           Data.Bifunctor (bimap)
import qualified Data.Text as T
import           Hascaf.Passes.Asm
import           Hascaf.Passes.Lower
import           Hascaf.Passes.Parser
import           Text.Megaparsec

compile :: FilePath -> T.Text -> Either String T.Text
compile filePath contents =
    bimap errorBundlePretty (assemble . lowerProgram) $
        parseProgram filePath contents
