module Hascaf.Types.IR where

import           Data.Int
import qualified Data.Text as T

data IR
    = Fun T.Text
    | Loc T.Text
    | Push Int32
    | Ret
    | Trap
    | NegI
    | NotI
    | LNotI
    deriving (Show, Eq)
