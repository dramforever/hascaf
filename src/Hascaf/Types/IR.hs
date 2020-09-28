module Hascaf.Types.IR where

import           Data.Int
import qualified Data.Text as T

data IR
    = Fun T.Text Int
    | EndFun Int
    | Loc T.Text
    | Push Int32
    | Pop
    | Ret
    | Trap
    | NegI | NotI | LNotI
    | AddI | SubI | MulI | DivI | ModI
    | EqI | NeI | LeI | GeI | LtI | GtI
    | LAndI | LOrI
    | FrameAddr Int
    | Load | Store
    | Beqz T.Text
    | Bnez T.Text
    | Br T.Text
    deriving (Show, Eq)
