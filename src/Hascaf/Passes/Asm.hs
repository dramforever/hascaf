module Hascaf.Passes.Asm where

import qualified Data.Text as T
import           Hascaf.Types.IR

assemble :: [IR] -> T.Text
assemble irs = T.unlines $ header ++ (irs >>= go)
    where go ir = "" : indent ("# <" <> T.pack (show ir) <> ">") : asmIR ir

asmIR :: IR -> [T.Text]
asmIR (Fun x) =
    [ indent $ ".text"
    , indent $ ".global " <> x
    , x <> ":"
    ]
asmIR (Loc x) = [ "." <> x <> ":" ]
asmIR Trap = indent <$> [ "unimp" ]
asmIR (Push x) = indent <$>
    [ "addi sp, sp, -__WORD_SIZE"
    , "li t1, " <> T.pack (show x)
    , "sw t1, 0(sp)"
    ]
asmIR Ret = indent <$>
    [ "lw a0, 0(sp)"
    , "addi sp, sp, __WORD_SIZE"
    , "jr ra"
    ]
asmIR NegI = unary ["neg a0, a0"]
asmIR NotI = unary ["not a0, a0"]
asmIR LNotI = unary ["seqz a0, a0"]

unary :: [T.Text] -> [T.Text]
unary mid = indent <$>
    [ "lw a0, 0(sp)" ]
    ++ mid
    ++ [ "sw a0, 0(sp)" ]

header :: [T.Text]
header =
    [ indent $ ".equ __WORD_SIZE, 4"
    ]

indent :: T.Text -> T.Text
indent "" = ""
indent x = "    " <> x
