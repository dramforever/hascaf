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
asmIR NegI = unary ["neg t0, t0"]
asmIR NotI = unary ["not t0, t0"]
asmIR LNotI = unary ["seqz t0, t0"]
asmIR AddI = binary ["add t0, t0, t1"]
asmIR SubI = binary ["sub t0, t0, t1"]
asmIR MulI = binary ["mul t0, t0, t1"]
asmIR DivI = binary ["div t0, t0, t1"]
asmIR ModI = binary ["rem t0, t0, t1"]
asmIR EqI = binary ["sub t0, t0, t1", "seqz t0, t0"]
asmIR NeI = binary ["sub t0, t0, t1", "snez t0, t0"]
asmIR LeI = binary ["sub t0, t0, t1", "sgtz t0, t0", "seqz t0, t0"]
asmIR GeI = binary ["sub t0, t0, t1", "sltz t0, t0", "seqz t0, t0"]
asmIR LtI = binary ["sub t0, t0, t1", "sltz t0, t0"]
asmIR GtI = binary ["sub t0, t0, t1", "sgtz t0, t0"]
asmIR LAndI = binary ["snez t0, t0", "snez t1, t1", "and t0, t0, t1"]
asmIR LOrI = binary ["or t0, t0, t1", "snez t0, t0"]

unary :: [T.Text] -> [T.Text]
unary mid = indent <$>
    [ "lw t0, 0(sp)" ]
    ++ mid
    ++ [ "sw t0, 0(sp)" ]

binary :: [T.Text] -> [T.Text]
binary mid = indent <$>
    [ "lw t0, __WORD_SIZE(sp)"
    , "lw t1, 0(sp)"
    , "addi sp, sp, __WORD_SIZE"
    ]
    ++ mid
    ++ [ "sw t0, 0(sp)" ]

header :: [T.Text]
header =
    [ indent $ ".equ __WORD_SIZE, 4"
    ]

indent :: T.Text -> T.Text
indent "" = ""
indent x = "    " <> x
