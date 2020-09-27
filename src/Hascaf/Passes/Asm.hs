module Hascaf.Passes.Asm where

import qualified Data.Text as T
import           Hascaf.Types.IR

assemble :: [IR] -> T.Text
assemble irs = T.unlines $ header ++ (irs >>= go)
    where go ir = "" : indent ("# <" <> showT ir <> ">") : asmIR ir

asmIR :: IR -> [T.Text]
asmIR (Fun x num) =
    (indent <$>
    [ ".text"
    , ".global " <> x
    ]) ++ [ x <> ":" ]
    ++ (indent <$>
    [ "addi sp, sp, - __XLEN * (2 + " <> showT num <> ")"
    , "sw fp, (__XLEN * (1 + " <> showT num <> "))(sp)"
    , "sw ra, (__XLEN * (0 + " <> showT num <> "))(sp)"
    , "addi fp, sp, - __XLEN * (2 + " <> showT num <> ")"
    ])
asmIR (EndFun num) =
    [ ".__epilogue:" ]
    ++ (indent <$>
    [ "lw a0, 0(sp)"
    , "lw fp, (__XLEN * (2 + " <> showT num <> "))(sp)"
    , "lw ra, (__XLEN * (1 + " <> showT num <> "))(sp)"
    , "addi sp, sp, __XLEN * (3 + " <> showT num <> ")"
    , "ret"
    ])
asmIR (Loc x) = [ "." <> x <> ":" ]
asmIR Trap = indent <$> [ "unimp" ]
asmIR (Push x) = indent <$>
    [ "addi sp, sp, -__XLEN"
    , "li t1, " <> showT x
    , "sw t1, 0(sp)"
    ]
asmIR Pop = indent <$> [ "addi sp, sp, __XLEN" ]
asmIR Ret = indent <$> [ "j .__epilogue" ]
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
asmIR (FrameAddr num) = indent <$>
    [ "addi sp, sp, - __XLEN"
    , "addi t0, fp, - __XLEN * (3 + " <> showT num <> ")"
    , "sw t0, 0(sp)"
    ]
asmIR Load = indent <$>
    [ "lw t0, 0(sp)"
    , "lw t0, (t0)"
    , "sw t0, 0(sp)"
    ]
asmIR Store = indent <$>
    [ "lw t0, __XLEN(sp)"
    , "lw t1, 0(sp)"
    , "sw t1, (t0)"
    , "sw t1, __XLEN(sp)"
    , "addi sp, sp, __XLEN"
    ]

unary :: [T.Text] -> [T.Text]
unary mid = indent <$>
    [ "lw t0, 0(sp)" ]
    ++ mid
    ++ [ "sw t0, 0(sp)" ]

binary :: [T.Text] -> [T.Text]
binary mid = indent <$>
    [ "lw t0, __XLEN(sp)"
    , "lw t1, 0(sp)"
    , "addi sp, sp, __XLEN"
    ]
    ++ mid
    ++ [ "sw t0, 0(sp)" ]

header :: [T.Text]
header =
    [ indent $ ".equ __XLEN, 4"
    ]

indent :: T.Text -> T.Text
indent "" = ""
indent x = "    " <> x

showT :: Show a => a -> T.Text
showT = T.pack . show
