module Hascaf.Types.AST where

import           Data.Kind
import qualified Data.Text as T

newtype Ident = Ident T.Text
    deriving (Show, Eq, Ord)

data Program x = Program [TopLevel x]

data TopLevel x = FunctionTop (Function x)

data Function x = Function (XFunction x) Typ Ident (Compound x)

type family XFunction x :: Type

data Declaration x = Declaration Typ Ident

data Typ = IntTyp
    deriving (Show, Eq)

data Compound x = Compound [Stmt x]

data Stmt x
    = ReturnS (Expr x)
    | ExprS (Expr x)
    | DeclS Typ (Var x) (Maybe (Expr x))
    | EmptyS
    | CompoundS (Compound x)
    | IfS (XPrefix x) (Expr x) (Stmt x) (Maybe (Stmt x))

type family XPrefix x :: Type

data Expr x
    = IntLit Integer
    | Unary UnaryOp (Expr x)
    | Binary BinaryOp (Expr x) (Expr x)
    | Assignment (XLValue x) (Expr x)
    | VarRef (Var x)
    | Ternary (XPrefix x) (Expr x) (Expr x) (Expr x)

type family XLValue x :: Type

data Var x = Var (XVar x) Ident
type family XVar x :: Type

data UnaryOp
    = Neg | Not | LNot
    deriving (Show, Eq, Ord)

data BinaryOp
    = Add | Sub | Mul | Div | Mod
    | Eq | Ne | Le | Ge | Lt | Gt
    | LAnd | LOr
    deriving (Show, Eq, Ord)

-- * Extensions

data Syn
data Tc
data Sta
data Lbl

type instance XFunction Syn = ()
type instance XFunction Tc  = ()
type instance XFunction Sta = FunctionInfo
type instance XFunction Lbl = FunctionInfo

data FunctionInfo
    = FunctionInfo
    { f_localSize :: Int -- ^ Maximum size of local variables, in words
    }


type instance XVar Syn      = ()
type instance XVar Tc       = Typ
type instance XVar Sta      = VarInfo
type instance XVar Lbl      = VarInfo

data VarInfo
    = VarInfo
    { v_type :: Typ
    , v_loc :: Int
    }


type instance XLValue Syn    = Expr Syn
type instance XLValue Tc     = LValue Tc
type instance XLValue Sta    = LValue Sta
type instance XLValue Lbl    = LValue Lbl

data LValue x
    = VarL (Var x)


type instance XPrefix Syn   = ()
type instance XPrefix Tc    = ()
type instance XPrefix Sta   = ()
type instance XPrefix Lbl   = T.Text
