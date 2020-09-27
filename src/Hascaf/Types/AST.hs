{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

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

data Expr x
    = IntLit Integer
    | Unary UnaryOp (Expr x)
    | Binary BinaryOp (Expr x) (Expr x)
    | Assignment (LValue x) (Expr x)
    | VarRef (Var x)

type family LValue x :: Type

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

type instance XFunction Syn = ()
type instance XFunction Tc = FunctionInfo

data FunctionInfo
    = FunctionInfo
    { f_localSize :: Int -- ^ Maximum size of local variables, in words
    }

type instance XVar Syn = ()
type instance XVar Tc = VarInfo

data VarInfo
    = VarInfo
    { v_type :: Typ
    , v_loc :: Int
    }

type instance LValue Syn = Expr Syn
type instance LValue Tc = TcLValue

data TcLValue
    = VarL (Var Tc)
