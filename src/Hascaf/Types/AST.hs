{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Hascaf.Types.AST where

import           Data.Derive.TopDown
import qualified Data.Text as T

newtype Ident = Ident T.Text

data Program = Program [TopLevel]

data TopLevel = FunctionTop Function

data Function = Function Typ Ident Compound

data Declaration = Declaration Typ Ident

data Typ = IntTyp

type Compound = [Stmt]

data Stmt = ReturnS Expr

data Expr = IntLit Integer

$(derivings [''Show, ''Eq] ''Program)
