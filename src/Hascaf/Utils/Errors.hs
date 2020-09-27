{-# LANGUAGE DeriveFunctor #-}

module Hascaf.Utils.Errors where

newtype Errors e a = Errors { runErrors :: Either e a }
    deriving (Functor)

instance Monoid e => Applicative (Errors e) where
    pure x = Errors (Right x)
    Errors f <*> Errors x = Errors $ case (f, x) of
        (Left el, Left er) -> Left (el <> er)
        (Left el, Right _) -> Left el
        (Right _, Left er) -> Left er
        (Right l, Right r) -> Right (l r)

mkError :: e -> Errors e a
mkError e = Errors (Left e)

(>>=?) :: Errors e a -> (a -> Errors e b) -> Errors e b
Errors (Left e) >>=? _ = Errors (Left e)
Errors (Right a) >>=? f = case runErrors (f a) of
    Left e -> mkError e
    Right b -> Errors (Right b)
