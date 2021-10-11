{-# LANGUAGE OverloadedStrings #-}

module Lhox.PrintAst where

import           Data.Functor.Sum (Sum (InL, InR))
import           Data.Text        (Text, pack)

import           Lhox.Parser      (LitNum (MkLitNum), LitStr (MkLitStr),
                                   Unary (MkUnary), UnaryOperator (..))

class Functor f => PrintAst f where
  aprint :: f Text -> Text

instance PrintAst LitNum where
  aprint (MkLitNum n) = pack . show $ n

instance PrintAst LitStr where
  aprint (MkLitStr t) = "\"" <> t <> "\""

instance PrintAst Unary where
  aprint (MkUnary o expr) = showUnaryOp o <> expr

instance (PrintAst f, PrintAst g) => PrintAst (Sum f g) where
  aprint (InL f) = aprint f
  aprint (InR g) = aprint g

-- * Printing utiliites

showUnaryOp :: UnaryOperator -> Text
showUnaryOp Negate = "-"
showUnaryOp Not    = "!"
