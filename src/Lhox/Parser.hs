{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lhox.Parser where

import           Data.Fix         (Fix)
import           Data.Functor.Sum (Sum (..))
import           Data.Text        (Text, pack)

import qualified Lhox.Lexer       as L

infixr 6 :+:
type (f :+: g) = Sum f g

newtype LitNum e = MkLitNum Double
  deriving Functor
newtype LitStr e = MkLitStr Text
  deriving Functor
data Unary e = MkUnary UnaryOperator e
  deriving Functor

data UnaryOperator = Negate
                   | Not

data BinaryOperator = Equals
                    | Different
                    | LessThan
                    | LessEqualThan
                    | GreaterThan
                    | GreaterEqualThan
                    | Plus
                    | Minus
                    | Times
                    | DividedBy

type ExprF f = Fix f
type Expr = ExprF (LitNum :+: LitStr :+: Unary)

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

showUnaryOp :: UnaryOperator -> Text
showUnaryOp Negate = "-"
showUnaryOp Not    = "!"
