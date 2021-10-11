{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lhox.Parser where

import           Data.Fix             (Fix (Fix))
import           Data.Function        (on)
import           Data.Functor.Classes (Eq1 (..), Show1 (..))
import           Data.Functor.Sum     (Sum (InL, InR))
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           Data.Text            (Text, pack)
import           Data.Void            (Void)

import qualified Lhox.Lexer           as L
import           Lhox.YoctoParsec     (choice)
import qualified Lhox.YoctoParsec     as YParsec

type Position = Int
type ParserState = YParsec.ParsecState (Seq L.Token) Position Void
type ParserError = YParsec.ParserError Void
type Parser a = YParsec.Parsec (Seq L.Token) Position Void a

instance YParsec.Stream (Seq L.Token) where
  type Element (Seq L.Token) = L.Token
  type Elements (Seq L.Token) = Seq L.Token
  lookahead ts = case Seq.viewl ts of
    Seq.EmptyL   -> Nothing
    a Seq.:< ts' -> Just (a, ts')

infixr 6 :+:
type (f :+: g) = Sum f g

newtype LitNum e = MkLitNum Double
  deriving (Eq, Functor)
newtype LitStr e = MkLitStr Text
  deriving (Eq, Functor)
data Unary e = MkUnary UnaryOperator e
  deriving (Eq, Functor)

data UnaryOperator = Negate
                   | Not
  deriving (Eq, Show)

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
  deriving (Eq, Show)

instance Eq1 LitNum where
  liftEq _ (MkLitNum x1) (MkLitNum x2) = x1 == x2

instance Eq1 LitStr where
  liftEq _ (MkLitStr x1) (MkLitStr x2) = x1 == x2

instance Eq1 Unary where
  liftEq f (MkUnary o1 e1) (MkUnary o2 e2) =
    o1 == o2 && f e1 e2

instance Show1 LitNum where
  liftShowsPrec sp sl d (MkLitNum x) = showsPrec d x

instance Show1 LitStr where
  liftShowsPrec sp sl d (MkLitStr s) = showsPrec d s

instance Show1 Unary where
  liftShowsPrec sp sl d (MkUnary op e) =
    showsPrec d op . (<>) " " . sp d e

type ExprF f = Fix f
type Expr = ExprF (LitNum :+: LitStr :+: Unary)

-- * Expression helpers

litNum :: Double -> Expr
litNum = Fix . InL . MkLitNum

litStr :: Text -> Expr
litStr = Fix . InR . InL . MkLitStr

unary :: UnaryOperator -> Expr -> Expr
unary op e = Fix . InR . InR $ MkUnary op e

-- * Parsing utilities

parse :: Seq L.Token -> Either (Seq (ParserError, Position)) Expr
parse tks =
  case YParsec.runParsec primary (parserState tks)
         YParsec.winK YParsec.loseK of
    Left err             -> Left (Seq.singleton err)
    Right (expr, pstate) -> Right expr

parserState :: Seq L.Token -> ParserState
parserState tks = YParsec.MkParsecState { YParsec.source = tks
                                        , YParsec.position = 1
                                        , YParsec.errors = Seq.empty
                                        }

succPos :: L.Token -> Position -> Position
succPos _ pos = pos + 1

satisfy :: (L.Token -> Bool) -> Parser L.Token
satisfy = YParsec.satisfy succPos

advance :: Parser L.Token
advance = YParsec.advance succPos

single :: L.TokenType -> Parser L.TokenType
single tt = L.tokenType <$> satisfy (\t -> L.tokenType t == tt)

primary :: Parser Expr
primary =
  choice [ number
         , string
         ]

number :: Parser Expr
number = do
  tt <- L.tokenType <$> advance
  case tt of
    L.Number x ->
      pure (litNum x)
    _ ->
      fail "number"

string :: Parser Expr
string = do
  tt <- L.tokenType <$> advance
  case tt of
    L.StringTk s ->
      pure (litStr s)
    _ ->
      fail "string"
