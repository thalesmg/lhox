{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Lhox.YoctoParsec
  (
    -- * Types
    SrcPosition(..)
  , ParserError(..)
  , CommonError(..)
  , ParsecState(..)
  , Parsec(..)
  , Stream(..)
    -- * Helpers
  , winK
  , loseK
  , try
  , getParsecPosition
  , throwParserError
  , peek
  , advance
  , satisfy
  , isAtEnd
  )
  where

import           Control.Applicative       (Alternative (..))
import           Control.Monad.Except      (MonadError (..))
import           Control.Monad.State.Class (MonadState (..))
import           Data.Generics.Labels      ()
import           Data.Maybe                (fromMaybe, isNothing, maybe)
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text, uncons)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Lens.Micro                (to)
import           Lens.Micro.Mtl            (use, (%=), (.=))


newtype SrcPosition = MkSrcPosition { getSrcPosition :: Int }
  deriving (Eq, Show)

newtype ParserError e = MkParserError { getParserError :: Either CommonError e }
  deriving (Eq, Show)

data CommonError = ErrorMessage Text
                 | UnexpectedEOF
                 | UnexpectedChar Char
                 | UnterminatedString
  deriving (Eq, Show)


data ParsecState s p e =
  MkParsecState { source   :: s
                , position :: p
                , errors   :: Seq (ParserError e, p)
                }
  deriving (Generic, Eq, Show)

newtype Parsec s p e a =
  MkParsec {
    runParsec :: forall r. -- final result type
                ParsecState s p e -- current lexer state
             -> (ParsecState s p e -> a -> Either (ParserError e, p) r) -- success continuation
             -> (ParsecState s p e -> ParserError e -> Either (ParserError e, p) r) -- failure continuation
             -> Either (ParserError e, p) r
  }

instance Functor (Parsec s p e) where
  fmap f (MkParsec lxr) = MkParsec \st win lose ->
    let win' st' a = win st' (f a)
    in lxr st win' lose

instance Applicative (Parsec s p e) where
  pure x = MkParsec \st win lose ->
    win st x

  (MkParsec lxrf) <*> (MkParsec lxra) = MkParsec \st winb lose ->
    let winf st' f = runParsec (fmap f (MkParsec lxra)) st' winb lose
    in lxrf st winf lose

instance Alternative (Parsec s p e) where
  empty = MkParsec \st _win lose -> lose st (MkParserError . Left . ErrorMessage $ "empty")
  (MkParsec l1) <|> (MkParsec l2) = MkParsec \st0 win0 lose0 ->
    let lose' st' err = l2 st0 win0 lose0
    in l1 st0 win0 lose'

instance Monad (Parsec s p e) where
  (MkParsec lxrm) >>= f = MkParsec \st win lose ->
    let win' st' a = runParsec (f a) st' win lose
    in lxrm st win' lose

instance MonadFail (Parsec s p e) where
  fail msg = MkParsec \st _win lose -> lose st (MkParserError . Left . ErrorMessage . T.pack $ msg)

instance MonadState (ParsecState s p e) (Parsec s p e) where
  get = MkParsec \st win _lose ->
    win st st

  put st' = MkParsec \st win _lose ->
    win st' ()

instance MonadError (ParserError e) (Parsec s p e) where
  throwError e = MkParsec \st _win lose ->
    lose st e

  catchError (MkParsec lxr) handler = MkParsec \st win lose ->
    case lxr st win lose of
      Left (err, _pos) -> runParsec (handler err) st win lose
      Right a          -> Right a

class Stream s where
  type Element s
  type Elements s
  lookahead :: s -> Maybe (Element s, s)

-- | Success final continuation
winK :: ParsecState s p e -> a -> Either (ParserError e, p) (a, ParsecState s p e)
winK st a = Right (a, st)

-- | Failure final continuation
loseK :: ParsecState s p e -> ParserError e -> Either (ParserError e, p) a
loseK st err = Left (err, position st)

-- * Helpers

try :: Parsec s p e a -> Parsec s p e (Maybe a)
try (MkParsec lxr) = MkParsec \st win lose ->
  let lose' _st' _err = win st Nothing
      win' st' a = win st' (Just a)
  in lxr st win' lose'

getParsecPosition :: Parsec s p e p
getParsecPosition = MkParsec \st win _lose -> win st (position st)

throwParserError :: CommonError -> Parsec s p e a
throwParserError = throwError . MkParserError . Left

peek :: (Stream s) => Parsec s e p (Maybe (Element s))
peek = fmap fst <$> use (#source . to lookahead)

advance :: (Stream s) => (Element s -> p -> p) -> Parsec s p e (Element s)
advance updatePos = do
  mcs <- use (#source . to lookahead)
  case mcs of
    Nothing -> throwParserError UnexpectedEOF
    Just (c, src') -> do
      #source .= src'
      #position %= updatePos c
      pure c

satisfy :: (Stream s) => (Element s -> p -> p) -> (Element s -> Bool) -> Parsec s p e (Element s)
satisfy updatePos p = do
  mc <- use (#source . to lookahead)
  case mc of
    Nothing ->
      throwParserError UnexpectedEOF
    Just (c, _) ->
      if p c
      then advance updatePos
      else fail "satisfy"

isAtEnd :: (Stream s) => Parsec s p e Bool
isAtEnd = isNothing <$> peek
