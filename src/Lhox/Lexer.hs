{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Lhox.Lexer
  (
    -- * Types
    Token(..)
  , LexError(..)
    -- * utilities
  , scanTokens
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative (Alternative(..))
import Control.Monad (when)
import Control.Monad.State (StateT(..))
import Control.Monad.State.Class (MonadState)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor (($>))
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, maybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, uncons)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro ((^.), (.~), to)
import Lens.Micro.Mtl ((.=), (%=), (<<%=), use)
import Control.Monad.Except

newtype SrcPosition = MkSrcPosition { getSrcPosition :: Int }
  deriving (Eq, Show)

data LexerState =
  MkLexerState { source :: Text
               , position :: SrcPosition
               , errors :: Seq (LexError, SrcPosition)
               }
  deriving (Generic, Eq, Show)

{-
-- TODO
newtype Lexer' a =
  MkLexer' {
    runLexer' :: Text -- remoining source
              -> SrcPosition -- current position in source code
              -> Seq Token -- tokens collected so far
              -> (Text -> SrcPosition -> Seq Token -> a -> Either LexError (Seq Token)) -- success continuation
              -> (Text -> SrcPosition -> Seq Token -> Text -> Either LexError (Seq Token)) -- failure continuation
              -> Either LexError (Seq Token)
  }
-}

newtype Lexer a = MkLexer { runLexer :: LexerState -> Either LexError (a, LexerState) }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState LexerState
  , MonadError LexError
  ) via (StateT LexerState (Either LexError))

instance Alternative Lexer where
  empty = MkLexer \_ -> Left (ErrorMessage "empty")
  (MkLexer l1) <|> (MkLexer l2) = MkLexer \s0 ->
    case (l1 s0, l2 s0) of
      (res1, Left _) -> res1
      (Left _, res2) -> res2
      (r1@(Right (a1, s1)), r2@(Right (a2, s2))) ->
        if T.length (source s1) <= T.length (source s2)
        then r1
        else r2

instance MonadFail Lexer where
  fail msg = MkLexer \_ -> Left (ErrorMessage (T.pack msg))

data Token =
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier Text
  | StringTk Text
  | Number Double
  | And
  | Class
  | Else
  | FalseTk
  | TrueTk
  | If
  | Fun
  | For
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | Var
  | While
  | EOF
  deriving (Eq, Show)

data LexError = ErrorMessage Text
              | UnexpectedEOF
              | UnexpectedChar Char
              | UnterminatedString
  deriving (Eq, Show)

scanTokens :: Text -> Either (Seq Token, Seq (LexError, SrcPosition)) (Seq Token)
scanTokens raw =
  case runLexer (manySeq token ignoredToken <* eof) (lexerState raw) of
    Left err -> Left (mempty, Seq.singleton (err, MkSrcPosition 0))
    Right (ts, lexst) ->
      if null (errors lexst)
      then Right (ts Seq.|> EOF)
      else let moreErrors = squeezeRemainingErrors (lexst & #errors .~ Seq.empty)
           in Left (ts, errors lexst <> moreErrors)

squeezeRemainingErrors :: LexerState -> Seq (LexError, SrcPosition)
squeezeRemainingErrors remainingState =
  case runLexer (many ignoredToken) remainingState of
    Left err -> Seq.singleton (err, position remainingState)
    Right (_, lexst) ->
      if T.null (source lexst)
      then errors lexst
      else error $ mconcat [ "the impossible has happened!\n"
                           , "state: "
                           , show lexst
                           ]

lexerState :: Text -> LexerState
lexerState src = MkLexerState { source = src
                              , position = MkSrcPosition 1
                              , errors = Seq.empty
                              }

increaseLine :: Lexer ()
increaseLine = #position %= \(MkSrcPosition l) -> MkSrcPosition (l + 1)

peek :: Lexer (Maybe Char)
peek = use (#source . to (fmap fst . uncons))

advance :: Lexer Char
advance = do
  mcs <- use (#source . to uncons)
  case mcs of
    Nothing -> throwError UnexpectedEOF
    Just (c, src') -> do
      #source .= src'
      when (c == '\n') increaseLine
      pure c

isAtEnd :: Lexer Bool
isAtEnd = isNothing <$> peek

appendError :: LexError -> Lexer ()
appendError err = do
  pos <- use #position
  #errors %= (Seq.|> (err, pos))

satisfy :: (Char -> Bool) -> Lexer Char
satisfy p = do
  mc <- peek
  case mc of
    Nothing ->
      throwError UnexpectedEOF
    Just c ->
      if p c
      then advance
      else fail "satisfy"

choice :: [Lexer a] -> Lexer a
choice = foldr (<|>) (fail "choice")

try :: Lexer a -> Lexer (Maybe a)
try (MkLexer lxr) = MkLexer \s ->
  case lxr s of
    Left _ ->
      Right (Nothing, s)
    Right (a, s') ->
      Right (Just a, s')

manySeq :: Lexer a -> Lexer () -> Lexer (Seq a)
manySeq lxr unk = do
  atEnd <- isAtEnd
  if atEnd
  then pure mempty
  else do
    ma <- try lxr
    case ma of
      Nothing -> do
        unk
        manySeq lxr unk
      Just a -> do
        rest <- manySeq lxr unk
        pure (a Seq.<| rest)

-- TODO: what to call this? does this make sense?
firstOrAfter :: Lexer a -> Lexer () -> Lexer a
firstOrAfter lxr unk = do
  ma <- try lxr
  case ma of
    Nothing -> do
      unk
      firstOrAfter lxr unk
    Just a ->
      pure a

lexeme :: Lexer () -> Lexer a -> Lexer a
lexeme spc lxr = lxr <* spc

token :: Lexer Token
token =
  lexeme (try (many whitespace) $> ()) $
  choice [ leftParen
         , rightParen
         , leftBrace
         , rightBrace
         , comma
         , dot
         , semicolon
         , minus
         , plus
         , star
         , slash
         , bangOrBangEqual
         , equalOrEqualEqual
         , lessOrLessEqual
         , greaterOrGreaterEqual
         , stringLiteral
         , numberLiteral
         , identifier
         ]

ignoredToken :: Lexer ()
ignoredToken =
  choice [ whitespace
         , comment
         , unknown
         ]

whitespace :: Lexer ()
whitespace = satisfy (`elem` [' ', '\t', '\r', '\n']) $> ()

eol :: Lexer ()
eol = single '\n' $> ()

eof :: Lexer Token
eof = do
  atEnd <- isAtEnd
  guard atEnd
  pure EOF

anyChar :: Lexer Char
anyChar = satisfy (const True)

takeWhile :: (Char -> Bool) -> Lexer Text
takeWhile p = go (TLB.fromText "")
  where
    go acc = do
      mc <- peek
      case mc of
        -- does not fail on EOF because it has seen everything
        -- successfully
        Nothing ->
          pure (TL.toStrict (TLB.toLazyText acc))
        Just c ->
          if p c
          then advance >> go (acc <> TLB.singleton c)
          else pure (TL.toStrict (TLB.toLazyText acc))

takeUntilM :: (Char -> Lexer Bool) -> Lexer Text
takeUntilM p = go (TLB.fromText "")
  where
    go acc = do
      mc <- peek
      case mc of
        -- fails on lack of input because it did not get to see the
        -- end condition
        Nothing -> fail "takeUntil"
        Just c -> do
          sat <- p c
          if sat
          then pure (TL.toStrict (TLB.toLazyText acc))
          else advance >> go (acc <> TLB.singleton c)

takeUntil :: (Char -> Bool) -> Lexer Text
takeUntil p = takeUntilM (pure <$> p)

unknown :: Lexer ()
unknown = do
  c <- anyChar
  appendError (UnexpectedChar c)

single :: Char -> Lexer Char
single c = satisfy (== c)

string :: Text -> Lexer Text
string txt = case uncons txt of
  Nothing -> pure ""
  Just (c, txt') -> do
    single c
    string txt'
    pure txt

leftParen :: Lexer Token
leftParen = single '(' $> LeftParen

rightParen :: Lexer Token
rightParen = single ')' $> RightParen

leftBrace :: Lexer Token
leftBrace = single '{' $> LeftBrace

rightBrace :: Lexer Token
rightBrace = single '}' $> RightBrace

comma :: Lexer Token
comma = single ',' $> Comma

dot :: Lexer Token
dot = single '.' $> Dot

semicolon :: Lexer Token
semicolon = single ';' $> Semicolon

minus :: Lexer Token
minus = single '-' $> Minus

plus :: Lexer Token
plus = single '+' $> Plus

star :: Lexer Token
star = single '*' $> Star

slash :: Lexer Token
slash = do
  single '/'
  c <- peek
  guard (c /= Just '/')
  pure Slash

bangOrBangEqual :: Lexer Token
bangOrBangEqual =
      (single '!' >> single '=' $> BangEqual)
  <|> (single '!' $> Bang)

equalOrEqualEqual :: Lexer Token
equalOrEqualEqual =
      (single '=' >> single '=' $> EqualEqual)
  <|> (single '=' $> Equal)

lessOrLessEqual :: Lexer Token
lessOrLessEqual =
      (single '<' >> single '=' $> LessEqual)
  <|> (single '<' $> Less)

greaterOrGreaterEqual :: Lexer Token
greaterOrGreaterEqual =
      (single '>' >> single '=' $> GreaterEqual)
  <|> (single '>' $> Greater)

comment :: Lexer ()
comment = do
  string "//"
  takeWhile (/= '\n')
  pure ()

stringLiteral :: Lexer Token
stringLiteral = do
  single '"'
  txt <- takeWhile (/= '"')
  end <- isAtEnd
  if end
    then do
      appendError UnterminatedString
      pure EOF
    else do
      single '"'
      pure (StringTk txt)

digit :: Lexer Char
digit = satisfy isDigit

numberLiteral :: Lexer Token
numberLiteral = do
  integerPart <- some digit
  mFractionalPart <- try do
    single '.'
    some digit
  let fractionalPart = maybe "" ('.' :) mFractionalPart
      num = read $ integerPart <> fractionalPart
  pure (Number num)

keywords :: Map Text Token
keywords =
  M.fromList [ ("and", And)
             , ("class", Class)
             , ("else", Else)
             , ("false", FalseTk)
             , ("for", For)
             , ("fun", Fun)
             , ("if", If)
             , ("nil", Nil)
             , ("or", Or)
             , ("print", Print)
             , ("return", Return)
             , ("super", Super)
             , ("this", This)
             , ("true", TrueTk)
             , ("var", Var)
             , ("while", While)
             ]

identifier :: Lexer Token
identifier = do
  t <- satisfy (\c -> isAlpha c || c == '_')
  xt <- takeWhile (\c -> isAlphaNum c || c == '_')
  let identRaw = T.singleton t <> xt
      mkeyword = M.lookup identRaw keywords
      ident = fromMaybe (Identifier identRaw) mkeyword
  pure ident
