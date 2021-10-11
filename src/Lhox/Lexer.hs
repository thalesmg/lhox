{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Lhox.Lexer
  (
    -- * Types
    Token(..)
  , TokenType(..)
  , LexError(..)
    -- * utilities
  , scanTokens
    -- * test utilities
  , toLexError
  ) where

import           Prelude                   hiding (takeWhile)

import           Control.Applicative       (Alternative (..))
import           Control.Monad             (when)
import           Control.Monad.Except
import           Control.Monad.State       (StateT (..))
import           Control.Monad.State.Class (MonadState (..))
import           Data.Char                 (isAlpha, isAlphaNum, isDigit)
import           Data.Foldable             (foldl')
import           Data.Function             ((&))
import           Data.Functor              (($>))
import           Data.Generics.Labels      ()
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe, isNothing, maybe)
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text, uncons)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as TLB
import           Data.Void                 (Void)
import           GHC.Generics              (Generic)
import           Lens.Micro                (to, (.~), (^.))
import           Lens.Micro.Mtl            (use, (%=), (.=), (<<%=))

import           Lhox.YoctoParsec          (isAtEnd, peek, try)
import qualified Lhox.YoctoParsec          as YParsec


type Position = YParsec.SrcPosition
type LexerState = YParsec.ParsecState Text Position Void
type Lexer a = YParsec.Parsec Text Position Void a
type LexError = YParsec.ParserError Void

data Token = MkToken { tokenType :: TokenType
                     , tokenPos  :: Position
                     }
  deriving (Eq, Show)

data TokenType =
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

instance YParsec.Stream Text where
  type Element Text = Char
  type Elements Text = Text
  lookahead = uncons

succPos :: Char -> Position -> Position
succPos '\n' (YParsec.MkSrcPosition n) = YParsec.MkSrcPosition $ n + 1
succPos _ pos                          = pos

advance :: Lexer Char
advance = YParsec.advance succPos

satisfy :: (Char -> Bool) -> Lexer Char
satisfy = YParsec.satisfy succPos

scanTokens :: Text -> Either (Seq Token, Seq (LexError, YParsec.SrcPosition)) (Seq Token)
scanTokens raw =
  case YParsec.runParsec (manySeq token ignoredToken <* eof)
         (lexerState raw) YParsec.winK YParsec.loseK of
    Left err -> Left (mempty, Seq.singleton err)
    Right (ts, lexst) ->
      if null (YParsec.errors lexst)
      then Right (ts Seq.|> MkToken EOF (YParsec.position lexst))
      else let moreErrors = squeezeRemainingErrors (lexst & #errors .~ Seq.empty)
           in Left (ts, YParsec.errors lexst <> moreErrors)

squeezeRemainingErrors :: LexerState -> Seq (LexError, YParsec.SrcPosition)
squeezeRemainingErrors remainingState =
  case YParsec.runParsec (many ignoredToken) remainingState YParsec.winK YParsec.loseK of
    Left err -> Seq.singleton err
    Right (_, lexst) ->
      if T.null (YParsec.source lexst)
      then YParsec.errors lexst
      else error $ mconcat [ "the impossible has happened!\n"
                           , "state: "
                           , show lexst
                           ]

lexerState :: Text -> LexerState
lexerState src = YParsec.MkParsecState { YParsec.source = src
                                       , YParsec.position = YParsec.MkSrcPosition 1
                                       , YParsec.errors = Seq.empty
                                       }

increaseLine :: Lexer ()
increaseLine = #position %= \(YParsec.MkSrcPosition l) -> YParsec.MkSrcPosition (l + 1)

-- FIXME: I believe I'd have to use a MonadParse like MegaParsec's to
-- link this with YParsec...
-- peek :: Lexer (Maybe Char)
-- peek = use (#source . to (fmap fst . uncons))

-- FIXME: I believe I'd have to use a MonadParse like MegaParsec's to
-- link this with YParsec...
-- advance :: Lexer Char
-- advance = do
--   mcs <- use (#source . to uncons)
--   case mcs of
--     Nothing -> YParsec.throwParserError YParsec.UnexpectedEOF
--     Just (c, src') -> do
--       #source .= src'
--       when (c == '\n') increaseLine
--       pure c

-- isAtEnd :: Lexer Bool
-- isAtEnd = isNothing <$> YParsec.peek

toLexError :: YParsec.CommonError -> LexError
toLexError = YParsec.MkParserError . Left

appendError :: YParsec.CommonError -> Lexer ()
appendError err = do
  pos <- use #position
  #errors %= (Seq.|> (toLexError err, pos))

-- satisfy :: (Char -> Bool) -> Lexer Char
-- satisfy p = do
--   mc <- peek
--   case mc of
--     Nothing ->
--       YParsec.throwParserError YParsec.UnexpectedEOF
--     Just c ->
--       if p c
--       then advance
--       else fail "satisfy"

choice :: [Lexer a] -> Lexer a
choice = foldr (<|>) (fail "choice")

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

mkToken :: TokenType -> Lexer Token
mkToken tt = MkToken tt <$> YParsec.getParsecPosition

eol :: Lexer ()
eol = single '\n' $> ()

eof :: Lexer Token
eof = do
  atEnd <- isAtEnd
  guard atEnd
  mkToken EOF

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
        Nothing -> fail "takeUntilM"
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
  appendError (YParsec.UnexpectedChar c)

-- FIXME: I believe I'd have to use a MonadParse like MegaParsec's to
-- link this with YParsec...
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
leftParen = single '(' *> mkToken LeftParen

rightParen :: Lexer Token
rightParen = single ')' *> mkToken RightParen

leftBrace :: Lexer Token
leftBrace = single '{' *> mkToken LeftBrace

rightBrace :: Lexer Token
rightBrace = single '}' *> mkToken RightBrace

comma :: Lexer Token
comma = single ',' *> mkToken Comma

dot :: Lexer Token
dot = single '.' *> mkToken Dot

semicolon :: Lexer Token
semicolon = single ';' *> mkToken Semicolon

minus :: Lexer Token
minus = single '-' *> mkToken Minus

plus :: Lexer Token
plus = single '+' *> mkToken Plus

star :: Lexer Token
star = single '*' *> mkToken Star

slash :: Lexer Token
slash = do
  single '/'
  c <- peek
  guard (c /= Just '/')
  mkToken Slash

bangOrBangEqual :: Lexer Token
bangOrBangEqual =
      (single '!' >> single '=' *> mkToken BangEqual)
  <|> (single '!' *> mkToken Bang)

equalOrEqualEqual :: Lexer Token
equalOrEqualEqual =
      (single '=' >> single '=' *> mkToken EqualEqual)
  <|> (single '=' *> mkToken Equal)

lessOrLessEqual :: Lexer Token
lessOrLessEqual =
      (single '<' >> single '=' *> mkToken LessEqual)
  <|> (single '<' *> mkToken Less)

greaterOrGreaterEqual :: Lexer Token
greaterOrGreaterEqual =
      (single '>' >> single '=' *> mkToken GreaterEqual)
  <|> (single '>' *> mkToken Greater)

comment :: Lexer ()
comment = do
  string "//"
  takeWhile (/= '\n')
  pure ()

stringLiteral :: Lexer Token
stringLiteral = do
  single '"'
  startPos <- YParsec.getParsecPosition
  txt <- takeWhile (/= '"')
  end <- isAtEnd
  if end
    then do
      appendError YParsec.UnterminatedString
      mkToken EOF
    else do
      single '"'
      pure $ MkToken (StringTk txt) startPos

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
  mkToken (Number num)

keywords :: Map Text TokenType
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
  mkToken ident
