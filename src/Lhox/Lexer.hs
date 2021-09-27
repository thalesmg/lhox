{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Lhox.Lexer
  (
    -- * Types
    Token(..)
  , LexError(..)
    -- * utilities
  , scanTokens
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
import           GHC.Generics              (Generic)
import           Lens.Micro                (to, (.~), (^.))
import           Lens.Micro.Mtl            (use, (%=), (.=), (<<%=))

newtype SrcPosition = MkSrcPosition { getSrcPosition :: Int }
  deriving (Eq, Show)

data LexerState =
  MkLexerState { source   :: Text
               , position :: SrcPosition
               , errors   :: Seq (LexError, SrcPosition)
               }
  deriving (Generic, Eq, Show)

newtype Lexer a =
  MkLexer {
    runLexer :: forall r. -- final result type
                LexerState -- current lexer state
             -> (LexerState -> a -> Either (LexError, SrcPosition) r) -- success continuation
             -> (LexerState -> LexError -> Either (LexError, SrcPosition) r) -- failure continuation
             -> Either (LexError, SrcPosition) r
  }

instance Functor Lexer where
  fmap f (MkLexer lxr) = MkLexer \st win lose ->
    let win' st' a = win st' (f a)
    in lxr st win' lose

instance Applicative Lexer where
  pure x = MkLexer \st win lose ->
    win st x

  (MkLexer lxrf) <*> (MkLexer lxra) = MkLexer \st winb lose ->
    let winf st' f = runLexer (fmap f (MkLexer lxra)) st' winb lose
    in lxrf st winf lose

instance Alternative Lexer where
  empty = MkLexer \st _win lose -> lose st (ErrorMessage "empty")
  (MkLexer l1) <|> (MkLexer l2) = MkLexer \st0 win0 lose0 ->
    let lose' st' err = l2 st0 win0 lose0
    in l1 st0 win0 lose'

instance Monad Lexer where
  (MkLexer lxrm) >>= f = MkLexer \st win lose ->
    let win' st' a = runLexer (f a) st' win lose
    in lxrm st win' lose

instance MonadFail Lexer where
  fail msg = MkLexer \st _win lose -> lose st (ErrorMessage (T.pack msg))

instance MonadState LexerState Lexer where
  get = MkLexer \st win _lose ->
    win st st

  put st' = MkLexer \st win _lose ->
    win st' ()

instance MonadError LexError Lexer where
  throwError e = MkLexer \st _win lose ->
    lose st e

  catchError (MkLexer lxr) handler = MkLexer \st win lose ->
    case lxr st win lose of
      Left (err, _pos) -> runLexer (handler err) st win lose
      Right a          -> Right a

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

winK :: LexerState -> a -> Either (LexError, SrcPosition) (a, LexerState)
winK st a = Right (a, st)

loseK :: LexerState -> LexError -> Either (LexError, SrcPosition) a
loseK st err = Left (err, position st)

scanTokens :: Text -> Either (Seq Token, Seq (LexError, SrcPosition)) (Seq Token)
scanTokens raw =
  case runLexer (manySeq token ignoredToken <* eof) (lexerState raw) winK loseK of
    Left err -> Left (mempty, Seq.singleton err)
    Right (ts, lexst) ->
      if null (errors lexst)
      then Right (ts Seq.|> EOF)
      else let moreErrors = squeezeRemainingErrors (lexst & #errors .~ Seq.empty)
           in Left (ts, errors lexst <> moreErrors)

squeezeRemainingErrors :: LexerState -> Seq (LexError, SrcPosition)
squeezeRemainingErrors remainingState =
  case runLexer (many ignoredToken) remainingState winK loseK of
    Left err -> Seq.singleton err
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
try (MkLexer lxr) = MkLexer \st win lose ->
  let lose' _st' _err = win st Nothing
      win' st' a = win st' (Just a)
  in lxr st win' lose'

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
