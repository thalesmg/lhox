module Lhox.Lexer
  (
    -- * Types
    Token(..)
  , LexError(..)
    -- * utilities
  , scanTokens
  ) where

import Data.Text (Text)

data Token = MkToken
  deriving (Eq, Show)

data LexError = MkLexError
  deriving (Eq, Show)

scanTokens :: Text -> Either LexError [Token]
scanTokens raw = pure []
