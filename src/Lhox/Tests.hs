{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{- |
  Due to a "bug" (feature?) of cabal <= 3.6.1.0, one cannot load both
  the library and its tests into @cabal repl@. Also, cabal does not
  "see" the changes made to the library during the REPL session. Which
  means you can break the lib and the tests will still pass.

  https://github.com/ndmitchell/ghcid/issues/320

  https://github.com/ndmitchell/ghcid/issues/208

  It seems that some work is being done to address that:

  https://gist.github.com/fendor/5b26d36538787c8c2ed8c6eb6e68541f

  Stack does not seem to be affected by this issue, since I have
  always used it without hitting this problem.

  So, I decided to stick with Gabriella Gonzalez's tip and went with a
  "God" stanza...

  https://www.haskellforall.com/2021/05/module-organization-guidelines-for.html

  An ad hoc alternative seems to be Neil Mitchell's idea of a @.ghci@
  file that loads everything. But I still need to research how that
  works.

  https://github.com/ndmitchell/ghcid/issues/320#issuecomment-641593608

  https://github.com/ndmitchell/ghcid/blob/a1a83a3385e0d67c9875492aaa9cbbab2322dbdc/.ghci
-}
module Lhox.Tests where

import           Data.Fix              (Fix (Fix))
import           Data.Functor.Foldable (cata)
import           Data.Functor.Sum      (Sum (..))
import qualified Data.Sequence         as Seq
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Text.RawString.QQ     (r)

import qualified Lhox.Lexer            as Lexer
import qualified Lhox.Parser           as Parser

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests"
  [ lexerTests
  , aprintTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer"
  [ testCase "empty string" $
      Lexer.scanTokens "" @?= Right (Seq.singleton (Lexer.MkToken Lexer.EOF
                                                      (Lexer.MkSrcPosition 1)))
  , testCase "operators" $
      Lexer.scanTokens "(){},.-+;*/ ! != = == < <= > >="
        @?= Right (Seq.fromList [ Lexer.MkToken Lexer.LeftParen
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.RightParen
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.LeftBrace
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.RightBrace
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Comma
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Minus
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Plus
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Semicolon
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Star
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Slash
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Bang
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.BangEqual
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Equal
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EqualEqual
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Less
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.LessEqual
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Greater
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.GreaterEqual
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "single line comment" $
      Lexer.scanTokens "// some comment" @?= Right (Seq.singleton
                                                    (Lexer.MkToken Lexer.EOF
                                                       (Lexer.MkSrcPosition 1)))
  , testCase "multi line comments" $
      let res = Lexer.scanTokens [r| 1 + 2 // end of line comment
                                     // lonely comment
                                     10 / 20.1
                                   |]
      in res @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.Number 1)
                                         (Lexer.MkSrcPosition 1)
                                     , Lexer.MkToken Lexer.Plus
                                         (Lexer.MkSrcPosition 1)
                                     , Lexer.MkToken (Lexer.Number 2)
                                         (Lexer.MkSrcPosition 1)
                                     , Lexer.MkToken (Lexer.Number 10)
                                         (Lexer.MkSrcPosition 3)
                                     , Lexer.MkToken Lexer.Slash
                                         (Lexer.MkSrcPosition 3)
                                     , Lexer.MkToken (Lexer.Number 20.1)
                                         (Lexer.MkSrcPosition 3)
                                     , Lexer.MkToken Lexer.EOF
                                         (Lexer.MkSrcPosition 4)
                                     ])
  , testCase "string literal" $
      Lexer.scanTokens "\"some string\""
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.StringTk "some string")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "string literal with newline" $
      Lexer.scanTokens "\"some \n string\""
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.StringTk "some \n string")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 2)
                                ])
  , testCase "string literal spaces around" $
      Lexer.scanTokens "  \"some string\"  "
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.StringTk "some string")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "number literal | integer" $
      Lexer.scanTokens "1234"
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.Number 1234)
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "number literal | float" $
      Lexer.scanTokens "1234.5678"
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.Number 1234.5678)
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "wrong number | dot before int" $
      Lexer.scanTokens ".5678"
        @?= Right (Seq.fromList [ Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken (Lexer.Number 5678)
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "wrong number | dot after int" $
      Lexer.scanTokens "1234."
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.Number 1234)
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "identifier | reserved" $
      let input = [r| and
                      class
                      else
                      false
                      for
                      fun
                      if
                      nil
                      or
                      print
                      return
                      super
                      this
                      true
                      var
                      while
                    |]
      in Lexer.scanTokens input
        @?= Right (Seq.fromList [ Lexer.MkToken Lexer.And
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Class
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Else
                                    (Lexer.MkSrcPosition 3)
                                , Lexer.MkToken Lexer.FalseTk
                                    (Lexer.MkSrcPosition 4)
                                , Lexer.MkToken Lexer.For
                                    (Lexer.MkSrcPosition 5)
                                , Lexer.MkToken Lexer.Fun
                                    (Lexer.MkSrcPosition 6)
                                , Lexer.MkToken Lexer.If
                                    (Lexer.MkSrcPosition 7)
                                , Lexer.MkToken Lexer.Nil
                                    (Lexer.MkSrcPosition 8)
                                , Lexer.MkToken Lexer.Or
                                    (Lexer.MkSrcPosition 9)
                                , Lexer.MkToken Lexer.Print
                                    (Lexer.MkSrcPosition 10)
                                , Lexer.MkToken Lexer.Return
                                    (Lexer.MkSrcPosition 11)
                                , Lexer.MkToken Lexer.Super
                                    (Lexer.MkSrcPosition 12)
                                , Lexer.MkToken Lexer.This
                                    (Lexer.MkSrcPosition 13)
                                , Lexer.MkToken Lexer.TrueTk
                                    (Lexer.MkSrcPosition 14)
                                , Lexer.MkToken Lexer.Var
                                    (Lexer.MkSrcPosition 15)
                                , Lexer.MkToken Lexer.While
                                    (Lexer.MkSrcPosition 16)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 17)
                                ])
  , testCase "identifier | user defined" $
      Lexer.scanTokens "foo _bar foo123 some_foo"
        @?= Right (Seq.fromList [ Lexer.MkToken (Lexer.Identifier "foo")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken (Lexer.Identifier "_bar")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken (Lexer.Identifier "foo123")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken (Lexer.Identifier "some_foo")
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ])
  , testCase "single invalid token" $
      Lexer.scanTokens " @ "
        @?= Left ( Seq.empty
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 1
                                 )
                 )
  , testCase "invalid token after valid" $
      Lexer.scanTokens "*/.\n-+,@"
        @?= Left ( Seq.fromList [ Lexer.MkToken Lexer.Star
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Slash
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Minus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Plus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Comma
                                    (Lexer.MkSrcPosition 2)
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 2
                                 )
                 )
  , testCase "invalid token before valid" $
      Lexer.scanTokens "@*/.\n-+,"
        @?= Left ( Seq.fromList [ Lexer.MkToken Lexer.Star
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Slash
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Minus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Plus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Comma
                                    (Lexer.MkSrcPosition 2)
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 1
                                 )
                 )
  , testCase "invalid token between valid" $
      Lexer.scanTokens "*/.\n@-+,"
        @?= Left ( Seq.fromList [ Lexer.MkToken Lexer.Star
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Slash
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Dot
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.Minus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Plus
                                    (Lexer.MkSrcPosition 2)
                                , Lexer.MkToken Lexer.Comma
                                    (Lexer.MkSrcPosition 2)
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 2
                                 )
                 )
  , testCase "unterminated string" $
      Lexer.scanTokens [r|- "unterminated |]
        @?= Left ( Seq.fromList [ Lexer.MkToken Lexer.Minus
                                    (Lexer.MkSrcPosition 1)
                                , Lexer.MkToken Lexer.EOF
                                    (Lexer.MkSrcPosition 1)
                                ]
                 , Seq.singleton ( Lexer.UnterminatedString
                                 , Lexer.MkSrcPosition 1
                                 )
                 )
  ]

aprintTests :: TestTree
aprintTests = testGroup "aprint"
  [ testCase "literal number" $
      let expr :: Parser.Expr
          expr = Fix (InL (Parser.MkLitNum 1.23))
      in cata Parser.aprint expr @?= "1.23"
  , testCase "literal string" $
      let expr :: Parser.Expr
          expr = Fix (InR (InL (Parser.MkLitStr "olá")))
      in cata Parser.aprint expr @?= "\"olá\""
  , testCase "unary | negate" $
      let expr :: Parser.Expr
          expr = Fix (InR (InR (Parser.MkUnary Parser.Negate (Fix (InL (Parser.MkLitNum 1.23))))))
      in cata Parser.aprint expr @?= "-1.23"
  , testCase "unary | not" $
      let expr :: Parser.Expr
          expr = Fix (InR (InR (Parser.MkUnary Parser.Not (Fix (InL (Parser.MkLitNum 1.23))))))
      in cata Parser.aprint expr @?= "!1.23"
  ]
