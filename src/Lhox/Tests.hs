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

import qualified Data.Sequence     as Seq
import           Test.Tasty        (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))
import           Text.RawString.QQ (r)

import qualified Lhox.Lexer        as Lexer

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "all tests"
  [ lexerTests
  ]

lexerTests :: TestTree
lexerTests = testGroup "lexer"
  [ testCase "empty string" $
      Lexer.scanTokens "" @?= Right (Seq.singleton Lexer.EOF)
  , testCase "operators" $
      Lexer.scanTokens "(){},.-+;*/ ! != = == < <= > >="
        @?= Right (Seq.fromList [ Lexer.LeftParen
                                , Lexer.RightParen
                                , Lexer.LeftBrace
                                , Lexer.RightBrace
                                , Lexer.Comma
                                , Lexer.Dot
                                , Lexer.Minus
                                , Lexer.Plus
                                , Lexer.Semicolon
                                , Lexer.Star
                                , Lexer.Slash
                                , Lexer.Bang
                                , Lexer.BangEqual
                                , Lexer.Equal
                                , Lexer.EqualEqual
                                , Lexer.Less
                                , Lexer.LessEqual
                                , Lexer.Greater
                                , Lexer.GreaterEqual
                                , Lexer.EOF
                                ])
  , testCase "single line comment" $
      Lexer.scanTokens "// some comment" @?= Right (Seq.singleton Lexer.EOF)
  , testCase "multi line comments" $
      let res = Lexer.scanTokens [r| 1 + 2 // end of line comment
                                     // lonely comment
                                     10 / 20.1
                                   |]
      in res @?= Right (Seq.fromList [ Lexer.Number 1
                                     , Lexer.Plus
                                     , Lexer.Number 2
                                     , Lexer.Number 10
                                     , Lexer.Slash
                                     , Lexer.Number 20.1
                                     , Lexer.EOF
                                     ])
  , testCase "string literal" $
      Lexer.scanTokens "\"some string\""
        @?= Right (Seq.fromList [ Lexer.StringTk "some string"
                                , Lexer.EOF
                                ])
  , testCase "string literal with newline" $
      Lexer.scanTokens "\"some \n string\""
        @?= Right (Seq.fromList [ Lexer.StringTk "some \n string"
                                , Lexer.EOF
                                ])
  , testCase "string literal spaces around" $
      Lexer.scanTokens "  \"some string\"  "
        @?= Right (Seq.fromList [ Lexer.StringTk "some string"
                                , Lexer.EOF
                                ])
  , testCase "number literal | integer" $
      Lexer.scanTokens "1234"
        @?= Right (Seq.fromList [ Lexer.Number 1234
                                , Lexer.EOF
                                ])
  , testCase "number literal | float" $
      Lexer.scanTokens "1234.5678"
        @?= Right (Seq.fromList [ Lexer.Number 1234.5678
                                , Lexer.EOF
                                ])
  , testCase "wrong number | dot before int" $
      Lexer.scanTokens ".5678"
        @?= Right (Seq.fromList [ Lexer.Dot
                                , Lexer.Number 5678
                                , Lexer.EOF
                                ])
  , testCase "wrong number | dot after int" $
      Lexer.scanTokens "1234."
        @?= Right (Seq.fromList [ Lexer.Number 1234
                                , Lexer.Dot
                                , Lexer.EOF
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
        @?= Right (Seq.fromList [ Lexer.And
                                , Lexer.Class
                                , Lexer.Else
                                , Lexer.FalseTk
                                , Lexer.For
                                , Lexer.Fun
                                , Lexer.If
                                , Lexer.Nil
                                , Lexer.Or
                                , Lexer.Print
                                , Lexer.Return
                                , Lexer.Super
                                , Lexer.This
                                , Lexer.TrueTk
                                , Lexer.Var
                                , Lexer.While
                                , Lexer.EOF
                                ])
  , testCase "identifier | user defined" $
      Lexer.scanTokens "foo _bar foo123 some_foo"
        @?= Right (Seq.fromList [ Lexer.Identifier "foo"
                                , Lexer.Identifier "_bar"
                                , Lexer.Identifier "foo123"
                                , Lexer.Identifier "some_foo"
                                , Lexer.EOF
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
        @?= Left ( Seq.fromList [ Lexer.Star
                                , Lexer.Slash
                                , Lexer.Dot
                                , Lexer.Minus
                                , Lexer.Plus
                                , Lexer.Comma
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 2
                                 )
                 )
  , testCase "invalid token before valid" $
      Lexer.scanTokens "@*/.\n-+,"
        @?= Left ( Seq.fromList [ Lexer.Star
                                , Lexer.Slash
                                , Lexer.Dot
                                , Lexer.Minus
                                , Lexer.Plus
                                , Lexer.Comma
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 1
                                 )
                 )
  , testCase "invalid token between valid" $
      Lexer.scanTokens "*/.\n@-+,"
        @?= Left ( Seq.fromList [ Lexer.Star
                                , Lexer.Slash
                                , Lexer.Dot
                                , Lexer.Minus
                                , Lexer.Plus
                                , Lexer.Comma
                                ]
                 , Seq.singleton ( Lexer.UnexpectedChar '@'
                                 , Lexer.MkSrcPosition 2
                                 )
                 )
  , testCase "unterminated string" $
      Lexer.scanTokens [r|- "unterminated |]
        @?= Left ( Seq.fromList [ Lexer.Minus
                                , Lexer.EOF
                                ]
                 , Seq.singleton ( Lexer.UnterminatedString
                                 , Lexer.MkSrcPosition 1
                                 )
                 )
  ]
