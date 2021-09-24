{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Sequence as Seq
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Lhox.Lexer as Lexer

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
      let res = Lexer.scanTokens "1 + 2 // end of line comment\n// lonely comment\n10 / 20.1"
      in res @?= Right (Seq.fromList [ Lexer.Number 1
                                     , Lexer.Plus
                                     , Lexer.Number 2
                                     , Lexer.Number 10
                                     , Lexer.Slash
                                     , Lexer.Number 20.1
                                     , Lexer.EOF
                                     ])
  ]
