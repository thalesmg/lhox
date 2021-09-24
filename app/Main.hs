module Main where

import           Control.Monad      (forever, unless, when)
import           Data.Foldable      (traverse_)
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitWith)
import           System.IO          (hFlush, isEOF, stdin, stdout)

import qualified Lhox.Lexer         as Lexer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      runPrompt
      putStrLn ""
    [filename] ->
      runFile filename
    _ -> do
      putStrLn "usage: lhox [script]"
      exitWith (ExitFailure 64)

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    input <- TIO.getLine
    run input
    runPrompt

runFile :: FilePath -> IO ()
runFile filepath = do
  contents <- TIO.readFile filepath
  run contents

run :: Text -> IO ()
run code = do
  case Lexer.scanTokens code of
    Left err ->
      putStrLn $ "Dang!\n  " <> show err
    Right tokens ->
      traverse_ print tokens
