module Main (main) where

import Checker
import Interpreter
import Parser
import Prelude hiding (read)
import Types

import Control.Exception
import Control.Monad
import Data.Text (Text)
import System.IO (hFlush, stdout)

import Data.Text.IO qualified as T
import Data.Text    qualified as T

main :: IO ()
main = forever do
  putStr "λ> "
  hFlush stdout
  l <- T.getLine
  if ":t " `T.isPrefixOf` l
    then tryEval (T.drop 3 l) check
    else tryEval l            (eval builtin)
 where
  tryEval :: Show a => Text -> (DExpr -> a) -> IO ()
  tryEval e f = case read e of
    Left  err  -> putStr err
    Right expr -> print (f (desugar expr)) `catch` print @SomeException
