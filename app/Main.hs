module Main (main) where

import Prelude hiding (read)
import Types
import Parser
import Interpreter
import Control.Monad
import Control.Exception

import Data.Text.IO qualified as T

main :: IO ()
main = forever do
  putStr "λ> "
  e <- read <$> T.getLine
  case e of
    Left err   -> putStr err
    Right expr ->
      print (eval builtin (desugar expr))
        `catch` print @SomeException
