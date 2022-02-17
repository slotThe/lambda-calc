module Main (main) where

import Prelude hiding (read)
import Types
import Parser
import Interpreter
import Data.Functor
import Control.Monad

import qualified Data.Text.IO as T

main :: IO ()
main = forever do
  putStr "λ> "
  e <- read <$> T.getLine
  case e of
    Left err   -> print err
    Right expr -> print $ eval builtin (desugar expr)
