module Main (main) where

import Parser
import Types

import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
  describe "Parser.read" $ do
    it "parses function application before over binary operations" $
      Parser.read "f 1 + b" == Right (EBin "+" (EApp (EVar "f") [EInt 1]) (EVar "b"))
    it "parses applications left-biased 1" $
      Parser.read "(\\f g a. f (g a)) (\\a. a + 1) (\\a. a + 2) 42"
        == Right (EApp (ELam ["f", "g", "a"] (EApp (EVar "f")
                                                   [EApp (EVar "g")
                                                         [EVar "a"]]))
                       [ ELam ["a"] (EBin "+" (EVar "a") (EInt 1))
                       , ELam ["a"] (EBin "+" (EVar "a") (EInt 2))
                       , EInt 42
                       ])
    it "parser applications left-biased 2" $
      Parser.read "(\\g f a. g (f a a))"
        == Right (ELam ["g", "f", "a"]
                  (EApp (EVar "g")
                        [EApp (EVar "f")
                              [EVar "a", EVar "a"]]))
