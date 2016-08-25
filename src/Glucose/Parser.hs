module Glucose.Parser where

import Glucose.Lexer
import Glucose.Lang

parse :: [Token] -> Either String Module
parse [] = Right Module
parse (NotWhitespace s:_) = Left $ "Unexpected token in input: '" ++ s ++ "'"
