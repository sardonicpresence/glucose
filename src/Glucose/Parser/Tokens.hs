{-# LANGUAGE TypeFamilies #-}
module Glucose.Parser.Tokens where

import Data.List as List
import Text.Megaparsec
import Glucose.Lexer.Lexeme
import Glucose.Lexer.Location

instance Stream [SyntacticToken] where
  type Token [SyntacticToken] = SyntacticToken
  uncons = List.uncons
  updatePos _ _ (SourcePos f _ _) t = let sourcePos = fromLocation f (location t) in (sourcePos, sourcePos)

instance ShowToken SyntacticToken where
  showTokens = concatMap show

fromLocation :: String -> Location -> SourcePos
fromLocation file (Location _ line col) = SourcePos file (intPos line) (intPos col)

intPos :: Int -> Pos
intPos = unsafePos . fromIntegral
