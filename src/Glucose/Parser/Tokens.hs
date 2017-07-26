{-# LANGUAGE TypeFamilies #-}
module Glucose.Parser.Tokens where

import Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec
import qualified Glucose.Error as Glucose
import Glucose.Lexer.SyntacticToken
import Glucose.Source

instance Stream [SyntacticToken] where
  type Token [SyntacticToken] = SyntacticToken
  uncons = List.uncons
  updatePos _ _ (SourcePos f _ _) t = (fromLocation f start, fromLocation f start) where
    start = location t

instance ShowToken SyntacticToken where
  showTokens = concatMap showToken

fromLocation :: String -> Location -> SourcePos
fromLocation file (Location _ line col) = SourcePos file (intPos line) (intPos col)

toLocation :: SourcePos -> Location
toLocation (SourcePos _ line col) = Location 0 (fromIntegral $ unPos line) (fromIntegral $ unPos col)

intPos :: Int -> Pos
intPos = unsafePos . fromIntegral

fromParseError :: (ShowErrorComponent e, ShowToken t, Ord t) => ParseError t e -> Glucose.CompileError
fromParseError e = Glucose.CompileError location $ Glucose.ParseError messages where
  location = toLocation . NonEmpty.head $ errorPos e
  messages = tail . lines $ parseErrorPretty e
