module Glucose.VarGen (variables, identifiers, VarGen, mkVarGen, genVar) where

import Control.Arrow
import Data.Text (Text, pack)
import Glucose.Identifier

variables :: [Text]
variables = map (pack . reverse) $ iterate nextVar "a"

identifiers :: [Identifier]
identifiers = map Identifier variables

data VarGen = VarGen String

mkVarGen :: VarGen
mkVarGen = VarGen ""

genVar :: VarGen -> (Identifier, VarGen)
genVar (VarGen n) = Identifier . pack . reverse &&& VarGen $ nextVar n

nextVar :: String -> String
nextVar [] = "a"
nextVar ('z':cs) = 'a' : nextVar cs
nextVar (c:cs) = succ c : cs
