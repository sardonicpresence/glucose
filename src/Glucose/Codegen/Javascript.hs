module Glucose.Codegen.JavaScript (codegen, codegenDefinitions) where

import Control.Comonad
import Data.Monoid
import Data.Text (Text, pack, unpack, intercalate)
import Glucose.Identifier
import Glucose.IR
import Glucose.Parser.Source
import Glucose.VarGen

data JSRaw = JSRaw Text
instance Show JSRaw where
  show (JSRaw s) = unpack s

codegen :: Module -> JSRaw
codegen (Module defs) = codegenDefinitions $ map extract defs

codegenDefinitions :: [Definition] -> JSRaw
codegenDefinitions defs = JSRaw $ foldMap definition defs

definition :: Definition -> Text
definition (Definition (FromSource _ (Identifier name)) (FromSource _ (Lambda args expr))) =
  "function " <> name <> "(" <> intercalate ", " (map (arg.extract) args) <> ") {\n" <>
  "  return " <> expression (extract expr) <> "\n" <>
  "}\n"
definition (Definition (FromSource _ (Identifier name)) def) = name <> " = " <> expression (extract def) <> "\n"

expression :: Expression -> Text
expression (Literal a) = pack $ show a
expression (Reference _ (Identifier a) _) = a
expression (Lambda args expr) =
  "function(" <> intercalate ", " (map (arg . extract) args) <> ") {" <>
  " return " <> expression (extract expr) <> " " <>
  "}"
expression (Apply (extract -> f) (extract -> a)) = case flattenApply f a of
  Application root calls partial -> maybe full partialApply partial where
    full = foldl (<>) (expression root) (map (parenList . map expression) calls)
    -- TODO: variable names can conflict with variables from outer scopes
    partialApply (Partial ty args) = let residual = take (arity ty) variables in
      "function" <> parenList residual <> " { return " <> full <> parenList (map expression args ++ residual) <> " }"

arity :: Type -> Int
arity (Function (Arity n m) _ _) = n - m
arity _ = 0

-- TODO: need to build lambdas to coerce function arguments to the expected arity

arg :: Arg -> Text
arg (Arg (Identifier a) _) = a

parenList :: [Text] -> Text
parenList as = "(" <> intercalate ", " as <> ")"
