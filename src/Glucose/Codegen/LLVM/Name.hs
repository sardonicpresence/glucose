module Glucose.Codegen.LLVM.Name (module LLVM.Name, module Glucose.Codegen.LLVM.Name) where

import Data.Monoid ((<>))
import Data.Text
import LLVM.Name

{- | Glucose built-ins start with a single '$'. -}
builtinName :: Text -> Name
builtinName = Name . ("$" <>) . mangle

{- | Two '$'s are used as a separated to derive unique, generated names from user-defined names.
  Generated code, such as lambdas, are named based on the function/variable requiring them
  followed by two '$'s and an ordinal e.g. 'test$$2'.
-}
deriveName :: Name -> Int -> Name
deriveName (Name base) n = Name $ base <> "$$" <> pack (show n)
