module Glucose.Namespace where

import Data.Map
import Glucose.AST (Definition(..), Identifier)
import Glucose.Error
import Glucose.Lexer.Location

newtype Namespace = Namespace (Map Identifier Location)

emptyNamespace :: Namespace
emptyNamespace = Namespace empty

declare :: Definition -> Namespace -> Error Namespace
declare (Definition n _ loc) (Namespace ns) = let (prior, ns') = insertLookupWithKey undefined n loc ns in
  maybe (pure $ Namespace ns') (throwError . formatError) prior where
  formatError previous = "duplicate definition of " ++ show n ++ " at " ++ show loc ++
                       "\npreviously defined at " ++ show previous
