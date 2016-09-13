module Glucose.Namespace where

import Data.Map as Map
import Glucose.AST (Definition(..))
import Glucose.Error
import Glucose.Identifier

newtype Namespace = Namespace (Map Identifier Definition)

emptyNamespace :: Namespace
emptyNamespace = Namespace empty

declare :: Definition -> Namespace -> Error Namespace
declare def@(Definition n _ loc) (Namespace ns) = let (prior, ns') = insertLookupWithKey undefined n def ns in
  maybe (pure $ Namespace ns') (throwError . formatError) prior where
  formatError (Definition _ _ previous) =
    "duplicate definition of " ++ show n ++ " at " ++ show loc ++ "\npreviously defined at " ++ show previous

definitionOf :: Identifier -> Namespace -> Maybe Definition
definitionOf n (Namespace ns) = Map.lookup n ns
