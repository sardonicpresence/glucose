module Glucose.Compiler where

import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Glucose.Desugar
import Glucose.Error
import Glucose.IR
import Glucose.Lexer
import Glucose.Parser
import Glucose.TypeChecker

-- | Compiles a single glucose source file with the given code-generator.
compile :: (Module Checked -> a) -> Text -> Either String a
compile codegen source = format $ pure . codegen =<< typeCheck =<< desugar =<< uncurry parse =<< tokenise source
  where format = first (unpack . formatError source)
