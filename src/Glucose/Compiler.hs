module Glucose.Compiler where

import Data.Bifunctor (first)
import Data.Text (Text, unpack)
import Glucose.Error
import Glucose.IR
import Glucose.Source
import qualified Glucose.Desugar as Desugar
import qualified Glucose.Lexer as Lexer
import qualified Glucose.Parser as Parser
import qualified Glucose.TypeChecker as TypeChecker

-- | Compiles a single glucose source file with the given code-generator.
compile :: (Module Checked FromSource -> a) -> Text -> Either String a
compile codegen source = format $ pure . codegen =<< typeCheck =<< desugar =<< uncurry parse =<< tokenise source
  where format = first (unpack . formatError source)

tokenise = liftErrors . Lexer.tokenise
parse = (liftErrors .) . Parser.parse
desugar = Desugar.desugar
typeCheck = liftErrors . TypeChecker.typeCheck
