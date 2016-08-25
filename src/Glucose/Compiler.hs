module Glucose.Compiler where

import Control.Monad
import Data.Text.Lazy
import Glucose.Lexer
import Glucose.Parser
import Glucose.Codegen

-- | Compiles a single glucose source file into LLVM IR.
compile :: Text -> Either String Text
compile = Right . pack . show . codegen <=< parse . tokenize
