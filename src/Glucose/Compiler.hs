module Glucose.Compiler (compile) where

import Control.Monad
import Data.Text
import Glucose.Lexer
import Glucose.Parser
import Glucose.TypeChecker
import Glucose.Codegen

-- | Compiles a single glucose source file into LLVM IR.
compile :: Text -> Either String Text
compile = Right . pack . show . codegen <=< typeCheck <=< parse <=< tokenize
