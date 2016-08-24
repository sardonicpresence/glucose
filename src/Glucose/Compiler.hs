module Glucose.Compiler where

import Data.Text.Lazy
import Glucose.Lexer
import Glucose.Parser
import Glucose.Codegen

-- | Compiles a single glucose source file into LLVM IR.
compile :: Text -> Text
compile = pack . show . codegen . parse . tokenize
