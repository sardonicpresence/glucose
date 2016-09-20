module Glucose.Compiler (CompilerOutput(..), compile) where

import Data.Bifunctor
import Data.Text
import Glucose.Error
import Glucose.Lexer
import Glucose.Parser
import Glucose.TypeChecker
import qualified Glucose.Codegen.JavaScript as JS
import qualified Glucose.Codegen.LLVM as LLVM

data CompilerOutput = LLVM | JavaScript

-- | Compiles a single glucose source file into LLVM IR.
compile :: CompilerOutput -> Text -> Either Text Text
compile output source = format $ pure . codegen =<< typeCheck =<< uncurry parse =<< tokenise source
  where format = bimap (formatError source) id
        codegen = pack . case output of
          LLVM -> show . LLVM.codegen
          JavaScript -> show . JS.codegen
