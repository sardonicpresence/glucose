module Glucose.Codegen where

import Glucose.Lang
import qualified LLVM.AST as LLVM

codegen :: Module -> LLVM.Module
codegen _ = LLVM.Module
