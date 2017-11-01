module Glucose.Codegen where

import Control.Comonad
import Data.Text
import Glucose.Codegen.JavaScript as JavaScript
import Glucose.Codegen.LLVM as LLVM
import Glucose.Codegen.Target
import Glucose.Format
import Glucose.IR

data CompilerOutput = LLVM Target | JavaScript | IR

type Codegen f = Module Checked f -> Text

codegen :: (Comonad f, FormattableFunctor Format f) => CompilerOutput -> Codegen f
codegen (LLVM target) = LLVM.codegen target
codegen JavaScript = JavaScript.codegen
codegen IR = format Complete

codegenDefinitions :: (Comonad f, FormattableFunctor Format f) => CompilerOutput -> Codegen f
codegenDefinitions (LLVM _) = LLVM.codegenModuleDefinitions
codegenDefinitions JavaScript = JavaScript.codegenModuleDefinitions
codegenDefinitions IR = format Complete
