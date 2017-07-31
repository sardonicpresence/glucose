module Glucose.Codegen where

import Control.Comonad
import Data.Text
import Glucose.Codegen.LLVM as LLVM
import Glucose.Codegen.JavaScript as JavaScript
import Glucose.IR

data CompilerOutput = LLVM | JavaScript

type Codegen f = Module Checked f -> Text

codegen :: Comonad f => CompilerOutput -> Codegen f
codegen LLVM = LLVM.codegen
codegen JavaScript = JavaScript.codegen

codegenDefinitions :: Comonad f => CompilerOutput -> Codegen f
codegenDefinitions LLVM = LLVM.codegenModuleDefinitions
codegenDefinitions JavaScript = JavaScript.codegenModuleDefinitions
