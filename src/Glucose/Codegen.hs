module Glucose.Codegen where

import Data.Text
import Glucose.Codegen.LLVM as LLVM
import Glucose.Codegen.JavaScript as JavaScript
import Glucose.IR

data CompilerOutput = LLVM | JavaScript

type Codegen = Module Checked -> Text

codegen :: CompilerOutput -> Codegen
codegen LLVM = LLVM.codegen
codegen JavaScript = JavaScript.codegen

codegenDefinitions :: CompilerOutput -> Codegen
codegenDefinitions LLVM = LLVM.codegenModuleDefinitions
codegenDefinitions JavaScript = JavaScript.codegenModuleDefinitions
