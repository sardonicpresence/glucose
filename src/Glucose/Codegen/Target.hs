module Glucose.Codegen.Target where

import Control.Applicative
import Data.List (stripPrefix)
import qualified LLVM.AST as LLVM

data Architecture = X64
data OperatingSystem = Windows
data Target = Target Architecture OperatingSystem LLVM.Triple

parseTriple :: String -> Maybe Target
parseTriple triple = do
  vendor <- stripPrefix "x86_64-" triple
  os <- stripPrefix "pc-" vendor <|> stripPrefix "w64-" vendor
  pure $ Target X64 Windows $ LLVM.Triple "x86_64" (takeWhile (/= '-') vendor) os

llvmTarget :: Target -> LLVM.Target
llvmTarget (Target arch os triple) = LLVM.Target dataLayout triple where
  dataLayout = LLVM.DataLayout (endian arch) (mangling os) (alignmentOverrides arch) (integerSizes arch) (stackAlign os)

endian :: Architecture -> LLVM.Endian
endian X64 = LLVM.LittleEndian

mangling :: OperatingSystem -> LLVM.Mangling
mangling Windows = LLVM.Windows

alignmentOverrides :: Architecture -> [(LLVM.Type, Int, Maybe Int)]
alignmentOverrides X64 = [(LLVM.I 64, 64, Nothing)]

integerSizes :: Architecture -> [Int]
integerSizes X64 = [8,16,32,64]

stackAlign :: OperatingSystem -> Maybe Int
stackAlign Windows = Just 128
