module Main where

import Control.Monad.Except
import System.IO (hPutStr, stderr)

import Glucose.CLI.Args
import Glucose.CLI.IO
import Glucose.Codegen
import Glucose.Compiler

main :: IO ()
main = logErrors $ do
  CompilerArgs {input, outputType} <- parseArgs
  let writeOutput = liftIO . writeWithExtensionOrStdout (extension outputType) input
  writeOutput <=< ExceptT $ compile (codegen outputType) <$> readFileOrStdin input

logErrors :: ExceptT String IO () -> IO ()
logErrors = either (hPutStr stderr) pure <=< runExceptT

extension :: CompilerOutput -> String
extension LLVM{} = "ll"
extension JavaScript = "js"
extension IR = ""
