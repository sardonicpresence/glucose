module Main where

import Prelude hiding (readFile, getContents, putStr, writeFile)
import Data.Text.Lazy
import Data.Text.Lazy.IO
import System.Console.CmdArgs
import System.FilePath
import Glucose.Compiler

data Compiler = Compiler { input :: FilePath } deriving (Show, Data, Typeable)

compiler = Compiler { input = def &= args &= typ "FILE" }
        &= summary "Experimental compiler for the trivial glucose language."

readInput :: FilePath -> IO Text
readInput "" = getContents
readInput path = readFile path

writeOutput :: FilePath -> Text -> IO ()
writeOutput "" = putStr
writeOutput inputPath = writeFile $ inputPath -<.> "ll"

main :: IO ()
main = do
  Compiler {input} <- cmdArgs compiler
  writeOutput input =<< compile <$> readInput input
