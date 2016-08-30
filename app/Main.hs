module Main where

import Prelude hiding (readFile, getContents, putStr, writeFile)
import Data.Text
import Data.Text.IO hiding (hPutStr)
import System.Console.CmdArgs
import System.FilePath
import System.IO (stderr, hPutStr)
import Glucose.Compiler

data Compiler = Compiler { input :: FilePath } deriving (Show, Data, Typeable)

compiler :: Compiler
compiler = Compiler { input = def &= args &= typ "FILE" }
        &= summary "glucose 0.0.1\nCopyright (C) 2016 Neil Vice"
        &= help "Experimental compiler for the trivial glucose language"

readInput :: FilePath -> IO Text
readInput "" = getContents
readInput path = readFile path

writeOutput :: FilePath -> Text -> IO ()
writeOutput "" = putStr
writeOutput inputPath = writeFile $ inputPath -<.> "ll"

main :: IO ()
main = do
  Compiler {input} <- cmdArgs compiler
  result <- compile <$> readInput input
  case result of
    Left e -> hPutStr stderr e
    Right ll -> writeOutput input ll
