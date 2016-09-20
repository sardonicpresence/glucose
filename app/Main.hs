module Main where

import Prelude hiding (readFile, getContents, putStr, writeFile)
import Data.Text
import Data.Text.IO hiding (hPutStr)
import System.Console.CmdArgs
import System.FilePath
import System.IO (stderr, hPutStr)
import Glucose.Compiler
import Glucose.Version

parseOutputType :: String -> IO CompilerOutput
parseOutputType "ll" = pure LLVM
parseOutputType "js" = pure JavaScript
parseOutputType s = error $ "Unknown output type: " ++ s

extension :: CompilerOutput -> String
extension LLVM = "ll"
extension JavaScript = "js"

data Compiler = Compiler { input :: FilePath, outputType :: String } deriving (Show, Data, Typeable)

compiler :: Compiler
compiler = Compiler { input = def &= args &= typ "FILE"
                    , outputType = "ll" &= explicit &= name "t" &= name "output-type" &= typ "EXT"
                                        &= help "Type of output to generate (ll - LLVM IR, js - JavaScript)"}
        &= summary ("glucose " ++ version ++ "\n" ++ copyright)
        &= help "Experimental compiler for the trivial glucose language"

readInput :: FilePath -> IO Text
readInput "" = getContents
readInput path = readFile path

writeOutput :: CompilerOutput -> FilePath -> Text -> IO ()
writeOutput _ "" = putStr
writeOutput outputType inputPath = writeFile $ inputPath -<.> extension outputType

main :: IO ()
main = do
  Compiler {input, outputType} <- cmdArgs compiler
  outputType <- parseOutputType outputType
  result <- compile outputType <$> readInput input
  case result of
    Left e -> hPutStr stderr $ unpack e
    Right compiled -> writeOutput outputType input compiled
