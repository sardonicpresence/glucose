module Glucose.CLI.IO where

import Prelude hiding (readFile, getContents, putStr, writeFile)
import Data.Text (Text)
import Data.Text.IO hiding (hPutStr)
import System.FilePath (FilePath, (-<.>))

readFileOrStdin :: FilePath -> IO Text
readFileOrStdin "" = getContents
readFileOrStdin path = readFile path

writeWithExtensionOrStdout :: String -> FilePath -> Text -> IO ()
writeWithExtensionOrStdout _ "" = putStr
writeWithExtensionOrStdout extension path = writeFile $ path -<.> extension
