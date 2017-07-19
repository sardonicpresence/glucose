module Glucose.CLI.Args (CompilerArgs(..), parseArgs) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Throw
import System.Console.CmdArgs

import Glucose.Codegen
import Glucose.Version

data CompilerArgs = CompilerArgs { input :: FilePath, outputType :: CompilerOutput }

parseArgs :: (MonadIO m, MonadThrow String m) => m CompilerArgs
parseArgs = fromRaw <=< liftIO $ cmdArgs glucose

data RawArgs = RawArgs { _input :: FilePath, _outputType :: String } deriving (Show, Data, Typeable)

fromRaw :: RawArgs -> MonadThrow String m => m CompilerArgs
fromRaw RawArgs{_input, _outputType} = CompilerArgs _input <$> parseOutputType _outputType

glucose :: RawArgs
glucose = compilerArgs &= summary (app ++ " " ++ version ++ "\n" ++ copyright) &= help tagline where
  compilerArgs = RawArgs {
    _input = def &= args &= typ "FILE",
    _outputType = "ll" &= explicit &= name "t" &= name "output-type" &= typ "EXT"
                       &= help "Type of output to generate (ll - LLVM IR, js - JavaScript)"
  }

parseOutputType :: MonadThrow String m => String -> m CompilerOutput
parseOutputType "ll" = pure LLVM
parseOutputType "js" = pure JavaScript
parseOutputType s = throwError $ "Unknown output type: " ++ s
