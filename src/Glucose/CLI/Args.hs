module Glucose.CLI.Args (CompilerArgs(..), parseArgs) where

import Control.Monad
import Control.Monad.Except
import System.Console.CmdArgs

import Glucose.Codegen
import Glucose.Codegen.Target
import Glucose.Version

data CompilerArgs = CompilerArgs { input :: FilePath, outputType :: CompilerOutput }

parseArgs :: (MonadIO m, MonadError String m) => m CompilerArgs
parseArgs = fromRaw <=< liftIO $ cmdArgs glucose

data RawArgs = RawArgs { _input :: FilePath, _outputType :: String, _triple :: String }
  deriving (Show, Data, Typeable)

fromRaw :: RawArgs -> MonadError String m => m CompilerArgs
fromRaw RawArgs{_input, _outputType, _triple} = CompilerArgs _input <$> parseOutputType _triple _outputType

glucose :: RawArgs
glucose = compilerArgs &= summary (app ++ " " ++ version ++ "\n" ++ copyright) &= help tagline where
  compilerArgs = RawArgs {
    _input = def &= args &= typ "FILE",
    _outputType = "ll" &= explicit &= name "t" &= name "output-type" &= typ "EXT"
                       &= help "Type of output to generate (ll - LLVM IR, js - JavaScript)",
    _triple = "x86_64-pc-windows" &= explicit &= name "triple" &= typ "TRIPLE" &= help "Target triple"
  }

parseOutputType :: MonadError String m => String -> String -> m CompilerOutput
parseOutputType triple "ll" = LLVM <$> maybe (throwError unsupported) pure (parseTriple triple) where
  unsupported = "Unsupported target triple: " ++ triple
parseOutputType _ "js" = pure JavaScript
parseOutputType _ "ir" = pure IR
parseOutputType _ s = throwError $ "Unknown output type: " ++ s
