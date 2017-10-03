module Test.Executable where

import Test.Prelude

import Control.Monad
import System.Exit
import System.Process

testRun :: Int -> String -> IO ()
testRun expected input = do
  (exitcode, _, stderr) <- readCreateProcessWithExitCode (shell "lli") input
  let result = case exitcode of ExitSuccess -> 0; ExitFailure a -> a
  when (result /= expected) $ putStrLn stderr
  result `shouldBe` expected
