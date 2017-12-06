module Test.Executable where

import Test.Prelude

import System.Exit
import System.Process

testRun :: String -> Int -> String -> IO ()
testRun exe expected input = do
  (exitcode, _, stderr) <- readCreateProcessWithExitCode (proc exe []) input
  let result = case exitcode of ExitSuccess -> 0; ExitFailure a -> a
  (result, stderr) `shouldBe` (expected, "")
