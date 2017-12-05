module Test.Executable where

import Test.Prelude

import System.Exit
import System.Process

testRun :: Int -> String -> IO ()
testRun expected input = do
  (exitcode, _, stderr) <- readCreateProcessWithExitCode (proc "lli" []) input
  let result = case exitcode of ExitSuccess -> 0; ExitFailure a -> a
  (result, stderr) `shouldBe` (expected, "")
