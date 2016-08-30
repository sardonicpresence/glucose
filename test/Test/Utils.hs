module Test.Utils where

import Test.Hspec

{-# INLINE shouldErrorContaining #-}
shouldErrorContaining :: Show a => Either String a -> String -> Expectation
shouldErrorContaining (Right a) _ = expectationFailure $ "did not fail, returned '" ++ show a ++ "'"
shouldErrorContaining (Left e) s = e `shouldContain` s
