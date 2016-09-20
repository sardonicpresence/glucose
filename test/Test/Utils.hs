module Test.Utils where

import Glucose.Error
import Test.Hspec

{-# INLINE shouldErrorContaining #-}
shouldErrorContaining :: (Show e, Show a) => Either e a -> String -> Expectation
shouldErrorContaining (Right a) _ = expectationFailure $ "did not fail, returned '" ++ show a ++ "'"
shouldErrorContaining (Left e) s = show e `shouldContain` s

{-# INLINE shouldErrorWith #-}
shouldErrorWith :: Show a => Either CompileError a -> CompileError -> Expectation
shouldErrorWith (Right a) _ = expectationFailure $ "did not fail, returned '" ++ show a ++ "'"
shouldErrorWith (Left a) e = a `shouldBe` e

{-# INLINE shouldShow #-}
shouldShow :: Show a => a -> String -> Expectation
shouldShow a b = show a `shouldBe` b
