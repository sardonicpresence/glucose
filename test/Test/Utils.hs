module Test.Utils where

import Glucose.Error
import Test.Hspec hiding (shouldBe)

infixr 1 `shouldBe`, `shouldErrorContaining`, `shouldErrorWith`, `shouldShow`

{-# INLINE shouldErrorContaining #-}
shouldErrorContaining :: (Show e, Show a) => Either e a -> String -> Expectation
shouldErrorContaining (Right a) _ = expectationFailure $ "did not fail, returned:" ++ formatResult a
shouldErrorContaining (Left e) s = show e `shouldContain` s

{-# INLINE shouldErrorWith #-}
shouldErrorWith :: Show a => Either CompileError a -> CompileError -> Expectation
shouldErrorWith (Right a) _ = expectationFailure $ "did not fail, returned:" ++ formatResult a
shouldErrorWith (Left a) e = a `shouldBe` e

{-# INLINE shouldShow #-}
shouldShow :: Show a => a -> String -> Expectation
shouldShow a b = show a `shouldBe` b

shouldBe :: (Eq a, Show a) => a -> a -> Expectation
shouldBe a e | a == e = pure ()
shouldBe a e = expectationFailure $ "expected:" ++ formatResult e ++ "\nbut got: " ++ formatResult a

formatResult :: Show a => a -> String
formatResult a = if '\n' `elem` show a
  then "\n" ++ show a
  else " " ++ show a
