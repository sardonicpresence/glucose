module Glucose.ErrorSpec where

import Test.Prelude

import Data.Monoid ((<>))
import Data.Text (Text)
import Glucose.Error as Error
import Glucose.IR
import Glucose.Lexer.SyntaxError as Lexer
import Glucose.Parser.EOFOr
import Glucose.Parser.ParseError as Parser
import Glucose.Source
import Glucose.Token
import Glucose.TypeChecker.TypeCheckError as TypeChecker
import Glucose.Test.IR.Checked

spec :: Spec
spec = describe "formatError" $ do
  it "correctly formats syntax errors" $
    formatError "" (Error.SyntaxError . Lexer.SyntaxError (read "2:4@6") $ SyntaxErrorDetails "message" "context")
      `shouldBe` "2:4:\nmessage context\n"
  it "correctly formats parse errors" $ do
    let parseError loc unexpected expected = Error.ParseError . Parser.ParseError (read loc) $ ParseErrorDetails unexpected expected
    -- Unexpected EOF
    formatError "" (parseError "1:2@3" EOF [NotEOF "expected"])
      `shouldBe` "1:2:\nunexpected end of file\nexpecting expected\n"
    formatError "" (parseError "4:5@6" EOF [NotEOF "expected", NotEOF "other"])
      `shouldBe` "4:5:\nunexpected end of file\nexpecting expected or other\n"
    formatError "" (parseError "7:8@9" EOF [NotEOF "expected", NotEOF "other", NotEOF "another"])
      `shouldBe` "7:8:\nunexpected end of file\nexpecting expected, other or another\n"
    -- Unexpected token
    formatError "\x5d0\&ab\n  c= \n_" (parseError "1:2@3" (NotEOF (EndOfDefinition `at` "2:6@9-2:5@8")) [EOF])
      `shouldBe` "1:2:\nunexpected implicit end of definition\nexpecting end of file\n"
    formatError "\x5d0\&ab\n  c= \n_" (parseError "4:5@6" (NotEOF (Identifier "" `at` "1:1@1-1:2@2")) [EOF, NotEOF "expected"])
      `shouldBe` "4:5:\nunexpected identifier 'ab'\nexpecting end of file or expected\n"
    formatError "\x5d0\&ab\\\n  c= \n_" (parseError "7:8@9" (NotEOF (BeginLambda `at` "1:3@3-1:3@3")) [NotEOF "expected", EOF, NotEOF "other"])
      `shouldBe` "7:8:\nunexpected lambda '\\'\nexpecting expected, end of file or other\n"
    formatError "\x5d0\&ab\n  c\x5d0= \n_" (parseError "10:11@12" (NotEOF (Operator Assign `at` "2:2@5-2:5@9")) [NotEOF "expected", NotEOF "other", NotEOF "another"])
      `shouldBe` "10:11:\nunexpected operator ' c\x5d0= '\nexpecting expected, other or another\n"
  it "correctly formats duplicate definition errors" $
    formatError "" (Error.TypeCheckError $ TypeChecker.DuplicateDefinition ("ab" `at` "1:2@3-1:3@4") (() `at` "2:3@12-2:5@14"))
      `shouldBe` "1:2:\nduplicate definition of 'ab'\npreviously defined at 2:3\n"
  it "correctly formats unrecognised variable errors" $
    formatError "" (Error.TypeCheckError $ TypeChecker.UnrecognisedVariable ("_\x5d0\&c" `at` "1:3@4-1:4@6"))
      `shouldBe` "1:3:\nunrecognised variable '_\x5d0\&c'\n"
  it "correctly formats recursive definition errors" $
    formatError "" (Error.TypeCheckError $ TypeChecker.RecursiveDefinition ("_\x5d0\&c" `at` "999:999@9999-999:1001@10001"))
      `shouldBe` "999:999:\nrecursive definition: the value of '_\x5d0\&c' depends on itself\n"
  it "correctly formats type mismatch errors" $ do
    let typeMismatch expected actual = Error.TypeCheckError $ TypeChecker.TypeMismatch (CheckedType expected) (CheckedType <$> actual)
    let testCase (a, s) (b, t) = formatError " _ \n a=\x5d0\&b ~\n\n" (typeMismatch a $ b `at` "2:2@5-2:4@8")
          `shouldBe` "2:2:\ntype mismatch: expected '" <> s <> "', found '" <> t <> "' for expression 'a=\x5d0\&b'\n"
    testTypePairsWithFormat testCase
  it "correctly formats infinite type errors" $ do
    let infiniteType expected actual = Error.TypeCheckError $ TypeChecker.InfiniteType (CheckedType actual) (CheckedType <$> expected)
    let testCase (a, s) (b, t) = formatError " _ \n a=\x5d0\&b ~\n\n" (infiniteType (a `at` "2:2@5-2:4@8") b)
          `shouldBe` "2:2:\ninfinite type: " <> s <> " ~ " <> t <> "\n"
    testTypePairsWithFormat testCase

testTypePairsWithFormat :: ((DataType (Type Checked), Text) -> (DataType (Type Checked), Text) -> IO ()) -> IO ()
testTypePairsWithFormat testCase = do
  testCase ("a", "a") ("b", "b")
  testCase (Integer, "Int") (Float, "Float")
  testCase (Constrained Integer, "Int") (Constrained Float, "Float")
  testCase (ADT "c", "c") (Constrained $ ADT "D", "D")
  testCase ("a" --> "b", "a -> b") (Integer --> Float, "Int -> Float")
  testCase (Constrained $ ADT "C" --> Constrained Integer, "C -> Int")
           (Constrained Float --> Constrained (Constrained "a" --> "b"), "Float -> a -> b")
  testCase (Constrained ("a" --> Constrained (ADT "B")) --> "c", "(a -> B) -> c")
           ((("a" --> "b") --> ("c" --> "d")) --> (("e" --> "f") --> ("g" --> "h")), "((a -> b) -> c -> d) -> (e -> f) -> g -> h")
