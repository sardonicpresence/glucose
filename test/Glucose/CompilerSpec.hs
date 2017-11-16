module Glucose.CompilerSpec (spec) where

import Test.Prelude

import Control.Comonad
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Glucose.Codegen
import Glucose.Codegen.Target
import Glucose.Compiler
import Glucose.Format

source :: Text
source = "b=\x5d0\&_a0\n" <>
         "\x5d0\&_a0 =\xa0\&123e01\n" <>
         "_b\x5d5\&0=12.3e-01\f" <>
         "type\xa0\&\x5d1_=  \x5d2_|\x5d3\n" <>
         "id=\\a->a\f" <>
         "const\xa0\&=\\\x5d4\&->id\xa0\x5d4"

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" . forAllOutputs $ \t ->
    unpack <$> compile t " \r\n \f\xa0\x85 " `shouldBe` Right ""
  it "fails on unexpected operator" . forAllOutputs $ \t ->
    compile t " \n$~" `shouldErrorContaining` "$~"
  it "correctly compiles utf8 example to LLVM" $
    unpack <$> compile (codegenDefinitions $ LLVM target) source `shouldBe` (Right . unlines)
      [ "@b = unnamed_addr alias i32, i32* @$5d0$_a0"
      , "@$5d0$_a0 = unnamed_addr constant i32 1230"
      , "@_b$5d5$0 = unnamed_addr constant double 1.23"
      , "@$5d2$_ = unnamed_addr constant i32 0"
      , "@$5d3$ = unnamed_addr constant i32 1"
      , "define fastcc %$box @id(%$box %a) unnamed_addr #0 {"
      , "  ret %$box %a"
      , "}"
      , "define fastcc %$box @const(%$box %$5d4$) unnamed_addr #0 {"
      , "  %1 = tail call fastcc %$box @id(%$box %$5d4$)"
      , "  ret %$box %1"
      , "}" ]
  it "correctly compiles utf8 example to JavaScript" $
    unpack <$> compile (codegenDefinitions JavaScript) source `shouldBe` (Right . unlines)
      [ "\x5d0\&_a0 = 1230"
      , "b = \x5d0\&_a0"
      , "_b\x5d5\&0 = 1.23"
      , "\x5d1_ = function() {}"
      , "\x5d2_ = new \x5d1_()"
      , "\x5d3 = new \x5d1_()"
      , "function id(a) { return a }"
      , "function $const(\x5d4) { return id(\x5d4) }" ]

forAllOutputs :: (Comonad f, FormattableFunctor Format f, Applicative m) => (Codegen f -> m a) -> m ()
forAllOutputs f = traverse_ (f . codegen) [LLVM target, JavaScript]

target :: Target
target = fromJust $ parseTriple "x86_64-pc-mingw32"
