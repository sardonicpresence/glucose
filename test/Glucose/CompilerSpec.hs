module Glucose.CompilerSpec (spec) where

import Test.Prelude

import Data.Foldable
import Data.Monoid
import Data.Text
import Glucose.Compiler

source :: Text
source = "b=\x5d0\&_a0\n" <>
         "\x5d0\&_a0 =\xa0\&123e01\n" <>
         "_b\x5d5\&0=12.3e-01\f" <>
         "type\xa0\&\x5d1_=  \x5d2_|\x5d3"

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" . forAllOutputs $ \t ->
    unpack <$> compile t " \r\n \f\xa0\x85 " `shouldBe` Right ""
  it "fails on unexpected operator" $ do
    compile LLVM " \n$~" `shouldErrorContaining` "$~"
    compile JavaScript " \n$~" `shouldErrorContaining` "$~"
  it "correctly compiles utf8 example to LLVM" $
    unpack <$> compileDefinitions LLVM source `shouldBe`
      Right ("@b = unnamed_addr alias i32, i32* @$5d0$_a0\n" ++
             "@$5d0$_a0 = unnamed_addr constant i32 1230, align 16\n" ++
             "@_b$5d5$0 = unnamed_addr constant double 1.23, align 16\n" ++
             "@$5d2$_ = unnamed_addr constant i32 0, align 16\n" ++
             "@$5d3$ = unnamed_addr constant i32 1, align 16\n")
  it "correctly compiles utf8 example to JavaScript" $
    unpack <$> compileDefinitions JavaScript source `shouldBe`
      Right ("\x5d0\&_a0 = 1230\n" ++
             "b = \x5d0\&_a0\n" ++
             "_b\x5d5\&0 = 1.23\n" ++
             "\x5d1_ = function() {}\n" ++
             "\x5d2_ = new \x5d1_()\n" ++
             "\x5d3 = new \x5d1_()\n")

forAllOutputs :: Applicative f => (CompilerOutput -> f a) -> f ()
forAllOutputs f = traverse_ f [LLVM, JavaScript]
