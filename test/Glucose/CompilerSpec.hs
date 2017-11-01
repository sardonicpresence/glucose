module Glucose.CompilerSpec (spec) where

import Test.Prelude

import Control.Comonad
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text
import Glucose.Codegen
import Glucose.Codegen.Target
import Glucose.Compiler
import Glucose.Format

source :: Text
source = "b=\x5d0\&_a0\n" <>
         "\x5d0\&_a0 =\xa0\&123e01\n" <>
         "_b\x5d5\&0=12.3e-01\f" <>
         "type\xa0\&\x5d1_=  \x5d2_|\x5d3"

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" . forAllOutputs $ \t ->
    unpack <$> compile t " \r\n \f\xa0\x85 " `shouldBe` Right ""
  it "fails on unexpected operator" . forAllOutputs $ \t ->
    compile t " \n$~" `shouldErrorContaining` "$~"
  it "correctly compiles utf8 example to LLVM" $
    unpack <$> compile (codegenDefinitions $ LLVM target) source `shouldBe`
      Right ("@b = unnamed_addr alias i32, i32* @$5d0$_a0\n" ++
             "@$5d0$_a0 = unnamed_addr constant i32 1230\n" ++
             "@_b$5d5$0 = unnamed_addr constant double 1.23\n" ++
             "@$5d2$_ = unnamed_addr constant i32 0\n" ++
             "@$5d3$ = unnamed_addr constant i32 1\n")
  it "correctly compiles utf8 example to JavaScript" $
    unpack <$> compile (codegenDefinitions JavaScript) source `shouldBe`
      Right ("\x5d0\&_a0 = 1230\n" ++
             "b = \x5d0\&_a0\n" ++
             "_b\x5d5\&0 = 1.23\n" ++
             "\x5d1_ = function() {}\n" ++
             "\x5d2_ = new \x5d1_()\n" ++
             "\x5d3 = new \x5d1_()\n")

forAllOutputs :: (Comonad f, FormattableFunctor Format f, Applicative m) => (Codegen f -> m a) -> m ()
forAllOutputs f = traverse_ (f . codegen) [LLVM target, JavaScript]

target :: Target
target = fromJust $ parseTriple "x86_64-pc-mingw32"
