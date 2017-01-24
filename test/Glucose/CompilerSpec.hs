module Glucose.CompilerSpec (spec) where

import Test.Prelude

import Data.Foldable
import Data.Text
import Glucose.Compiler

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" . forAllOutputs $ \t ->
    unpack <$> compile t " \r\n \f\xa0\x85 " `shouldBe` Right ""
  it "fails on unexpected operator" $ do
    compile LLVM " \n$~" `shouldErrorContaining` "$~"
    compile JavaScript " \n$~" `shouldErrorContaining` "$~"
  it "correctly compiles utf8 example to LLVM" $
    unpack <$> compileDefinitions LLVM "\x5d0\&_a0 =\xa0\&123e01\n_b\x5d5\&0=12.3e-01\fb=\x5d0\&_a0" `shouldBe`
      Right ("@$5d0$_a0 = unnamed_addr constant i32 1230, align 16\n" ++
             "@_b$5d5$0 = unnamed_addr constant double 1.23, align 16\n" ++
             "@b = unnamed_addr alias i32, i32* @$5d0$_a0\n")
  it "correctly compiles utf8 example to JavaScript" $
    unpack <$> compileDefinitions JavaScript "\x5d0\&_a0 =\xa0\&123e01\n_b\x5d5\&0=12.3e-01\fb=\x5d0\&_a0" `shouldBe`
      Right ("\x5d0\&_a0 = 1230\n" ++
             "_b\x5d5\&0 = 1.23\n" ++
             "b = \x5d0\&_a0\n")

forAllOutputs :: Applicative f => (CompilerOutput -> f a) -> f ()
forAllOutputs f = traverse_ f [LLVM, JavaScript]
