module Glucose.CompilerSpec (spec) where

import Test.Prelude

import Data.Text
import Glucose.Compiler

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" $
    unpack <$> compile " \r\n \f\xa0\x85 " `shouldBe` Right ""
  it "fails on unexpected operator" $
    compile " \n  $~" `shouldErrorContaining` "$~"
  it "correctly compiles utf8 example with global numeric constants" $
    unpack <$> compile "\x5d0\&_a0 =\xa0\&123e01\n_b\x5d5\&0=12.3e-01" `shouldBe`
      Right "@$5d0$_a0 = unnamed_addr constant i32 1230\n\n@_b$5d5$0 = unnamed_addr constant double 1.23\n"
