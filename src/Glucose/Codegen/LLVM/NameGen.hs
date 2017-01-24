module Glucose.Codegen.LLVM.NameGen where

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Monad.State
import LLVM.Name

newtype NameGen a = NameGen (ReaderT Name (State Int) a)
  deriving (Functor, Applicative, Monad)

withNewScope :: Name -> NameGen a -> a
withNewScope name (NameGen a) = evalState (runReaderT a name) 1

newGlobal :: NameGen Name
newGlobal = NameGen . ReaderT $ \name -> state $ deriveName name &&& (+1)
