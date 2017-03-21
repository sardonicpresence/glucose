module Glucose.Codegen.LLVM.Types (tagMask, boxed, box, fn, size, closure, arity, typeDeclarations, rtName) where

import Control.Arrow
import Data.Monoid
import Data.Text (Text)
import LLVM.AST
import LLVM.Name

tagMask :: Int
tagMask = 0xF

boxed, box, fn, size, closure, arity :: Type
boxed = rtType Boxed
box = rtType Box
fn = rtType Fn
size = rtType Size
closure = rtType Closure
arity = rtType Arity

data RTType = Boxed | Box | Fn | Size | Arity | Closure

rtTypeName :: RTType -> Name
rtTypeName Boxed = rtName "boxed"
rtTypeName Box = rtName "box"
rtTypeName Fn = rtName "fn"
rtTypeName Size = rtName "size"
rtTypeName Arity = rtName "arity"
rtTypeName Closure = rtName "closure"

rtTypeRep :: RTType -> Type
rtTypeRep Boxed = Opaque
rtTypeRep Box = Ptr (rtType Boxed)
rtTypeRep Fn = Opaque
rtTypeRep Size = I 64
rtTypeRep Arity = I 16
rtTypeRep Closure = Struct [Ptr $ rtType Fn, rtType Arity, rtType Arity, Array 0 (rtType Box)]

rtType :: RTType -> Type
rtType = uncurry Custom . (rtTypeName &&& rtTypeRep)

rtName :: Text -> Name
rtName = Name . ("$" <>)

typeDeclarations :: [Global]
typeDeclarations = map (uncurry TypeDef . (rtTypeName &&& rtTypeRep)) [Boxed, Box, Fn, Size, Arity, Closure]
