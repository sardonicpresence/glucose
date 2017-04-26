module Glucose.Codegen.LLVM.Types
(
  tagMask, untagMask, argAlign,
  boxed, box, fn, size, arity, argsize, closure,
  closureType, rtName, typeDeclarations
) where

import Control.Arrow
import Data.Monoid
import Data.Text (Text)
import LLVM.AST
import LLVM.Name

tagMask :: Expression
tagMask = Literal $ IntegerLiteral (Just $ rtTypeName Size) 64 0xF

untagMask :: Expression
untagMask = Literal $ IntegerLiteral (Just $ rtTypeName Size) 64 (-0xF - 1)

-- | Alignment of the first applied argument in a closure.
argAlign :: Int
argAlign = 16

boxed, box, fn, size, arity, argsize, closure :: Type
boxed = rtType Boxed
box = rtType Box
fn = rtType Fn
size = rtType Size
arity = rtType Arity
argsize = rtType ArgSize
closure = rtType Closure

data RTType = Boxed | Box | Fn | Size | Arity | ArgSize | Closure

rtTypeName :: RTType -> Name
rtTypeName Boxed = rtName "boxed"
rtTypeName Box = rtName "box"
rtTypeName Fn = rtName "fn"
rtTypeName Size = rtName "size"
rtTypeName Arity = rtName "arity"
rtTypeName ArgSize = rtName "argsize"
rtTypeName Closure = rtName "closure"

rtTypeRep :: RTType -> Type
rtTypeRep Boxed = Opaque
rtTypeRep Box = Ptr (rtType Boxed)
rtTypeRep Fn = Opaque
rtTypeRep Size = I 64
rtTypeRep Arity = I 16
rtTypeRep ArgSize = I 32
rtTypeRep Closure = closureType 0 []

closureType :: Int -> [Type] -> Type
-- closureType 0 [] = Struct [Ptr fn, arity, arity, argsize, Array 0 box]
closureType n args = Struct [Ptr fn, arity, arity, argsize, Array n box, Packed args]

rtType :: RTType -> Type
rtType = uncurry Custom . (rtTypeName &&& rtTypeRep)

rtName :: Text -> Name
rtName = Name . ("$" <>)

typeDeclarations :: [Global]
typeDeclarations = map (uncurry TypeDef . (rtTypeName &&& rtTypeRep)) [Boxed, Box, Fn, Size, Arity, ArgSize, Closure]
