module Glucose.Codegen.LLVM.Types
(
  Representation(..), typeRep, repType, valueType, functionType,
  argAlign, alignment,
  boxed, box, fn, size, arity, argsize, closure,
  closureType, rtName, typeDeclarations
) where

import Control.Arrow
import Data.Monoid
import Data.Text (Text)
import LLVM.AST
import LLVM.Name

-- * Representations

data Representation = I32Rep | F64Rep | BoxRep
                    | FunRep Representation [Representation] -- TODO: Until we have closures
  deriving (Eq, Ord)

typeRep :: Type -> Representation
typeRep (I n) | n <= 32 = I32Rep
typeRep (I n) = error $ show n ++ "-bit integers are not supported!"
typeRep F64 = F64Rep
-- typeRep (Ptr (Function a bs)) = FunRep (typeRep a) (map typeRep bs) -- TODO: Until we have closures
typeRep _ = BoxRep

repType :: Representation -> Type
repType I32Rep = I 32
repType F64Rep = F64
repType BoxRep = box
repType (FunRep a bs) = Ptr $ Function (repType a) (map repType bs) -- TODO: Until we have closures

valueType :: Type -> Type
valueType = repType . typeRep

functionType :: Representation -> [Representation] -> Type
functionType result args = Ptr $ Function (repType result) (map repType args)



alignment :: Int
alignment = 16

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

{- | The LLVM type of a closure with the given bound pointer & non-pointer arguments.

  %$closure = type {
    %$fn*, ; Code pointer + tag bits (used for..?)
    %$arity, ; Number of unbound arguments
    %$arity, ; Number of pointer args that follow
    %$argsize, ; Number of bytes of non-pointer args that follow
    [0 x %$box], ; Variable number of bound pointer arguments (must be 16-byte aligned)
    <{}> ; Variable number of bound non-pointer arguments in a packed struct
  }
-}
closureType :: Int -> [Type] -> Type
-- closureType n [] = Struct [Ptr fn, arity, arity, argsize, Array n box]
closureType n args = Struct [Ptr fn, arity, arity, argsize, Array n box, Packed args]

rtType :: RTType -> Type
rtType = uncurry Custom . (rtTypeName &&& rtTypeRep)

rtName :: Text -> Name
rtName = Name . ("$" <>)

typeDeclarations :: [Global]
typeDeclarations = map (uncurry TypeDef . (rtTypeName &&& rtTypeRep)) [Boxed, Box, Fn, Size, Arity, ArgSize, Closure]
