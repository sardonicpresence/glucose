module LLVM.AST where

import LLVM.Name

import Data.List

data Module = Module Target [Global]
  deriving (Eq)

data Target = Target DataLayout Triple
  deriving (Eq)

data DataLayout = DataLayout Endian Mangling [(Type, Int, Maybe Int)] [Int] (Maybe Int)
  deriving (Eq)

data Endian = BigEndian | LittleEndian
  deriving (Eq)

data Mangling = Elf | Mips | MachO | Windows | Windowsx86
  deriving (Eq)

data Triple = Triple String String String
  deriving (Eq)

data Global = VariableDefinition Name Linkage Expression
            | Alias Name Name Type
            | FunctionDefinition Name Linkage [Arg] [Statement] Expression
            | TypeDef Name Type
            | FunctionDeclaration Name Type -- TODO: Linkage?
  deriving (Eq)

data Statement = Call Int Expression [Expression]
               | Load Int Type Expression -- TODO: type necessary?
               | Store Expression Expression
               | Bitcast Int Type Expression
               | GEP Int Expression [Expression]
  deriving (Eq)

data Expression = Literal Literal
                | GlobalReference Name Type
                | LocalReference Name Type
                | BitcastGlobalRef Name Type Type
  deriving (Eq)

data Linkage = External | Private | LinkOnceODR
  deriving (Eq)

data Arg = Arg Name Type
  deriving (Eq)

data Literal = IntegerLiteral (Maybe Name) Int Integer | FloatLiteral (Maybe Name) Double
  deriving (Eq)

data Type = I Int | F64 | Ptr Type | Function Type [Type] | Custom Name Type | Opaque
          | Array Int Type | Vector Int Type | Struct [Type] | Packed [Type]
  deriving (Eq)


-- * Show instances

instance Show Module where
  show (Module target globals) = if null globals then "" else show target ++ "\n\n" ++ go globals where
    go [] = ""
    go [g] = show g
    go (a@FunctionDefinition{}:b@FunctionDefinition{}:gs) = show a ++ "\n" ++ go (b:gs)
    go (a:b:gs) | globalKind a == globalKind b = show a ++ go (b:gs)
    go (a:b:gs) = show a ++ "\n" ++ go (b:gs)

instance Show Target where
  show (Target layout triple) = "target datalayout = " ++ show layout ++ "\ntarget triple = \"" ++ show triple ++ "\""

instance Show DataLayout where
  show (DataLayout endian mangling aligns ints stack) = "\"" ++ intercalate "-" parts ++ "\"" where
    parts = [show endian, show mangling] ++ map showAlign aligns ++
            ["n" ++ intercalate ":" (map show ints), maybe "" (("S" ++) . show) stack]
    showAlign (ty, abi, pref) = show ty ++ ":" ++ show abi ++ maybe "" ((":" ++) . show) pref

instance Show Triple where
  show (Triple arch vendor os) = intercalate "-" [arch, vendor, os]

instance Show Endian where
  show BigEndian = "E"
  show LittleEndian = "e"

instance Show Mangling where
  show Elf = "m:e"
  show Mips = "m:m"
  show MachO = "m:o"
  show Windows = "m:w"
  show Windowsx86 = "m:x"

instance Show Global where
  show (VariableDefinition name linkage value) = "@" ++ show name ++ " = " ++ withSpace linkage ++ "unnamed_addr constant " ++ withType value ++ ", align 16\n"
  show (Alias to from ty) = "@" ++ show to ++ " = unnamed_addr alias " ++ show ty ++ ", " ++ show ty ++ "* @" ++ show from ++ "\n"
  show (FunctionDefinition name linkage args assigns ret) =
    "define " ++ withSpace linkage ++ show (typeOf ret) ++ " @" ++ show name ++ "(" ++ intercalate ", " (map withType args) ++ ") unnamed_addr align 16 nounwind {\n"
    ++ concatMap (\a -> "  " ++ show a ++ "\n") assigns
    ++ "  ret " ++ withType ret ++ "\n}\n"
  show (TypeDef name ty) = "%" ++ show name ++ " = type " ++ show ty ++ "\n"
  show (FunctionDeclaration name ty) = "declare " ++ show (returnType $ Ptr ty) ++ " @" ++ show name ++ "("
    ++ intercalate ", " (map show $ argTypes ty) ++ ") align 16 nounwind\n"

instance Show Statement where
  show (Call n f args) = "%" ++ show n ++ " = call " ++ show (returnType $ typeOf f) ++ " " ++ show f ++ "(" ++
    intercalate ", " (map withType args) ++ ")"
  show (Load n ty value) = "%" ++ show n ++ " = load " ++ show ty ++ ", " ++ withType value
  show (Store from to) = "store " ++ withType from ++ ", " ++ withType to
  show (Bitcast n ty value) = "%" ++ show n ++ " = bitcast " ++ withType value ++ " to " ++ show ty
  show (GEP n p indices) = "%" ++ show n ++ " = getelementptr " ++ show (deref $ typeOf p) ++ ", " ++ withType p ++
                           concatMap ((", " ++) . withType) indices

instance Show Expression where
  show (Literal value) = show value
  show (GlobalReference name _) = "@" ++ show name
  show (LocalReference name _) = "%" ++ show name
  show (BitcastGlobalRef name from to) = "bitcast(" ++ show from ++ "* @" ++ show name ++ " to " ++ show to ++ ")"

instance Show Linkage where
  show External = ""
  show Private = "private"
  show LinkOnceODR = "linkonce_odr"

instance Show Arg where
  show (Arg name _) = "%" ++ show name

instance Show Literal where
  show (IntegerLiteral _ _ n) = show n
  show (FloatLiteral _ n) = show n

instance Show Type where
  show (I n) = "i" ++ show n
  show F64 = "double"
  show (Ptr ty) = show ty ++ "*"
  show (Function ret args) = show ret ++ " (" ++ intercalate ", " (map show args) ++ ")"
  show (Custom name _) = "%" ++ show name
  show Opaque = "opaque"
  show (Array n ty) = "[" ++ show n ++ " x " ++ show ty ++ "]"
  show (Vector n ty) = "<" ++ show n ++ " x " ++ show ty ++ ">"
  show (Struct tys) = "{" ++ intercalate ", " (map show tys) ++ "}"
  show (Packed tys) = "<{" ++ intercalate ", " (map show tys) ++ "}>"

withSpace :: Show a => a -> String
withSpace a = if null (show a) then "" else show a ++ " "


-- * Typed instances

instance Typed Global where
  typeOf (VariableDefinition _ _ expr) = Ptr $ typeOf expr
  typeOf (Alias _ _ ty) = Ptr ty
  typeOf (FunctionDefinition _ _ args _ expr) = Ptr $ Function (typeOf expr) $ map typeOf args
  typeOf (TypeDef _ _) = undefined

instance Typed Expression where
  typeOf (Literal value) = typeOf value
  typeOf (GlobalReference _ ty) = Ptr ty
  typeOf (LocalReference _ ty) = ty
  typeOf (BitcastGlobalRef _ _ ty) = ty

instance Typed Arg where
  typeOf (Arg _ ty) = ty

instance Typed Literal where
  typeOf (IntegerLiteral name bits _) = maybe (I bits) (flip Custom $ I bits) name
  typeOf (FloatLiteral name _) = maybe F64 (flip Custom F64) name

class Typed a where
  typeOf :: a -> Type

withType :: (Typed a, Show a) => a -> String
withType a = show (typeOf a) ++ " " ++ show a


-- * Utilities

deref :: Type -> Type
deref (Ptr a) = a
deref a = a

argTypes :: Type -> [Type]
argTypes (Function to from) = from ++ argTypes to
argTypes _ = []

argReference :: Arg -> Expression
argReference (Arg name ty) = LocalReference name ty

rename :: Name -> Global -> Global
rename n (VariableDefinition _ linkage expr) = VariableDefinition n linkage expr
rename n (Alias _ m ty) = Alias n m ty
rename n (FunctionDefinition _ linkage args assigns result) = FunctionDefinition n linkage args assigns result
rename n (TypeDef _ ty) = TypeDef n ty

returnType :: Type -> Type
returnType (Ptr (Function a _)) = a
returnType a = error $ "cannot call a value of non-function-pointer type: " ++ show a


-- * Misc

data GlobalKind = GlobalVariable | GlobalAlias | GlobalFunction | GlobalType | GlobalDeclaration deriving (Eq)

globalKind :: Global -> GlobalKind
globalKind VariableDefinition{} = GlobalVariable
globalKind Alias{} = GlobalAlias
globalKind FunctionDefinition{} = GlobalFunction
globalKind TypeDef{} = GlobalType
globalKind FunctionDeclaration{} = GlobalDeclaration
