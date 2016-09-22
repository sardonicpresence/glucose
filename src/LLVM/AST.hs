module LLVM.AST where

import Data.List
import LLVM.Name

data Module = Module Target [Global] deriving (Eq)

data Global = VariableDefinition Name Linkage Expression
            | Alias Name Name Type
            | FunctionDefinition Name Linkage [Arg] [BasicBlock]
            | TypeDef Name Type
            | FunctionDeclaration Name Type
  deriving (Eq)

data BasicBlock = BasicBlock (Maybe Name) [Statement] Terminator deriving (Eq)

data Statement = Assignment Name Assignment
               | Store Expression Expression
  deriving (Eq)

data Assignment = Call Expression [Expression]
                | Load Expression
                | Bitcast Expression Type
                | GEP Expression [Expression]
                | PtrToInt Expression Type
                | BinaryOp BinaryOp Expression Expression
  deriving (Eq)

data Terminator = Return Expression
                | Branch Expression Name Name
  deriving (Eq)

data Expression = Literal Literal
                | GlobalReference Name Type
                | LocalReference Name Type
                | BitcastGlobalRef Name Type Type
  deriving (Eq)

data Arg = Arg Name Type deriving (Eq)

data Literal = IntegerLiteral (Maybe Name) Int Integer
             | FloatLiteral (Maybe Name) Double
  deriving (Eq)

data Type = Void | I Int | F64 | Ptr Type | Function Type [Type] | Custom Name Type | Opaque
          | Array Int Type | Vector Int Type | Struct [Type] | Packed [Type]
  deriving (Eq)

data Linkage = External | Private | LinkOnceODR deriving (Eq)

data BinaryOp = And | ICmp Comparison deriving (Eq)

data Comparison = Eq deriving (Eq)

-- * Target

data Target = Target DataLayout Triple deriving (Eq)

data DataLayout = DataLayout Endian Mangling [(Type, Int, Maybe Int)] [Int] (Maybe Int) deriving (Eq)

data Endian = BigEndian | LittleEndian deriving (Eq)

data Mangling = Elf | Mips | MachO | Windows | Windowsx86 deriving (Eq)

data Triple = Triple String String String deriving (Eq)


-- * Show instances

instance Show Module where
  show (Module target globals) = if null globals then "" else show target ++ "\n" ++ go globals where
    go [] = ""
    go [g] = show g
    go (a@FunctionDefinition{}:b@FunctionDefinition{}:gs) = show a ++ "\n" ++ go (b:gs)
    go (a:b:gs) | globalKind a == globalKind b = show a ++ go (b:gs)
    go (a:b:gs) = show a ++ "\n" ++ go (b:gs)

instance Show Global where
  show (VariableDefinition name linkage value) =
    global name ++ " = " ++ withSpace linkage ++ "unnamed_addr constant " ++ withType value ++ ", " ++ alignment ++ "\n"
  show (Alias to from ty) =
    global to ++ " = unnamed_addr alias " ++ show ty ++ ", " ++ show ty ++ "* " ++ global from ++ "\n"
  show (FunctionDefinition name linkage args blocks) =
    "define " ++ withSpace linkage ++ show (typeOf $ last blocks) ++ " " ++ global name ++ "(" ++ arguments args ++ ") " ++ functionAttributes
              ++ " {\n" ++ concatMap show blocks ++ "}\n"
  show (TypeDef name ty) = local name ++ " = type " ++ show ty ++ "\n"
  show (FunctionDeclaration name ty) =
    "declare " ++ show (returnType ty) ++ " " ++ global name ++ "(" ++ args ++ ") " ++ functionAttributes ++ " \n"
    where args = intercalate ", " . map show $ argTypes ty

instance Show BasicBlock where
  show (BasicBlock label statements terminator) =
    maybe "" ((++ ":\n") . show) label ++ concatMap showLine statements ++ showLine terminator
    where showLine s = "  " ++ show s ++ "\n"

instance Show Statement where
  show (Assignment name assignment) = local name ++ " = " ++ show assignment
  show (Store from to) = "store " ++ withType from ++ ", " ++ withType to

instance Show Assignment where
  show (Call f args) = "tail call " ++ show (returnType . deref $ typeOf f) ++ " " ++ show f ++ "(" ++ arguments args ++ ")"
  show (Load value) = "load " ++ show (deref $ typeOf value) ++ ", " ++ withType value
  show (Bitcast value ty) = "bitcast " ++ withType value ++ " to " ++ show ty
  show (GEP p indices) =
    "getelementptr " ++ show (deref $ typeOf p) ++ ", " ++ withType p ++ concatMap ((", " ++) . withType) indices
  show (PtrToInt expr ty) = "ptrtoint " ++ withType expr ++ " to " ++ show ty
  show (BinaryOp op a b) = show op ++ " " ++ withType a ++ ", " ++ show b

instance Show Terminator where
  show (Return expr) = "ret " ++ withType expr
  show (Branch cond ifTrue ifFalse) = "br " ++ withType cond ++ ", label " ++ local ifTrue ++ ", label " ++ local ifFalse

instance Show BinaryOp where
  show And = "and"
  show (ICmp comparison) = "icmp " ++ show comparison

instance Show Comparison where
  show Eq = "eq"

instance Show Expression where
  show (Literal value) = show value
  show (GlobalReference name _) = global name
  show (LocalReference name _) = local name
  show (BitcastGlobalRef name from to) = "bitcast(" ++ show from ++ "* " ++ global name ++ " to " ++ show to ++ ")"

instance Show Linkage where
  show External = ""
  show Private = "private"
  show LinkOnceODR = "linkonce_odr"

instance Show Arg where
  show (Arg name _) = local name

instance Show Literal where
  show (IntegerLiteral _ _ n) = show n
  show (FloatLiteral _ n) = show n

instance Show Type where
  show Void = "void"
  show (I n) = "i" ++ show n
  show F64 = "double"
  show (Ptr ty) = show ty ++ "*"
  show (Function ret args) = show ret ++ " (" ++ intercalate ", " (map show args) ++ ")"
  show (Custom name _) = local name
  show Opaque = "opaque"
  show (Array n ty) = "[" ++ show n ++ " x " ++ show ty ++ "]"
  show (Vector n ty) = "<" ++ show n ++ " x " ++ show ty ++ ">"
  show (Struct tys) = "{" ++ intercalate ", " (map show tys) ++ "}"
  show (Packed tys) = "<{" ++ intercalate ", " (map show tys) ++ "}>"

instance Show Target where
  show (Target layout triple) = "target datalayout = " ++ show layout ++ "\n" ++
                                "target triple = \"" ++ show triple ++ "\"\n"

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

local :: Name -> String
local name = "%" ++ show name

global :: Name -> String
global name = "@" ++ show name

arguments :: (Typed a, Show a) => [a] -> String
arguments = intercalate ", " . map withType

functionAttributes :: String
functionAttributes = "unnamed_addr " ++ alignment ++ " nounwind"

alignment :: String
alignment = "align 16"

withSpace :: Show a => a -> String
withSpace a = if null (show a) then "" else show a ++ " "


-- * Typed instances

class Typed a where
  typeOf :: a -> Type

instance Typed Global where
  typeOf (VariableDefinition _ _ expr) = typeOf expr
  typeOf (Alias _ _ ty) = Ptr ty
  typeOf (FunctionDefinition _ _ args blocks) = Function (typeOf $ last blocks) $ map typeOf args
  typeOf (TypeDef _ ty) = ty
  typeOf (FunctionDeclaration _ ty) = ty

instance Typed BasicBlock where
  typeOf (BasicBlock _ _ terminator) = typeOf terminator

instance Typed Assignment where
  typeOf (Call f _) = returnType $ typeOf f
  typeOf (Load expr) = deref $ typeOf expr
  typeOf (Bitcast _ ty) = ty
  typeOf (GEP p indices) = Ptr $ foldl dereference (typeOf p) indices where
    dereference ty index = case ty of
      Custom _ ty -> dereference ty index
      Ptr ty -> ty
      Array _ ty -> ty
      Struct tys -> tys !! case index of
        Literal (IntegerLiteral _ _ a) -> fromInteger a
        Literal a -> error $ "cannot use non-integral type as an index: " ++ show (typeOf a)
        _ -> error "cannot use a non-constant expression to index into a structure type"
      _ -> error $ "cannot getElementPtr of non-aggregate value type: " ++ show ty
  typeOf (PtrToInt _ ty) = ty
  typeOf (BinaryOp op a _) = opType op $ typeOf a

instance Typed Terminator where
  typeOf (Return expr) = typeOf expr
  typeOf Branch{} = Void

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

withType :: (Typed a, Show a) => a -> String
withType a = show (typeOf a) ++ " " ++ show a

opType :: BinaryOp -> Type -> Type
opType And ty = ty
opType (ICmp _) _ = I 1


-- * Utilities

deref :: Type -> Type
deref (Ptr a) = a
deref a = a

argTypes :: Type -> [Type]
argTypes (Function to from) = from ++ argTypes to
argTypes _ = []

argReference :: Arg -> Expression
argReference (Arg name ty) = LocalReference name ty

returnType :: Type -> Type
returnType (Ptr a) = returnType a
returnType (Function a _) = a
returnType a = error $ "cannot call a value of non-function type: " ++ show a


-- * Misc

data GlobalKind = GlobalVariable | GlobalAlias | GlobalFunction | GlobalType | GlobalDeclaration deriving (Eq)

globalKind :: Global -> GlobalKind
globalKind VariableDefinition{} = GlobalVariable
globalKind Alias{} = GlobalAlias
globalKind FunctionDefinition{} = GlobalFunction
globalKind TypeDef{} = GlobalType
globalKind FunctionDeclaration{} = GlobalDeclaration
