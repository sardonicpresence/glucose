module LLVM.AST where

import Control.Comonad
import Control.Lens
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import LLVM.Name

data Module = Module Target [Global] deriving (Eq)

-- TODO: split things without types e.g. TypeDef & AttributeGroup into different type
data Global = VariableDefinition Name Linkage UnnamedAddr Expression Alignment
            | Alias Name Linkage UnnamedAddr Expression Type
            | FunctionDefinition Name Linkage CallingConvention [Parameter Arg] FunctionAttributes (Result [BasicBlock])
            | TypeDef Name Type
            | FunctionDeclaration Name Linkage CallingConvention (Result Type) [Parameter Type] FunctionAttributes
            | AttributeGroup Int [String]
  deriving (Eq)

data Result a = Result [String] Alignment a deriving (Eq, Functor)

data Parameter a = Parameter [String] Alignment a deriving (Eq, Functor)

data FunctionAttributes = FunctionAttributes UnnamedAddr [String] [Int] Alignment deriving (Eq)

noAttributes :: UnnamedAddr -> FunctionAttributes
noAttributes addr = FunctionAttributes addr [] [] (Alignment 0)

data BasicBlock = BasicBlock (Maybe Name) [Statement] Terminator deriving (Eq)

data Statement = Assignment Name Assignment
               | VoidCall CallingConvention Expression [Expression]
               | Store Expression Expression
               | Comment String
  deriving (Eq)

data Assignment = Call CallingConvention Expression [Expression]
                | Load Expression
                | GEP Expression [Expression]
                | Convert ConversionOp Expression Type
                | BinaryOp BinaryOp Expression Expression
                | Phi (Expression, Name) [(Expression, Name)]
                | Alloca Type Expression
  deriving (Eq)

data Terminator = Return Expression
                | Branch Expression Name Name
                | Switch Expression Name [(Expression, Name)]
                | Jump Name
                | Unreachable
  deriving (Eq)

data Expression = Literal Literal
                | Undefined Type
                | GlobalReference Name Type
                | LocalReference Name Type
                | ConstGEP Expression [Expression]
                | ConstConvert ConversionOp Expression Type
                | ConstBinaryOp BinaryOp Expression Expression
                | Placeholder Name Type
  deriving (Eq)

data Arg = Arg Name Type deriving (Eq)

data Literal = IntegerLiteral (Maybe Name) Int Integer
             | FloatLiteral (Maybe Name) Double
             | ZeroInitializer Type
             | StringLiteral String
  deriving (Eq)

data Type = Void | I Int | F64 | Ptr Type | Function Type [Type] | Custom Name Type | Opaque
          | Array Int Type | Vector Int Type | Struct [Type] | Packed [Type]
  deriving (Eq)

data Linkage = External | Private | LinkOnceODR deriving (Eq)

data UnnamedAddr = Named | Unnamed | LocalUnnamed deriving (Eq)

newtype Alignment = Alignment Int deriving (Eq) -- 0 indicates no alignment

data CallingConvention = CCC | FastCC | StdCall deriving (Eq)

data ConversionOp = Bitcast | PtrToInt | IntToPtr | Trunc | Zext deriving (Eq)

data BinaryOp = And | Or | Xor | Add | Sub | Mul | ICmp Comparison deriving (Eq)

data Comparison = Eq | Ne | Ugt | Uge | Ult | Ule | Sgt | Sge | Slt | Sle deriving (Eq)

-- * Target

data Target = Target DataLayout Triple deriving (Eq)

data DataLayout = DataLayout Endian Mangling [(Type, Int, Maybe Int)] [Int] (Maybe Int) deriving (Eq)

data Endian = BigEndian | LittleEndian deriving (Eq)

data Mangling = Elf | Mips | MachO | Windows | Windowsx86 deriving (Eq)

data Triple = Triple String String String deriving (Eq)

-- * Lenses

class References a where
  expressions :: Traversal' a Expression
instance References Statement where
  expressions f (Assignment name assignment) = Assignment name <$> expressions f assignment
  expressions f (VoidCall cc fn args) = VoidCall cc <$> f fn <*> traverse f args
  expressions f (Store from to) = Store <$> f from <*> f to
  expressions _ (Comment s) = pure $ Comment s
instance References Assignment where
  expressions f (Call cc fn args) = Call cc <$> f fn <*> traverse f args
  expressions f (Load ptr) = Load <$> f ptr
  expressions f (GEP ptr indices) = GEP <$> f ptr <*> traverse f indices
  expressions f (Convert op arg ty) = Convert op <$> f arg <*> pure ty
  expressions f (BinaryOp op a b) = BinaryOp op <$> f a <*> f b
  expressions f (Phi pred preds) = Phi <$> _1 f pred <*> traverse (_1 f) preds
  expressions f (Alloca ty n) = Alloca ty <$> f n

-- * Show instances

instance Show Module where
  show (Module target globals) = if null globals then "" else show target ++ "\n" ++ go globals where
    go [] = ""
    go [g] = show g
    go (a@FunctionDefinition{}:b@FunctionDefinition{}:gs) = show a ++ "\n" ++ go (b:gs)
    go (a:b:gs) | globalKind a == globalKind b = show a ++ go (b:gs)
    go (a:b:gs) = show a ++ "\n" ++ go (b:gs)

instance Show Global where
  show (VariableDefinition name linkage addr value alignment) =
    global name ++ " =" ++ withSpace linkage ++ withSpace addr
                ++ " constant " ++ withType value ++ withComma alignment ++ "\n"
  show (Alias to linkage addr from ty) =
    global to ++ " =" ++ withSpace linkage ++ withSpace addr
              ++ " alias " ++ show ty ++ ", " ++ show (Ptr ty) ++ " " ++ show from ++ "\n"
  show (FunctionDefinition name linkage cc args attrs blocks) =
    "define" ++ withSpace linkage ++ withSpace cc ++ " " ++ show (defReturnType <$> blocks) ++ " "
             ++ global name ++ "(" ++ arguments args ++ ")" ++ withSpace attrs
             ++ " {\n" ++ concatMap show (extract blocks) ++ "}\n"
  show (TypeDef name ty) = local name ++ " = type " ++ show ty ++ "\n"
  show (FunctionDeclaration name linkage cc result args attrs) =
    "declare" ++ withSpace linkage ++ withSpace cc ++ withSpace result ++ " "
              ++ global name ++ "(" ++ intercalate ", " (map show args) ++ ")" ++ withSpace attrs ++ " \n"
  show (AttributeGroup n attrs) =
    "attributes #" ++ show n ++ " = { " ++ unwords attrs ++ " }"

instance Show a => Show (Parameter a) where
  show (Parameter attrs alignment a) = show a ++ concatMap (" " ++) attrs ++ withSpace alignment

instance Show a => Show (Result a) where
  show (Result attrs alignment a) = concatMap (++ " ") attrs ++ withSpaceAfter alignment ++ show a

instance Show FunctionAttributes where
  show (FunctionAttributes addr attrs groups alignment) =
    show addr ++ concatMap (" " ++) attrs ++ concatMap ((" #" ++) . show) groups ++ withSpace alignment

instance Show BasicBlock where
  show (BasicBlock label statements terminator) =
    maybe "" ((++ ":\n") . show) label ++ concatMap showLine statements ++ showLine terminator
    where showLine s = "  " ++ show s ++ "\n"

instance Show Statement where
  show (VoidCall cc f args) = show (Call cc f args)
  show (Assignment name assignment) = local name ++ " = " ++ show assignment
  show (Store from to) = "store " ++ withType from ++ ", " ++ withType to
  show (Comment s) = "; " ++ s

instance Show Assignment where
  show (Call cc f args) = "tail call" ++ withSpace cc ++ " " ++ show (returnType . deref $ typeOf f) ++ " " ++ show f ++ "(" ++ arguments args ++ ")"
  show (Load value) = "load " ++ show (deref $ typeOf value) ++ ", " ++ withType value
  show (GEP p indices) =
    "getelementptr inbounds " ++ show (deref $ typeOf p) ++ ", " ++ withType p ++ concatMap ((", " ++) . withType) indices
  show (Convert op expr ty) = show op ++ " " ++ withType expr ++ " to " ++ show ty
  show (BinaryOp op a b) = show op ++ " " ++ withType a ++ ", " ++ show b
  show phi@(Phi pred preds) = "phi " ++ show (typeOf phi) ++ " " ++ intercalate ", " (map showPred $ pred:preds) where
    showPred (value, label) = "[" ++ show value ++ ", " ++ local label ++ "]"
  show (Alloca ty n) = "alloca " ++ show ty ++ ", " ++ withType n ++ ", align 16"

instance Show Terminator where
  show (Return expr) = "ret " ++ withType expr
  show (Branch cond ifTrue ifFalse) = "br " ++ withType cond ++ ", label " ++ local ifTrue ++ ", label " ++ local ifFalse
  show (Switch cond def cases) = "switch " ++ withType cond ++ ", label " ++ local def ++ " [ " ++
    unwords (map (\(val, label) -> withType val ++ ", label " ++ local label) cases) ++ " ]"
  show (Jump label) = "br label " ++ local label
  show Unreachable = "unreachable"

instance Show ConversionOp where
  show Bitcast = "bitcast"
  show PtrToInt = "ptrtoint"
  show IntToPtr = "inttoptr"
  show Trunc = "trunc"
  show Zext = "zext"

instance Show BinaryOp where
  show And = "and"
  show Or = "or"
  show Xor = "xor"
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show (ICmp comparison) = "icmp " ++ show comparison

instance Show Comparison where
  show Eq = "eq"
  show Ne = "ne"
  show Ugt = "ugt"
  show Uge = "uge"
  show Ult = "ult"
  show Ule = "ule"
  show Sgt = "sgt"
  show Sge = "sge"
  show Slt = "slt"
  show Sle = "sle"

instance Show Expression where
  show (Literal value) = show value
  show (Undefined _) = "undef"
  show (GlobalReference name _) = global name
  show (LocalReference name _) = local name
  show (ConstGEP p indices) =
    "getelementptr inbounds (" ++ show (deref $ typeOf p) ++ ", " ++ withType p ++ concatMap ((", " ++) . withType) indices ++ ")"
  show (ConstConvert op expr ty) = show op ++ "(" ++ withType expr ++ " to " ++ show ty ++ ")"
  show (ConstBinaryOp op a b) = show op ++ "(" ++ withType a ++ ", " ++ withType b ++ ")"
  show Placeholder{} = error "Placeholder has not been replaced"

instance Show Linkage where
  show External = ""
  show Private = "private"
  show LinkOnceODR = "linkonce_odr"

instance Show UnnamedAddr where
  show Named = ""
  show Unnamed = "unnamed_addr"
  show LocalUnnamed = "local_unnamed_addr"

instance Show Alignment where
  show (Alignment 0) = ""
  show (Alignment n) = "align " ++ show n

instance Show CallingConvention where
  show CCC = "" -- Default
  show FastCC = "fastcc"
  show StdCall = "x86_stdcallcc"

instance Show Arg where
  show (Arg name _) = local name

instance Show Literal where
  show (IntegerLiteral _ _ n) = show n
  show (FloatLiteral _ n) = show n
  show (ZeroInitializer _) = "zeroinitializer"
  show (StringLiteral s) = "c\"" ++ concatMap mangleChar s ++ "\"" where
    mangleChar c = if isControl c then "\\" ++ hex (ord c) else [c]
    hex c = map toUpper [intToDigit (shift c (-8)), intToDigit (c .&. 0xF)]

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

withSpace :: Show a => a -> String
withSpace = withPrefix " "

withSpaceAfter :: Show a => a -> String
withSpaceAfter = withSuffix " "

withComma :: Show a => a -> String
withComma = withPrefix ", "

withPrefix :: Show a => String -> a -> String
withPrefix prefix a = if null (show a) then "" else prefix ++ show a

withSuffix :: Show a => String -> a -> String
withSuffix suffix a = if null (show a) then "" else show a ++ suffix


-- * Typed instances

class Typed a where
  typeOf :: a -> Type

instance Typed Type where
  typeOf = id

instance Typed Global where
  typeOf (VariableDefinition _ _ _ expr _) = Ptr $ typeOf expr
  typeOf (Alias _ _ _ _ ty) = Ptr ty
  typeOf (FunctionDefinition _ _ _ args _ blocks) = Ptr $ Function (defReturnType $ extract blocks) $ map typeOf args
  typeOf TypeDef{} = undefined
  typeOf (FunctionDeclaration _ _ _ result args _) = Ptr $ Function (typeOf result) (map typeOf args)
  typeOf AttributeGroup{} = undefined

instance Typed a => Typed (Result a) where
  typeOf (Result _ _ a)= typeOf a

instance Typed a => Typed (Parameter a) where
  typeOf (Parameter _ _ a)= typeOf a

instance Typed BasicBlock where
  typeOf (BasicBlock _ _ terminator) = typeOf terminator

instance Typed Assignment where
  typeOf (Call _ f _) = returnType $ typeOf f
  typeOf (Load expr) = deref $ typeOf expr
  typeOf (GEP p indices) = Ptr $ foldl dereference (typeOf p) indices where
    dereference ty index = case ty of
      Custom _ ty -> dereference ty index
      Ptr ty -> ty
      Array _ ty -> ty
      Packed tys -> derefStruct tys
      Struct tys -> derefStruct tys
      _ -> error $ "cannot getelementptr of non-aggregate value type: " ++ show ty
      where
        derefStruct tys = tys !! case index of
          Literal (IntegerLiteral _ _ a) | fromInteger a < length tys -> fromInteger a
          Literal (IntegerLiteral _ _ a) -> error $ "getelementptr index " ++ show a ++ " is out-of-bounds for " ++ show ty
          Literal a -> error $ "cannot use non-integral type as an index: " ++ show (typeOf a)
          _ -> error "cannot use a non-constant expression to index into a structure type"
  typeOf (Convert _ _ ty) = ty
  typeOf (BinaryOp op a _) = opType op $ typeOf a
  typeOf (Phi (a,_) _) = typeOf a
  typeOf (Alloca ty _) = Ptr ty

instance Typed Terminator where
  typeOf (Return expr) = typeOf expr
  typeOf _ = Void

instance Typed Expression where
  typeOf (Literal value) = typeOf value
  typeOf (Undefined ty) = ty
  typeOf (GlobalReference _ ty) = ty
  typeOf (LocalReference _ ty) = ty
  typeOf (ConstGEP p indices) = typeOf $ GEP p indices
  typeOf (ConstConvert _ _ ty) = ty
  typeOf (ConstBinaryOp op a _) = opType op $ typeOf a
  typeOf (Placeholder _ ty) = ty

instance Typed Arg where
  typeOf (Arg _ ty) = ty

instance Typed Literal where
  typeOf (IntegerLiteral name bits _) = maybe (I bits) (flip Custom $ I bits) name
  typeOf (FloatLiteral name _) = maybe F64 (flip Custom F64) name
  typeOf (ZeroInitializer ty) = ty
  typeOf (StringLiteral s) = Array (length s) (I 8)

withType :: (Typed a, Show a) => a -> String
withType a = show (typeOf a) ++ " " ++ show a

opType :: BinaryOp -> Type -> Type
opType (ICmp _) _ = I 1
opType _ ty = ty


-- * Utilities

deref :: Type -> Type
deref (Ptr a) = a
deref ty = error $ "Expected pointer instead of " ++ show ty

argTypes :: Type -> [Type]
argTypes (Function to from) = from ++ argTypes to
argTypes _ = []

argReference :: Arg -> Expression
argReference (Arg name ty) = LocalReference name ty

returnType :: Type -> Type
returnType (Ptr a) = returnType a
returnType (Function a _) = a
returnType a = error $ "cannot call a value of non-function type: " ++ show a

applied :: Type -> Type
applied (Ptr a) = applied a
applied (Function r [_]) = r
applied (Function r (_:as)) = Function r as
applied a = error $ "cannot call a value of non-function type: " ++ show a

defReturnType :: [BasicBlock] -> Type
defReturnType blocks = fromMaybe Void . listToMaybe . filter (/= Void) $ map typeOf blocks

sameRepresentation :: Type -> Type -> Bool
sameRepresentation a b | a == b = True
sameRepresentation (Custom _ a) b | sameRepresentation a b = True
sameRepresentation a (Custom _ b) | sameRepresentation a b = True
sameRepresentation (Ptr _) (Ptr _) = True
sameRepresentation _ _ = False

typeSize :: Type -> Int
typeSize (I n) = n
typeSize F64 = 64
typeSize (Custom _ ty) = typeSize ty
typeSize (Array n ty) = n * typeSize ty
typeSize (Vector n ty) = n * typeSize ty
typeSize ty = error $ "Type " ++ show ty ++ " isn't a primitive type!" -- TODO

nameOf :: Global -> Name
nameOf (VariableDefinition name _ _ _ _) = name
nameOf (Alias name _ _ _ _) = name
nameOf (FunctionDefinition name _ _ _ _ _) = name
nameOf (TypeDef name _) = name -- TODO: Questionable
nameOf (FunctionDeclaration name _ _ _ _ _) = name
nameOf (AttributeGroup _ _) = undefined -- TODO

globalRef :: Global -> Expression
globalRef a = GlobalReference (nameOf a) (typeOf a)


-- * Misc

instance Comonad Result where
  extract (Result _ _ a) = a
  duplicate a@(Result attrs alignment _) = Result attrs alignment a

instance Applicative Result where
  pure = Result [] (Alignment 0)
  (<*>) = error "Result is not really Applicative!"

instance Comonad Parameter where
  extract (Parameter _ _ a) = a
  duplicate a@(Parameter attrs alignment _) = Parameter attrs alignment a

instance Applicative Parameter where
  pure = Parameter [] (Alignment 0)
  (<*>) = error "Parameter is not really Applicative!"

data GlobalKind = GlobalVariable | GlobalAlias | GlobalFunction
                | GlobalType | GlobalDeclaration | GlobalAttributeGroup
  deriving (Eq)

globalKind :: Global -> GlobalKind
globalKind VariableDefinition{} = GlobalVariable
globalKind Alias{} = GlobalAlias
globalKind FunctionDefinition{} = GlobalFunction
globalKind TypeDef{} = GlobalType
globalKind FunctionDeclaration{} = GlobalDeclaration
globalKind AttributeGroup{} = GlobalAttributeGroup
