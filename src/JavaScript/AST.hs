module JavaScript.AST where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import JavaScript.Name

newtype JavaScript = JavaScript [Definition]
  deriving (Eq)

data Definition
  = Assign Identifier Expression
  | Function Name [Name] (Maybe Expression)
  deriving (Eq)

data Expression
  = Reference Identifier
  | IntegerLiteral Int
  | FloatLiteral Double
  | Call Expression [Expression]
  | New Expression [Expression]
  | Lambda [Name] (Maybe Expression)
  deriving (Eq)

newtype Identifier = Identifier [Text]
  deriving (Eq)


-- * Show instances

instance Show JavaScript where
  show (JavaScript defs) = unlines $ map show defs

instance Show Definition where
  show (Assign n expr) = show n <> " = " <> show expr
  show (Function n as def) = showFunction (Just n) as def

instance Show Expression where
  show (Reference n) = show n
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a
  show (Call f as) = show f <> parenList (map show as)
  show (New f as) = "new " <> show (Call f as)
  show (Lambda as def) = showFunction Nothing as def

instance Show Identifier where
  show (Identifier ns) = intercalate "." $ map unpack ns

showFunction :: Maybe Name -> [Name] -> Maybe Expression -> String
showFunction n as def = "function" <> maybeName n <> parenList (map show as) <> " {" <> statements def <> "}" where
  statements = maybe "" $ \expr -> " return " <> show expr <> " "
  maybeName = maybe "" $ \name -> " " <> show name

parenList :: [String] -> String
parenList as = "(" <> intercalate ", " as <> ")"
