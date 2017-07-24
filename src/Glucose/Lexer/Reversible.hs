module Glucose.Lexer.Reversible where

import Control.Comonad
import Data.Monoid
import Data.Text as Text
import Glucose.Error
import Glucose.Lexer
import Glucose.Lexer.Location
import Glucose.Parser.Source
import Glucose.Token

-- | Whitespace text.
newtype Whitespace = Whitespace Text deriving (Show)

-- | UTF8 text split into tokens, with sufficient context to reconstruct the original text.
data TokenisedReversible = TokenisedReversible Whitespace [ReversibleToken]

-- | A token with sufficient context to reconstruct the original text.
data ReversibleToken = ReversibleToken { token :: Token, location :: Location, lexeme :: Text, leadingSpace :: Text }
  deriving (Eq, Ord, Show)

tokeniseReversible :: Text -> Either CompileError TokenisedReversible
tokeniseReversible input = fromTokens . extract <$> tokenise input where
  fromTokens = uncurry TokenisedReversible . go 0 input where
    go _ remainder [] = (Whitespace remainder, [])
    go prev input (FromSource (SourceRange start end) token : rest) =
      let (leadingSpace, more) = Text.splitAt (codePoint start - prev) input
          (lexeme, remainder) = Text.splitAt (codePoint end - codePoint start) more
       in (ReversibleToken token start lexeme leadingSpace :) <$> go (codePoint end) remainder rest

detokenise :: TokenisedReversible -> Text
detokenise (TokenisedReversible (Whitespace trailingSpace) ts) = foldMap expand ts <> trailingSpace

expand :: ReversibleToken -> Text
expand ReversibleToken{leadingSpace, lexeme} = leadingSpace <> lexeme
