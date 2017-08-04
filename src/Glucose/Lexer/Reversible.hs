module Glucose.Lexer.Reversible where

import Control.Monad.Except
import Data.Monoid
import Data.Text as Text
import Glucose.Lexer
import Glucose.Lexer.SyntaxError
import Glucose.Source
import Glucose.Token

-- | Whitespace text.
newtype Whitespace = Whitespace Text deriving (Eq, Show)

-- | UTF8 text split into tokens, with sufficient context to reconstruct the original text.
data TokenisedReversible = TokenisedReversible Whitespace [ReversibleToken] deriving (Eq, Show)

-- | A token with sufficient context to reconstruct the original text.
data ReversibleToken = ReversibleToken { token :: Token, _location :: Location, lexeme :: Text, leadingSpace :: Text }
  deriving (Eq, Show)

instance Located ReversibleToken where
  location = _location

tokeniseReversible :: MonadError SyntaxError m => Text -> m (Location, TokenisedReversible)
tokeniseReversible input = fmap fromTokens <$> tokenise input where
  fromTokens = uncurry TokenisedReversible . go 0 input where
    go _ remainder [] = (Whitespace remainder, [])
    go prev input (FromSource (SourceRange start end) token : rest) =
      let (leadingSpace, more) = Text.splitAt (codePoint start - prev) input
          (lexeme, remainder) = Text.splitAt (codePoint end - codePoint start + 1) more
       in (ReversibleToken token start lexeme leadingSpace :) <$> go (codePoint end + 1) remainder rest

detokenise :: TokenisedReversible -> Text
detokenise (TokenisedReversible (Whitespace trailingSpace) ts) = foldMap expand ts <> trailingSpace where
  expand ReversibleToken{leadingSpace, lexeme} = leadingSpace <> lexeme
