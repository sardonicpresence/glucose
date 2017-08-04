module Glucose.Lexer.NumericLiteral
(
  NumericLiteral(), numericLiteral, extendNumericLiteral, completeNumericLiteral
)
where

import Control.Monad.Except
import Data.Char
import Data.Ratio
import Glucose.Lexer.SyntaxError
import Glucose.Token

data DecimalPlaces = NoDP | DP Int

data Exponent = NoExponent | Missing | Negative | Exponent Int

data NumericLiteral = NumericLiteral Integer DecimalPlaces Exponent

numericLiteral :: Int -> NumericLiteral
numericLiteral n = NumericLiteral (toInteger n) NoDP NoExponent

extendNumericLiteral :: MonadError SyntaxErrorDetails m => Char -> NumericLiteral -> m (Maybe NumericLiteral)
extendNumericLiteral '.' (NumericLiteral n NoDP NoExponent) = pure.pure $ NumericLiteral n (DP 0) NoExponent
extendNumericLiteral 'e' (NumericLiteral _ (DP 0) NoExponent) = pure Nothing
extendNumericLiteral 'e' (NumericLiteral n dp NoExponent) = pure.pure $ NumericLiteral n dp Missing
extendNumericLiteral '-' (NumericLiteral _ NoDP Missing) = negativeExponent
extendNumericLiteral '-' (NumericLiteral n dp Missing) = pure.pure $ NumericLiteral n dp Negative
extendNumericLiteral c lit | isDigit c = pure.pure $ addDigit (digitToInt c) lit
extendNumericLiteral _ _ = pure Nothing

addDigit :: Int -> NumericLiteral -> NumericLiteral
addDigit d (NumericLiteral n NoDP NoExponent) = NumericLiteral (n*10 + toInteger d) NoDP NoExponent
addDigit d (NumericLiteral n (DP dp) NoExponent) = NumericLiteral (n*10 + toInteger d) (DP (dp+1)) NoExponent
addDigit d (NumericLiteral n dp Missing) = NumericLiteral n dp (Exponent d)
addDigit 0 (NumericLiteral n dp Negative) = NumericLiteral n dp Negative
addDigit d (NumericLiteral n dp Negative) = NumericLiteral n dp (Exponent (-d))
addDigit d (NumericLiteral n dp (Exponent e)) = NumericLiteral n dp (Exponent $ e*10 + d)

completeNumericLiteral :: MonadError SyntaxErrorDetails m => NumericLiteral -> m (Token, Maybe Char)
completeNumericLiteral (NumericLiteral n (DP 0) _) = pure (IntegerLiteral n, Just '.')
completeNumericLiteral (NumericLiteral n NoDP NoExponent) = purely $ IntegerLiteral n
completeNumericLiteral (NumericLiteral n NoDP (Exponent e)) | e >= 0 = purely $ IntegerLiteral $ n * 10^e
completeNumericLiteral (NumericLiteral n (DP dp) NoExponent) = purely $ FloatLiteral $ fromInteger n % 10^dp
completeNumericLiteral (NumericLiteral n (DP dp) (Exponent e)) = purely $ FloatLiteral $ fromInteger n * 10.0^^e / 10^dp
completeNumericLiteral (NumericLiteral _ NoDP (Exponent _)) = negativeExponent
completeNumericLiteral (NumericLiteral _ NoDP Negative) = negativeExponent
completeNumericLiteral _ = throwError $ SyntaxErrorDetails "missing exponent" "numeric literal"

purely :: Applicative f => a -> f (a, Maybe b)
purely a = pure (a, Nothing)

negativeExponent :: MonadError SyntaxErrorDetails m => m a
negativeExponent = throwError $ SyntaxErrorDetails "negative exponent" "integer literal"
