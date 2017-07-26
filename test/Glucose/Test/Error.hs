module Glucose.Test.Error where

import Data.Text
import Glucose.Error
import Glucose.Parser.EOFOr
import Glucose.Source
import Glucose.Token

compileError :: String -> ErrorDetails -> CompileError
compileError loc = CompileError (read loc)

unexpectedEof :: String -> [Text] -> CompileError
unexpectedEof loc es = CompileError (read loc) (Unexpected (Right EOF) Nothing es)

unexpectedToken :: FromSource Token -> [Text] -> CompileError
unexpectedToken t es = CompileError (startLocation t) (Unexpected (Right (NotEOF t)) Nothing es)

unexpectedChar :: String -> Char -> Text -> CompileError
unexpectedChar loc c = unexpectedThing loc (pack ['\'', c, '\''])

unexpectedThing :: String -> Text -> Text -> CompileError
unexpectedThing loc a ctxt = CompileError (read loc) (Unexpected (Left a) (Just ctxt) [])
