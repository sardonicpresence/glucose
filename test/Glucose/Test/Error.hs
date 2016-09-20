module Glucose.Test.Error where

import Data.Text
import Glucose.Error
import Glucose.Parser.EOFOr
import Glucose.Parser.Source
import Glucose.Token

compileError :: String -> ErrorDetails -> CompileError
compileError loc = CompileError (read loc)

unexpectedEof :: String -> [Text] -> CompileError
unexpectedEof loc es = CompileError (read loc) (Unexpected (Right EOF) Nothing es)

unexpectedToken :: String -> FromSource Token -> [Text] -> CompileError
unexpectedToken loc t es = CompileError (read loc) (Unexpected (Right (NotEOF t)) Nothing es)

unexpectedChar :: String -> Char -> Text -> CompileError
unexpectedChar loc c = unexpectedThing loc (pack ['\'', c, '\''])

unexpectedThing :: String -> Text -> Text -> CompileError
unexpectedThing loc a ctxt = CompileError (read loc) (Unexpected (Left a) (Just ctxt) [])
