module Fine.Codegen.Lit (genLitCode) where

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Fine.Syntax (Lit (..))

genLitCode :: Lit -> Text
genLitCode (Int v) = Text.pack $ show v
genLitCode (Float v) = Text.pack $ show v
genLitCode (Bool True) = "true"
genLitCode (Bool False) = "false"
genLitCode (Str s) = [i|"#{s}"|]
genLitCode (Unit) = "undefined"
