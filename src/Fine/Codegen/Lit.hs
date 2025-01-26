module Fine.Codegen.Lit (genLitCode) where

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Syntax.Common (Lit (..))

genLitCode :: Lit -> Text
genLitCode (Int v) = T.pack $ show v
genLitCode (Float v) = T.pack $ show v
genLitCode (Bool True) = "true"
genLitCode (Bool False) = "false"
genLitCode (Str s) = [i|"#{s}"|]
genLitCode (Unit) = "undefined"
