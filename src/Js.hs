{-# LANGUAGE OverloadedStrings #-}
module Js
    ( preJs
    ) where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )

preJs :: Text
preJs = T.concat [header, jsprint, jsprintln, ending]

header :: Text
header = "/* Primitive start */\n"

ending :: Text
ending = "\n/* Primitive end */\n"

jsprint :: Text
jsprint = "function print(x) { process.stdout.write(x) }"

jsprintln :: Text
jsprintln = "function println(x) { process.stdout.write(x + \"\\n\") }"
