{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
module Js
    ( preJs
    ) where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Text.InterpolatedString.Perl6  ( q )

preJs :: Text
preJs = T.unlines
    [ header
    , jsprint
    , jsprintln
    , jsToInt
    , jsReadLine
    , jsToString
    , jsError
    , ending
    ]

header :: Text
header = "/* Primitive start */\n"

ending :: Text
ending = "\n/* Primitive end */\n"

jsprint :: Text
jsprint = "function print(x) { process.stdout.write(x) }"

jsprintln :: Text
jsprintln = "function println(x) { process.stdout.write(x + \"\\n\") }"

jsError :: Text
jsError = "function error(x) { println(x); process.exit(1) }"

jsToInt :: Text
jsToInt = [q|function toInt(x) {
    var res = parseInt(x);
      if (isNaN(res)) {
        error("Error: Could not parse int");
      } else {
        return res;
      }
}|]

jsReadLine :: Text
jsReadLine = [q|const deasync = require('deasync');
const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});
let inputLines = [];
rl.on('line', function(answer) {
  inputLines.push(answer);
});
function readLine() {
  deasync.loopWhile(function(){return inputLines.length <= 0;});
  return inputLines.shift();
}|]

jsToString :: Text
jsToString = [q|function toString(x) {
    if (typeof x === 'number') {
        return x.toString()
    } else if (typeof x === 'boolean') {
        return x ? "true" : "false"
    } else if (typeof x === 'string') {
        return x
    } else if (typeof x === 'object') {
        var acc = []; var ty = ""; var cs = "";
        for (const [key, value] of Object.entries(x)) {
            if (key == "type") {
                ty = value; continue;
            }
            if (key == "constr") {
                cs = value; continue;
            }
            acc.push(`${toString(value)}`)
        }
        return ty + "." + cs + "(" + acc.join(", ") + ")"
    } else {
        return "()"
    }
}|]
