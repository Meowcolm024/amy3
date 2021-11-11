import Parser

main :: IO ()
main = print $ regularParse expr
    "a match { case Maybe.Just[Int](3) => 4 case a => 0 case Maybe.Nothing() => false}"
