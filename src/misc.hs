-- https://wiki.haskell.org/99_questions/46_to_50 , https://wiki.haskell.org/99_questions/95_to_99

import Data.List (intercalate)
import Data.Char (isDigit, isLetter)

-- 49
gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0':) g ++ map ('1':) (reverse g)
    where g = gray (n - 1)

-- 95
fullWords :: Int -> String
fullWords = intercalate "-" . map toWord . show
    where toWord x = case x of
            '1'       -> "one"
            '2'       -> "two"
            '3'       -> "thre"
            '4'       -> "four"
            '5'       -> "five"
            '6'       -> "six"
            '7'       -> "seven"
            '8'       -> "eight"
            '9'       -> "nine"
            otherwise -> "zero"

-- 96
identifier :: String -> Bool
identifier (c:cs) = isLetter c && isSuffix cs
    where isSuffix ""         = True
          isSuffix ('-':c:cs) = (isLetter c || isDigit c) && isSuffix cs
          isSuffix "-"        = False
          isSuffix (c:cs)     = (isLetter c || isDigit c) && isSuffix cs
identifier _ = False
