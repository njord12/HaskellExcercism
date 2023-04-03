module Acronyms(abbreviate) where

import Data.Char(toUpper, isUpper)
import Data.List.Split (split, keepDelimsL, whenElt, condense)

abbreviate :: String -> String
abbreviate xs = getFirstLetters $ unwords $ splitCamelCase $ map charToWhitespace xs

splitCamelCase :: String -> [String]
splitCamelCase = split (condense . keepDelimsL $ whenElt isUpper)

getFirstLetters :: String -> String
getFirstLetters xs = map (toUpper . head) (words xs)

charToWhitespace :: Char -> Char
charToWhitespace x =
    case x of
        '-' -> ' '
        '_' -> ' '
        _ -> x