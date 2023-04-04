module Anagram(anagramsFor)
where

import Data.List(sort)
import Data.Char(toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor s ss = filter (isAnagram s) ss


isAnagram :: String -> String -> Bool
isAnagram str1 str2 = lStr1 /= lStr2 && sort lStr1 == sort lStr2 
    where
        lStr1 = lowerString str1
        lStr2 = lowerString str2

lowerString :: String -> String
lowerString s = map toLower s