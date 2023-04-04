module Anagram(anagramsFor)
where

import Data.List(sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor s ss = filter (isAnagram s) ss


isAnagram :: String -> String -> Bool
isAnagram str1 str2 = sort str1 == sort str2 