module AnagramSpec
where

import SpecHelper


spec :: Spec
spec = 
    describe "Anagram" $ do
        it "No matches" $
            anagramsFor "diaper" ["hello", "world", "zombies", "pants"] `shouldMatchList` []
    
        it "detects two anagrams" $
            anagramsFor "master" ["stream", "pigeon", "maters"] `shouldMatchList` ["stream", "maters"]

        it "does not detect anagram subsets" $
            anagramsFor "good" ["dog", "goody"] `shouldMatchList` []

        it "detects anagram" $
            anagramsFor "listen" ["enlists", "google", "inlets", "banana"] `shouldMatchList` ["inlets"]

        it "detects three anagrams" $
            anagramsFor "allergy" ["gallery", "ballerina", "regally", "clergy", "largely", "leading"] `shouldMatchList` ["gallery", "regally", "largely"]

        it "does not detect non-anagrams with identical checksum" $
            anagramsFor "mass" ["last"] `shouldMatchList` []
        
        it "detects anagrams using case-insensitive subjetc" $
            anagramsFor "Orchestra" ["cashregister", "Carthorse", "radishes"] `shouldMatchList` ["Carthorse"]

        it "does not detect a anagram if the original word is repeated" $
            anagramsFor "go" ["go Go GO"] `shouldMatchList` []

        it "anagrams must use all letters exactly once" $
            anagramsFor "tapper" ["patter"] `shouldMatchList` []
        
        it "words are not anagrams of themselves (case insensitive)" $
            anagramsFor "BANANA" ["BANANA", "banana", "Banana"] `shouldMatchList` []
