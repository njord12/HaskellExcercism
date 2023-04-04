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
