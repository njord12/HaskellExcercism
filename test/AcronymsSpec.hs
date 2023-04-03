module AcronymsSpec where

import SpecHelper

spec :: Spec
spec = 
    describe "abbreviate" $ do
        context "basic string" $
            it "Should be PNG" $
                abbreviate "Portable Network Graphics" `shouldBe` "PNG"
        
        context "camelcase" $
            it "should be HTML" $
                abbreviate "HyperText Markup Language" `shouldBe` "HTML"
        
        context "all caps words" $
            it "should be GIMP" $
                abbreviate "GNU Image Manipulation Program" `shouldBe` "GIMP"
        
        context "punctuation without whitespace" $
            it "should be CMOS" $
                abbreviate "Complementary metal-oxide semiconductor" `shouldBe` "CMOS"

        context "very long abbreviation" $
            it "should be ROTFLSHTMDCOALM" $
                abbreviate "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me" `shouldBe` "ROTFLSHTMDCOALM"

        context "underscore for emphasis" $
            it "should be TRNT" $
                abbreviate "The Road _Not_ Taken" `shouldBe` "TRNT"

main :: IO ()
main = hspec spec