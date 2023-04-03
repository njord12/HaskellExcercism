module GrainsSpec
where


import SpecHelper
import Data.Foldable ( for_ )


main :: IO()
main = hspecWith defaultConfig {configFailFast=True} spec

spec :: Spec
spec = do
    describe "square" $ for_ squareCases squareTest
    describe "total" $ totalTest totalCase

    where
        squareTest (description, n, expected) = it description assertion
            where
                assertion = expression `shouldBe` expected
                expression = fmap fromIntegral . square . fromIntegral $ n
        
        totalTest (description, expected) = it description assertion
            where
                assertion = fromIntegral total `shouldBe` expected

squareCases :: [(String, Integer, Maybe Integer)]
squareCases = 
    [
        ("Square 1", 1, Just 1),
        ("Square 2", 2, Just 2),
        ("Square 3", 3, Just 4),
        ("Square 4", 4, Just 8),
        ("Square 16", 16, Just 32768),
        ("square 32", 32, Just 2147483648),
        ("square 64", 64, Just 9223372036854775808),
        ("square negative", -1, Nothing),
        ("square 0", 0, Nothing),
        ("square 65", 65, Nothing)
    ]

totalCase :: (String, Integer)
totalCase = ("total grains", 18446744073709551615)