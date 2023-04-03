module Grains (square, total, grainsOnBoard)
where
import Data.Maybe (mapMaybe)


square :: Integer -> Maybe Integer
square n 
    | n < 1 || n > 64 = Nothing
    | otherwise = Just (2 ^ (n-1))


total :: Integer
total = sum (mapMaybe square [1..64])

grainsOnBoard :: Maybe Integer
grainsOnBoard = foldl accum (Just 0) [1..64]
    where
        accum :: Maybe Integer -> Int -> Maybe Integer
        accum acc n = 
            case acc of
                Nothing -> Nothing
                Just total -> 
                    case square (fromIntegral n) of
                        Nothing -> Nothing
                        Just grains -> Just (total + grains)