module Test.Abides.Data.Bounded where

-- | minBound <= x <= maxBound?
bounded :: Bounded a => Ord a => a -> Bool
bounded x = minBound <= x && x <= maxBound
