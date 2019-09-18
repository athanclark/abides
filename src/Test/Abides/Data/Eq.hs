module Test.Abides.Data.Eq
  ( P.reflexive
  , symmetry
  , transitive
  , negation
  ) where

import qualified Test.Abides.Properties as P


-- | x == y => y == x?
symmetry :: Eq a => a -> a -> Bool
symmetry x y = if x == y then y == x else True


-- | x == y && y == z => x == z
transitive :: Eq a => a -> a -> a -> Bool
transitive x y z = if x == y && y == z then x == z else True


-- | x /= y => not (x == y)
negation :: Eq a => a -> a -> Bool
negation x y = if x /= y then not (x == y) else True
