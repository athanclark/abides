module Test.Abides.Data.CommutativeRing where

import qualified Test.Abides.Properties as P


commutative :: Num a => Eq a => a -> a -> Bool
commutative = P.commutative (*)
