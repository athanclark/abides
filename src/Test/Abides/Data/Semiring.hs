module Test.Abides.Data.Semiring where

import qualified Test.Abides.Properties as P


commutativeMonoid :: Num a => Eq a => a -> a -> a -> Bool
commutativeMonoid x y z = a && b && c
  where
    a = P.associative (+) x y z
    b = (0 + x == x + 0) && (x + 0 == x)
    c = P.commutative (+) x y


monoid :: Num a => Eq a => a -> a -> a -> Bool
monoid x y z = a && b
  where
    a = P.associative (*) x y z
    b = (1 * x == x * 1) && (x * 1 == x)


leftDistributive :: Num a => Eq a => a -> a -> a -> Bool
leftDistributive x = P.distributive (x *) (+)

rightDistributive :: Num a => Eq a => a -> a -> a -> Bool
rightDistributive x = P.distributive (* x) (+)


annihilation :: Num a => Eq a => a -> Bool
annihilation x = (x * 0 == 0 * x) && (x * 0 == 0)
