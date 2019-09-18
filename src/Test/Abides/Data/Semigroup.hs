module Test.Abides.Data.Semigroup where

import qualified Test.Abides.Properties as P


associative :: Semigroup a => Eq a => a -> a -> a -> Bool
associative = P.associative (<>)
