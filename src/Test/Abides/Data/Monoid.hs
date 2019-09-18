module Test.Abides.Data.Monoid where

import Test.Abides.Properties (constL, constR)


leftIdentity :: Monoid a => Eq a => a -> Bool
leftIdentity = constR (<>) mempty


rightIdentity :: Monoid a => Eq a => a -> Bool
rightIdentity x = constL (<>) x mempty
