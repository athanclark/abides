module Test.Abides.Control.Comonad where

import Control.Comonad (Comonad (extend, extract), (<<=))


leftIdentity :: Comonad w => Eq (w a) => w a -> Bool
leftIdentity x = (extract <<= x) == x


rightIdentity :: Comonad w => Eq a => (w a -> a) -> w a -> Bool
rightIdentity f x = extract (f <<= x) == f x


associative :: Comonad w => Eq (w c) => (w b -> c) -> (w a -> b) -> w a -> Bool
associative f g x = extend f (extend g x) == extend (f . extend g) x
