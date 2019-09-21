module Test.Abides.Control.Alternative where

import Control.Applicative (Alternative ((<|>)), empty)


-- | (f <|> g) <*> x == (f <*> x) <|> (g <|> x)
distributive :: Alternative f => Applicative f => Eq (f b) => f (a -> b) -> f (a -> b) -> f a -> Bool
distributive f g x = ((f <|> g) <*> x) == ((f <*> x) <|> (g <*> x))


-- | empty <*> x == empty
annihilation :: Alternative f => Applicative f => Eq (f b) => f (a -> b) -> Bool
annihilation f = (f <*> empty) == empty
