module Test.Abides.Control.Alternative where

import Control.Applicative (Alternative ((<|>)), empty)
import qualified Test.Abides.Properties as P


-- | (f <|> g) <*> x == (f <*> x) <|> (g <|> x)
distributive :: Alternative f => Applicative f => Eq (f b) => f a -> f (a -> b) -> f (a -> b) -> Bool
distributive x = P.distributive' (<*> x) (<|>) (<|>)


-- | empty <*> x == empty
annihilation :: Alternative f => Applicative f => Eq (f b) => f (a -> b) -> Bool
annihilation f = (f <*> empty) == empty
