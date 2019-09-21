{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Test.Abides.Control.Applicative where


identity :: Applicative f => Eq (f a) => f a -> Bool
identity x = (pure id <*> x) == x


composition :: Applicative f => Eq (f c) => f (b -> c) -> f (a -> b) -> f a -> Bool
composition f g h = (pure (.) <*> f <*> g <*> h) == (f <*> (g <*> h))


-- FIXME ambiguous type?
-- homomorphism :: forall f a b. Applicative f => Eq (f b) => (a -> b) -> a -> Bool
-- homomorphism f x = (f' <*> x') == y'
--   where
--     f' :: f (a -> b)
--     f' = pure f

--     x' :: f a
--     x' = pure x

--     y' :: f b
--     y' = pure (f x)
