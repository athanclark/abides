module Test.Abides.Data.Functor where


-- | fmap id x == x?
identity :: Functor f => Eq (f a) => f a -> Bool
identity x = fmap id x == x

-- | fmap (f . g) x == fmap f (fmap g x)?
composition :: Functor f => Eq (f a) => (a -> a) -> (a -> a) -> f a -> Bool
composition f g x = fmap (f . g) x == fmap f (fmap g x)
