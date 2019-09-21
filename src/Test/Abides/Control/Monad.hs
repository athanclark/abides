module Test.Abides.Control.Monad where


import Control.Monad (MonadPlus (mzero, mplus))


leftIdentity :: Monad m => Eq (m b) => (a -> m b) -> a -> Bool
leftIdentity f x = (pure x >>= f) == f x


rightIdentity :: Monad m => Eq (m a) => m a -> Bool
rightIdentity x = (x >>= pure) == x


associative :: Monad m => Eq (m c) => (a -> m b) -> (b -> m c) -> m a -> Bool
associative f g x = ((x >>= f) >>= g) == (x >>= (\k -> f k >>= g))


annihilation :: MonadPlus m => Eq (m b) => (a -> m b) -> Bool
annihilation f = (mzero >>= f) == mzero


distributive :: MonadPlus m => Eq (m b) => (a -> m b) -> m a -> m a -> Bool
distributive f x y = (mplus x y >>= f) == mplus (x >>= f) (y >>= f)
