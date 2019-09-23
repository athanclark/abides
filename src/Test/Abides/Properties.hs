module Test.Abides.Properties where


-- | x == x?
reflexive :: Eq a => a -> Bool
reflexive x = x == x

-- | f x y == f y x?
commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative f x y = f x y == f y x

-- | f (f x y) z == f x (f y z)?
associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = f (f x y) z == f x (f y z)

-- | f (f x) == f x?
idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f x = f (f x) == f x

-- | f (g x y) == g (f x) (f y)?
distributive :: Eq a => (a -> a) -> (a -> a -> a) -> a -> a -> Bool
distributive f g x y = f (g x y) == g (f x) (f y)

-- | f (g x y) == g' (f x) (f y)?
distributive' :: Eq b => (a -> b) -> (a -> a -> a) -> (b -> b -> b) -> a -> a -> Bool
distributive' f g g' x y = f (g x y) == g' (f x) (f y)

-- | f x y == x? Note: bottom ~ forall y. f bottom y == bottom, while unit ~ forall x. f x unit == x
constL :: Eq a => (a -> a -> a) -> a -> a -> Bool
constL f x y = f x y == x

-- | f x y == y?
constR :: Eq a => (a -> a -> a) -> a -> a -> Bool
constR f x y = f x y == y

