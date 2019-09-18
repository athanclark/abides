module Test.Abides.Data.Ord where


-- | x <= x?
reflexive :: Ord a => a -> Bool
reflexive x = x <= x


-- | x <= y && y <= x => x == y
antisymmetry :: Ord a => a -> a -> Bool
antisymmetry x y = if x <= y && y <= x then x == y else True


-- | x <= y && y <= z => x <= z
transitive :: Ord a => a -> a -> a -> Bool
transitive x y z = if x <= y && y <= z then x <= z else True
