module Test.Abides.Data.Enum where


-- | compare x y == compare (fromEnum x) (fromEnum y)
compareHom :: Enum a => Ord a => a -> a -> Bool
compareHom x y = compare x y == compare (fromEnum x) (fromEnum y)


-- | pred (succ x) == x
predsucc :: Enum a => Eq a => a -> Bool
predsucc x = pred (succ x) == x


-- | succ (pred x) == x
succpred :: Enum a => Eq a => a -> Bool
succpred x = succ (pred x) == x
