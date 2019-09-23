module Test.Abides.Data.Ring where


additiveInverse :: Num a => Eq a => a -> Bool
additiveInverse x = a && b
  where
    a = (x - x) == (x + negate x)
    b = (negate x + x) == 0
