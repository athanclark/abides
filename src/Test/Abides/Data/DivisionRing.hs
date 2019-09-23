module Test.Abides.Data.DivisionRing where


inverse :: Fractional a => Eq a => a -> Bool
inverse x = if x == 0 then True else (x * recip x) == (recip x * x) && (x * recip x) == 1
