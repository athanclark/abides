module Test.Abides.Data.EuclideanRing where


integralDomain :: Num a => Eq a => a -> a -> Bool
integralDomain x y = if x /= 0 && y /= 0 then x * y /= 0 else True


-- nonnegative :: Num a => Eq a => a -> Bool
-- nonnegative x = if x /= 0 then degree x >= 0 else True


-- quotientRemainder :: Num a => Eq a => a -> a -> Bool
-- quotientRemainder x y =
--   if y /= 0
--     then
--       let q = x / y
--           r = x `mod` y
--       in  (x == q * y + r) && ((r == 0) || (degree r < degree y))
--     else True


-- submultiplicative :: Num a => Eq a => a -> a -> Bool
-- submultiplicative x y =
--   if x /= 0 && y /= 0
--     then degree x <= degree (x * y)
--     else True
