{-# LANGUAGE NoImplicitPrelude #-}

module Test.Abides.Control.Category where

import Prelude hiding ((.), id)
import Control.Category (Category ((.), id))


identity :: Category c => Eq (c a b) => c a b -> Bool
identity p = ((id . p) == p) && ((p . id) == p)


associative :: Category q => Eq (q a d) => q c d -> q b c -> q a b -> Bool
associative x y z = (x . (y . z)) == ((x . y) . z)
