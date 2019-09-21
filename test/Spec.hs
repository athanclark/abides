import qualified Test.Abides.Control.Alternative as Alt
import qualified Test.Abides.Control.Applicative as App
import qualified Test.Abides.Control.Category as Cat
import qualified Test.Abides.Control.Comonad as Com
import qualified Test.Abides.Control.Monad as Mon
import qualified Test.Abides.Data.Bounded as Bou
import qualified Test.Abides.Data.Enum as Enu
import qualified Test.Abides.Data.Eq as Eq
import qualified Test.Abides.Data.Foldable as Fol
import qualified Test.Abides.Data.Functor as Fun
import qualified Test.Abides.Data.Monoid as Mono
import qualified Test.Abides.Data.Ord as Ord
import qualified Test.Abides.Data.Semigroup as Sem

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Monoid (Sum (..))



main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Test.Abides.Control"
    [ testProperty "Alternative" alternativeTests
    , testProperty "Applicative" applicativeTests
    -- , testProperty "Category" categoryTests
    , testProperty "Comonad" comonadTests
    , testProperty "Monad" monadTests
    , testProperty "MonadPlus" monadPlusTests
    ]
  , testGroup "Test.Abides.Data"
    [ testProperty "Bounded" boundedTests
    , testProperty "Enum" enumTests
    , testProperty "Eq" eqTests
    , testProperty "Foldable" foldableTests
    , testProperty "Functor" functorTests
    , testProperty "Monoid" monoidTests
    , testProperty "Ord" ordTests
    , testProperty "Semigroup" semigroupTests
    ]
  ]
  where
    alternativeTests :: [Int] -> Bool
    alternativeTests x = and
      [ Alt.distributive [(+ 1)] [(* 2)] x
      , Alt.annihilation (map (+) x)
      ]
    applicativeTests :: [Int] -> Bool
    applicativeTests x = and
      [ App.identity x
      , App.composition [(+ 1)] [(* 2)] x
      ]
    -- categoryTests :: Kleisli [] Int Int -> Bool
    -- categoryTests x = and
    --   [ Cat.identity x
    --   , App.composition [(+ 1)] [(* 2)] x
    --   ]
    comonadTests :: (Int,Int) -> Bool
    comonadTests x = and
      [ Com.leftIdentity x
      , Com.rightIdentity (\(a,b) -> b + 1) x
      , Com.associative (\(a,b) -> b * 2) (\(a,b) -> b + 1) x
      ]
    monadTests :: [Int] -> Bool
    monadTests x = and
      [ Mon.leftIdentity (\a -> [a] ++ x) 5
      , Mon.rightIdentity x
      , Mon.associative (\a -> [a,1]) (\a -> [2,a]) x
      ]
    monadPlusTests :: Maybe Int -> Bool
    monadPlusTests x = and
      [ Mon.annihilation (\a -> Just (a + 1))
      , Mon.distributive (\a -> Just (a + 1)) x (Just 5)
      ]
    boundedTests :: Int -> Bool
    boundedTests x = Bou.bounded x
    enumTests :: Int -> Bool
    enumTests x = and
      [ Enu.compareHom x 1
      , Enu.predsucc x
      , Enu.succpred x
      ]
    eqTests :: (Int,Int) -> Bool
    eqTests (x,y) = and
      [ Eq.symmetry x y
      , Eq.transitive x y x
      , Eq.negation x y
      ]
    foldableTests :: [Sum Int] -> Bool
    foldableTests x = Fol.foldMap' (+ (Sum 5)) x
    functorTests :: [Int] -> Bool
    functorTests x = and
      [ Fun.identity x
      , Fun.composition (+ 5) (* 2) x
      ]
    monoidTests :: Sum Int -> Bool
    monoidTests x = and
      [ Mono.leftIdentity x
      , Mono.rightIdentity x
      ]
    ordTests :: (Int,Int) -> Bool
    ordTests (x,y) = and
      [ Ord.reflexive x
      , Ord.antisymmetry x y
      , Ord.transitive x y x
      ]
    semigroupTests :: (Sum Int, Sum Int, Sum Int) -> Bool
    semigroupTests (x,y,z) = Sem.associative x y z
