module Data.Tree.DUAL.Test where

import           Prelude --(class Semigroup, unit, ($), (<$>), (==), append)
import           Data.Monoid.Action (class Action)
--import           Data.Monoid.Additive (Additive(..))
--import           Data.Monoid.Multiplicative (Multiplicative(..))
import           Data.Maybe(Maybe(..), fromMaybe)
import           Data.Newtype (class Newtype, unwrap, wrap)
import           Data.List.Lazy (replicate)
import           Data.List.NonEmpty (NonEmptyList(..), singleton, fromList)
import           Data.NonEmpty(NonEmpty(..), (:|), foldMap1)
import           Data.Tuple (Tuple(..))
import           Data.Semiring(class Semiring)
import           Control.Apply (lift2)
import           Data.Tree.DUAL.Internal (empty, leaf, leafU, getU, DUALTree(..), DUALTreeU(..), DUALTreeNE(..), applyUpre, applyD)
import           Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)
import           Test.QuickCheck.Gen (Gen, chooseInt, oneOf, listOf, sized, uniform)
import           Test.QuickCheck (quickCheck)

data U = Additive Int
derive instance eqU :: Eq (U)

instance semigroupU :: Semigroup (U) where
  append (Additive a) (Additive b) = Additive (a + b)

instance actionU :: Action U Int where
  act (Additive i) s = i + s

newtype D = Multiplicative Int

instance actionD :: Action D Int where
  act (Multiplicative i) s = i * s

instance actionDU :: Action D U where
  act (Multiplicative i) (Additive j) = Additive (i * j)

-- FIXME: Why is this necessary? `D` is a synonmym of `Multiplicative` which is a `Semigroup`
instance semigroupD :: Semigroup D where
  append (Multiplicative x) (Multiplicative y) = Multiplicative (x*y)

data DUALTreeExpr d u a l =
    EEmpty
  | ELeaf u l
  | ELeafU u
  | EConcat (NonEmptyList (DUALTreeExpr d u a l))
  | EAct d (DUALTreeExpr d u a l)
--  | EAnnot a (DUALTreeExpr d u a l)
--  deriving (Show, Typeable)

mkU :: Gen U
mkU = Additive <$> (arbitrary :: Gen Int)

instance arbitraryU :: Arbitrary U where
  arbitrary = mkU

mkD :: Gen D
mkD = Multiplicative <$> (arbitrary :: Gen Int)

instance arbitraryD :: Arbitrary D where
  arbitrary = mkD

type T = DUALTree D U Boolean Boolean

-- A Wrapper for type T so we can make arbitrary instances of it
newtype DT = DT T

mkLeaf :: forall d u a l. Gen u -> Gen l -> Gen (DUALTreeExpr d u a l)
mkLeaf genU genL = oneOf $ NonEmpty l [l, lu]
  where
    l = lift2 ELeaf genU genL
    lu = ELeafU <$> genU

mkLeafT :: forall d a. Gen (DUALTreeExpr d U a Boolean)
mkLeafT = mkLeaf mkU (arbitrary :: Gen Boolean)

mkConcatExpr :: forall d a. Int ->  Gen (DUALTreeExpr d U a Boolean)
mkConcatExpr len = do
                     ls <- fromList <$> listOf len (mkTreeExpr (len-1))
                     case ls of
                       (Just xs) -> pure $ EConcat xs
                       Nothing -> mkLeafT -- Got to make something sane here

mkActExpr :: forall a. Gen (DUALTreeExpr D U a Boolean)
mkActExpr = EAct <$> mkD <*> (mkTreeExpr 0) -- FIXME: Go deeper

mkTreeExpr :: forall d a. Int -> Gen (DUALTreeExpr d U a Boolean)
mkTreeExpr 0 = mkLeafT
mkTreeExpr n = do
                 len <- chooseInt 1 n
                 mkConcatExpr len

buildTree :: forall d u a l. Semigroup d => Semigroup u => Action d u => DUALTreeExpr d u a l -> DUALTree d u a l
buildTree EEmpty       = empty
buildTree (ELeaf u l)  = leaf u l
buildTree (ELeafU u)   = leafU u
buildTree (EConcat (NonEmptyList ts)) =  foldMap1 buildTree ts
buildTree (EAct d t)   = applyD d (buildTree t)
--buildTree (EAnnot a t) = annot a (buildTree t)

{-
 -- FIXME: I'd like to be able to use sized

instance arbDT :: Arbitrary DT where
  arbitrary = DT $ buildTree <$> sized uniform

-}

instance arbitraryDT :: Arbitrary DT where
  arbitrary = do
                i <- chooseInt 0 4 -- manually control bounds rather than use sized
                te <- mkTreeExpr i
                pure $ DT (buildTree te)

prop_leaf_u :: Int -> Boolean
prop_leaf_u u = getU (leaf u unit) == Just u

prop_leafU_u :: U -> Boolean
prop_leafU_u u = getU (leafU u) == Just u

prop_applyUpre :: U -> DT -> Boolean
prop_applyUpre u (DT t) = getU (applyUpre u t) == Just (u `append` fromMaybe u (getU t))

--prop_applyUpost :: U -> T -> Boolean
--prop_applyUpost u t = getU (applyUpost u t) == Just (fromMaybe mempty (getU t) `append` u)


main = do
  quickCheck prop_leaf_u
  quickCheck prop_leafU_u
  quickCheck prop_applyUpre