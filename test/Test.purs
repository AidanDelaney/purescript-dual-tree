module Data.Tree.DUAL.Test where

import           Prelude --(class Semigroup, unit, ($), (<$>), (==), append)
import           Data.Monoid.Action (class Action)
import           Data.Monoid.Additive
import           Data.Monoid.Multiplicative
import           Data.Maybe(Maybe(..), fromMaybe)
import           Data.Newtype (class Newtype, unwrap, wrap)
import           Data.Array(singleton)
import           Data.NonEmpty((:|))
import           Data.Tuple (Tuple(..))
import           Data.Semiring(class Semiring)
import           Control.Apply (lift2)
import           Data.Tree.DUAL.Internal (empty, leaf, leafU, getU, DUALTree(..), DUALTreeU(..), DUALTreeNE(..), applyUpre)
import           Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)
import           Test.QuickCheck.Gen (Gen,  oneOf)
import           Test.QuickCheck (quickCheck)

newtype U = Additive Int
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

newtype DT = DT (DUALTree D U Boolean Boolean)

data DUALTreeExpr d u a l =
    EEmpty
  | ELeaf u l
  | ELeafU u
--  | EConcat (NonEmpty (DUALTreeExpr d u a l))
--  | EAct d (DUALTreeExpr d u a l)
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

newtype PairUD = PairUD (Tuple U (DUALTreeNE D U Boolean Boolean))

unwrapPairUD :: PairUD -> DUALTreeU D U Boolean Boolean
unwrapPairUD (PairUD t) = wrap t

instance arbitraryDNE :: Arbitrary PairUD where
  arbitrary = PairUD <$> (Tuple <$> u <*> t)
              where
                u = mkU
                l = arbitrary :: Gen Boolean
                t = oneOf $ (LeafU <$> u)  :| singleton (Leaf <$> u <*> l)

newtype DTU = DTU (DUALTreeU D U Boolean Boolean)

instance arbitraryDTU :: Arbitrary DTU where
  arbitrary = (DTU <<< unwrapPairUD) <$> arbitrary :: Gen PairUD

-- This constant unwrapping sucks.  I don't want to bother making these types instance of
-- Newtype, but we can't just make an instance of DualTree here as we're not in the same module.
unwrapDTU :: forall d u a l. DTU -> Maybe (DUALTreeU D U Boolean Boolean)
unwrapDTU (DTU d) = Just d

instance arbitraryDT :: Arbitrary DT where
  arbitrary = DT <<< wrap <<< unwrapDTU <$> arbitrary :: Gen DTU

mkLeaf :: forall d a. Gen (DUALTreeExpr d U a Int)
mkLeaf = oneOf $ l :| singleton lu
  where
    l = (lift2 ELeaf mkU (arbitrary :: Gen Int))
    lu = (ELeafU <$> mkU)

mkTreeExpr :: forall d a. Int -> Gen (DUALTreeExpr d U a Int)
mkTreeExpr 0 = mkLeaf
mkTreeExpr n = mkLeaf

buildTree :: forall d u a l. Semigroup d => Semigroup u => DUALTreeExpr d U a l -> DUALTree d U a l
buildTree EEmpty       = empty
buildTree (ELeaf u l)  = leaf u l
buildTree (ELeafU u)   = leafU u
--buildTree (EConcat ts) = sconcat (NEL.map buildTree ts)
--buildTree (EAct d t)   = applyD d (buildTree t)
--buildTree (EAnnot a t) = annot a (buildTree t)


--instance arbitraryTree :: Arbitrary T where
--  arbitrary = map buildTree (mkTreeExpr 0)

prop_leaf_u :: Int -> Boolean
prop_leaf_u u = getU (leaf u unit) == Just u

prop_leafU_u :: U -> Boolean
prop_leafU_u u = getU (leafU u) == Just u

prop_applyUpre :: forall d a l. U -> DT -> Boolean
prop_applyUpre u (DT t) = getU (applyUpre u t) == Just u -- Just (u `append` fromMaybe empty (getU t))

--prop_applyUpost :: U -> T -> Boolean
--prop_applyUpost u t = getU (applyUpost u t) == Just (fromMaybe mempty (getU t) `append` u)


main = do
  quickCheck prop_leaf_u
  quickCheck prop_leafU_u
  quickCheck prop_applyUpre