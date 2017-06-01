module Test.Main where

import           Prelude
import           Data.Monoid.Action
import           Data.Maybe(Maybe(..))
import           Data.NonEmpty ((:|), singleton)
import           Control.Applicative ((<$>))
import           Data.Generic.Rep (Sum, Product)
import           Data.Tree.DUAL.Internal (empty, leaf, leafU, getU, DUALTree)
import           Data.Unit
import           Data.Semigroup (class Semigroup)
import           Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import           Test.QuickCheck.Gen (Gen, vectorOf, randomSample', sized, uniform, oneOf)
import           Test.QuickCheck hiding ((===))

newtype U = Sum Int
newtype D = Product Int

data T = DUALTree D U Boolean Boolean

data DUALTreeExpr d u a l =
    EEmpty
  | ELeaf u l
  | ELeafU u
--  | EConcat (NonEmpty (DUALTreeExpr d u a l))
--  | EAct d (DUALTreeExpr d u a l)
--  | EAnnot a (DUALTreeExpr d u a l)
--  deriving (Show, Typeable)

mkU :: Gen U
mkU = Sum <$> (arbitrary :: Gen Int)

mkLeaf :: forall d a l. Gen (DUALTreeExpr d U a l)
mkLeaf = ELeafU <$> mkU

mkTreeExpr :: forall d u a l. Int -> Gen (DUALTreeExpr d U a l)
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


main = do
  quickCheck prop_leaf_u