module Test.Main where

import           Prelude (class Semigroup, unit, ($), (<$>), (==))
import           Data.Monoid.Action
import           Data.Maybe(Maybe(..))
import           Data.Array(singleton)
import           Data.NonEmpty((:|))
import           Control.Apply (lift2)
import           Data.Tree.DUAL.Internal (empty, leaf, leafU, getU, DUALTree)
import           Test.QuickCheck.Arbitrary (arbitrary)
import           Test.QuickCheck.Gen (Gen,  oneOf)
import           Test.QuickCheck (quickCheck)

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

mkD :: Gen D
mkD = Product <$> (arbitrary :: Gen Int)

{-[Sum <$> (arbitrary :: Gen Int)
                      , ]-}

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


main = do
  quickCheck prop_leaf_u