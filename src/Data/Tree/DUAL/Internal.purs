module Data.Tree.DUAL.Internal
       (
           -- * DUAL-trees
           DUALTreeNE(..), DUALTreeU(..), DUALTree(..)

           -- * Constructing DUAL-trees
           , empty, leaf, leafU --, annot, applyD

           -- * Accessors and eliminators
           , nonEmpty, getU, applyUpre
       )
where

import Prelude

import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..), fromList, singleton)
import Data.Tuple (Tuple(..), fst)
import Data.Maybe (Maybe(..))
import Data.Monoid.Action (class Action, act)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (fold1, foldMap1, (:|))
import Data.Functor (class Functor, map)
import Data.Semigroup.Foldable (class Foldable1, foldMap1Default)

-- | /Non-empty/ DUAL-trees.
data DUALTreeNE d u a l
  = Leaf   u l        -- ^ Leaf with data value and @u@ annotation
  | LeafU  u          -- ^ Leaf with only @u@ annotation
  | Concat (NonEmptyList (DUALTreeU d u a l))
                      -- ^ n-way branch, containing a /non-empty/ list
                      --   of subtrees.
  | Act    d (DUALTreeU d u a l)
                      -- ^ @d@ annotation
  | Annot  a (DUALTreeU d u a l)
                      -- ^ Internal data value

derive instance ftorDUALTreeNE :: Functor (DUALTreeNE d u a)
derive instance eqDUALTreeNE :: (Eq d, Eq u, Eq a, Eq l) => Eq (DUALTreeNE d u a l)

instance semigroupDUALTreeNE :: (Action d u, Semigroup u) => Semigroup (DUALTreeNE d u a l) where
  append t1 t2   = fold1 (t1 :| (singleton t2))

--sconcat = Concat . map pullU

--instance foldableDUALTreeNE :: Foldable1 (DUALTreeNE d u a l) where
--  fold1 = Concat . map pullU
--  foldMap1 = foldMap1Default

-- | A non-empty DUAL-tree paired with a cached @u@ value.  These
--   should never be constructed directly; instead, use 'pullU'.
newtype DUALTreeU d u a l = DUALTreeU { unDUALTreeU :: Tuple u  (DUALTreeNE d u a l) }

derive instance ftorDUALTreeU :: Functor (DUALTreeU d u a)
derive instance eqDUALTreeU :: (Eq d, Eq u, Eq a, Eq l) => Eq (DUALTreeU d u a l)
instance ntypeDUALTreeU :: Newtype (DUALTreeU d u a l) (Tuple u  (DUALTreeNE d u a l)) where
  unwrap (DUALTreeU r) = r.unDUALTreeU
  wrap t = DUALTreeU {unDUALTreeU : t}

instance semigroupDUALTreeU :: Semigroup (DUALTreeU d u a l) where
  append t1 t2   = fold1 (t1 :| (singleton t2))

newtype DUALTree d u a l = DUALTree { unDUALTree :: Maybe (DUALTreeU d u a l) }

derive instance ftorDUALTree :: Functor (DUALTree d u a)
derive instance eqDUALTree :: (Eq d, Eq u, Eq a, Eq l) => Eq (DUALTree d u a l)
instance ntypeDUALTree :: Newtype (DUALTree d u a l) (Maybe (DUALTreeU d u a l)) where
  unwrap (DUALTree r) = r.unDUALTree
  wrap t = DUALTree {unDUALTree : t}


instance semigroupDUALTree :: Semigroup (DUALTree d u a l) where
  append t1 t2   = fold1 (t1 :| (singleton t2))

--  deriving ( Functor, Semigroup, Typeable, Show, Eq )

-- | The empty DUAL-tree.  This is a synonym for 'mempty', but with a
--   more general type.
empty :: forall d u a l. DUALTree d u a l
empty = DUALTree {unDUALTree: Nothing}

-- | Construct a leaf node from a @u@ annotation along with a leaf
--   datum.
leaf :: forall d u a l. u -> l -> DUALTree d u a l
leaf u l = DUALTree {unDUALTree: (Just (DUALTreeU {unDUALTreeU: Tuple u (Leaf u l)}))}


-- | Construct a leaf node from a @u@ annotation.
leafU :: forall d u a l. u -> DUALTree d u a l
leafU u = DUALTree {unDUALTree: dtu}
  where
    dtu = Just (DUALTreeU {unDUALTreeU: Tuple u (LeafU u)})


-- | Add a @u@ annotation to the root, combining it (on the left) with
--   the existing cached @u@ annotation.  This function is provided
--   just for convenience; @applyUpre u t = 'leafU' u \<\> t@.
applyUpre :: forall d u a l. Semigroup u => Action d u => u -> DUALTree d u a l -> DUALTree d u a l
applyUpre u t = append (leafU u) t


-- | Decompose a DUAL-tree into either @Nothing@ (if empty) or a
--   top-level cached @u@ annotation paired with a non-empty
--   DUAL-tree.
nonEmpty :: forall d u a l. (DUALTree d u a l) -> Maybe (Tuple u (DUALTreeNE d u a l))
nonEmpty t = map unwrap $ unwrap t

{-nonEmpty (DUALTree t) = case t.unDUALTree of
                          Nothing -> Nothing
                          Just (DUALTreeU r) -> Just r.unDUALTreeU -}

-- | Get the @u@ annotation at the root, or @Nothing@ if the tree is
--   empty.
getU :: forall d u a l. DUALTree d u a l -> Maybe u
getU t = map fst $ nonEmpty t

-- | \"Pull\" the root @u@ annotation out into a tuple.
pullU :: forall d u a l. Semigroup u => Action d u => DUALTreeNE d u a l -> DUALTreeU d u a l
pullU (Leaf u l) = wrap $ Tuple u  (Leaf u l)
pullU (LeafU u)  = wrap $ Tuple u  (LeafU u)
pullU (Concat (NonEmptyList ts))= wrap $ Tuple u' (Concat (NonEmptyList ts))
                     where
                       u' = foldMap1 (fst <<< unwrap) ts
pullU (Act d dt)   = wrap $ Tuple (act d u) (Act d dt)
                                             where
                                               u = fst $ unwrap dt
pullU (Annot a dt) = wrap $ Tuple  u (Annot a dt)
                                             where
                                               u = fst $ unwrap dt
