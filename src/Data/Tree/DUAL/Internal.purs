module Data.Tree.DUAL.Internal
       (
           -- * DUAL-trees
           DUALTreeNE(..), DUALTreeU(..), DUALTree(..)

           -- * Constructing DUAL-trees
           , empty, leaf, leafU, annot, applyD

           -- * Accessors and eliminators
           , nonEmpty, getU, applyUpre, applyUpost

           -- * Accessors and eliminators
           , flatten
       )
where

import Prelude

import Data.List (List(..), concat)
import Data.List.NonEmpty (NonEmptyList(..), fromList, toList, singleton)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Action (class Action, act)
import Data.Newtype (class Newtype, unwrap, wrap, over)
import Data.NonEmpty (fold1, foldMap1, (:|))
import Data.Functor (class Functor, map)
import Data.Semigroup.Foldable (class Foldable1, foldMap1Default)
import Data.Profunctor.Strong ((***))

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

newtype DAct d = DAct { unDAct :: d }
instance ntypeDAct :: Newtype (DAct d) d where
  unwrap (DAct r) = r.unDAct
  wrap d = DAct { unDAct : d }

instance actionDAct :: (Semigroup d, Semigroup u, Action d u) => Action (DAct d) (DUALTreeNE d u a l) where
  act (DAct d) (Act d' t) = Act (d.unDAct <> d') t
  act (DAct d) t          = Act d.unDAct (pullU t)

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

instance actionDActDUALTreeU :: (Semigroup d, Semigroup u, Action d u) => Action (DAct d) (DUALTreeU d u a l) where
  act d = over wrap (act (unwrap d) *** act d)

newtype DUALTree d u a l = DUALTree { unDUALTree :: Maybe (DUALTreeU d u a l) }

derive instance ftorDUALTree :: Functor (DUALTree d u a)
derive instance eqDUALTree :: (Eq d, Eq u, Eq a, Eq l) => Eq (DUALTree d u a l)
instance ntypeDUALTree :: Newtype (DUALTree d u a l) (Maybe (DUALTreeU d u a l)) where
  unwrap (DUALTree r) = r.unDUALTree
  wrap t = DUALTree {unDUALTree : t}


instance semigroupDUALTree :: Semigroup (DUALTree d u a l) where
  append t1 t2   = fold1 (t1 :| (singleton t2))

instance monoidDUALTree :: Monoid (DUALTree d u a l) where
  mempty = DUALTree {unDUALTree : mempty}

-- | Apply a @d@ annotation at the root of a tree.  Semantically, all
--   @u@ annotations are transformed by the action of @d@, although
--   operationally @act@ incurs only a constant amount of work.
instance actionDActDUALTree :: (Semigroup d, Semigroup u, Action d u)
    => Action (DAct d) (DUALTree d u a l) where
  act = over wrap <<< map <<< act

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

-- | Add a @u@ annotation to the root, combining it (on the right) with
--   the existing cached @u@ annotation.  This function is provided
--   just for convenience; @applyUpost u t = t \<\> 'leafU' u@.
applyUpost :: forall d u a l. Semigroup u => Action d u => u -> DUALTree d u a l -> DUALTree d u a l
applyUpost u t = t <> leafU u

-- | Apply a @d@ annotation at the root of a tree, transforming all
--   @u@ annotations by the action of @d@.
applyD :: forall d u a l. Semigroup d => Semigroup u => Action d u => d -> DUALTree d u a l -> DUALTree d u a l
applyD d = act $ DAct { unDAct : d }

-- | Decompose a DUAL-tree into either @Nothing@ (if empty) or a
--   top-level cached @u@ annotation paired with a non-empty
--   DUAL-tree.
nonEmpty :: forall d u a l. (DUALTree d u a l) -> Maybe (Tuple u (DUALTreeNE d u a l))
nonEmpty t = map unwrap $ unwrap t

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

-- | Add an internal data value at the root of a tree.  Note that this
--   only works on /non-empty/ trees; on empty trees this function is
--   the identity.
annot :: forall d u a l. Semigroup u => Action d u => a -> DUALTree d u a l -> DUALTree d u a l
annot a = (over wrap <<< map) (pullU <<< Annot a)

------------------------------------------------------------
-- Folds
------------------------------------------------------------
-- | Fold for non-empty DUAL-trees.
foldDUALNE :: forall d u a l r. Monoid d
           => (d -> l -> r) -- ^ Process a leaf datum along with the
                            --   accumulation of @d@ values along the
                            --   path from the root
           -> r             -- ^ Replace @LeafU@ nodes
           -> (NonEmptyList r -> r)  -- ^ Combine results at a branch node
           -> (d -> r -> r)      -- ^ Process an internal d node
           -> (a -> r -> r)      -- ^ Process an internal datum
           -> DUALTreeNE d u a l -> r
foldDUALNE  = foldDUALNE' Nothing
  where
    foldDUALNE' dacc lf _   _   _    _   (Leaf _ l)  = lf (maybe mempty id dacc) l
    foldDUALNE' _    _  lfU _   _    _   (LeafU _)   = lfU
    foldDUALNE' dacc lf lfU con down ann (Concat ts)
      = con (map (foldDUALNE' dacc lf lfU con down ann <<< snd <<< unwrap) ts)
    foldDUALNE' dacc lf lfU con down ann (Act d t)
      = down d (foldDUALNE' (dacc <> (Just d)) lf lfU con down ann <<< snd <<< unwrap $ t)
    foldDUALNE' dacc lf lfU con down ann (Annot a t)
      = ann a (foldDUALNE' dacc lf lfU con down ann <<< snd <<< unwrap $ t)

-- | Fold for DUAL-trees. It is given access to the internal and leaf
--   data, internal @d@ values, and the accumulated @d@ values at each
--   leaf.  It is also allowed to replace \"@u@-only\" leaves with a
--   constant value.  In particular, however, it is /not/ given access
--   to any of the @u@ annotations, the idea being that those are used
--   only for /constructing/ trees.  If you do need access to @u@
--   values, you can duplicate the values you need in the internal
--   data nodes.
--
--   Be careful not to mix up the @d@ values at internal nodes with
--   the @d@ values at leaves.  Each @d@ value at a leaf satisfies the
--   property that it is the 'mconcat' of all internal @d@ values
--   along the path from the root to the leaf.
--
--   The result is @Nothing@ if and only if the tree is empty.

foldDUAL :: forall d u a l r. Monoid d
         => (d -> l -> r)          -- ^ Process a leaf datum along with the
                                   --   accumulation of @d@ values along the
                                   --   path from the root
         -> r                      -- ^ Replace @u@-only nodes
         -> (NonEmptyList r -> r)      -- ^ Combine results at a branch node
         -> (d -> r -> r)          -- ^ Process an internal d node
         -> (a -> r -> r)          -- ^ Process an internal datum
         -> DUALTree d u a l -> Maybe r
foldDUAL _ _ _ _ _ (DUALTree { unDUALTree : Nothing })
  = Nothing
foldDUAL l u c d a (DUALTree { unDUALTree : Just p })
  = Just $ foldDUALNE l u c d a ( snd <<< unwrap $ p)

-- | A specialized fold provided for convenience: flatten a tree into
--   a list of leaves along with their @d@ annotations, ignoring
--   internal data values.
flatten :: forall d u a l. Monoid d => DUALTree d u a l -> List (Tuple l d)
flatten = fromMaybe Nil
        <<< foldDUAL
            (\d l -> Cons (Tuple l d) (Nil)) -- Fixme: can we avoid concating these singleton lists?
            Nil
            (concat <<< toList)
            (flip const)
            (const id)
