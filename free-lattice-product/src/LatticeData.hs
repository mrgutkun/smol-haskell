{-# LANGUAGE MultiWayIf #-}

module LatticeData where

import Data.Foldable (foldl')
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (All(..), Any(..))

data Node a
  = Top
  | Bottom

  | Join (Node a) (Node a) [Node a]
  | Meet (Node a) (Node a) [Node a]

  | Plain a
  deriving (Show, Eq)


data Lattice a = Lattice
  { nodes :: [Node a]
    -- ^ Invariants:
    -- assume that there is a Bottom node
    -- should be closed under joins and meets
  , joins :: Map (Node a, Node a) (Node a)
    -- ^ Invariant: should be total over `nodes`
  , meets :: Map (Node a, Node a) (Node a)
    -- ^ Invariant: should be total over `nodes`
  }
  deriving Show

data PartialOrdering
  = EQ'
  | LT'
  | GT'
  | NEQ

opposite :: PartialOrdering -> PartialOrdering
opposite = \case
  EQ' -> EQ'
  LT' -> GT'
  GT' -> LT'
  NEQ -> NEQ

class PartialOrd a where
  partialOrdering :: a -> a -> PartialOrdering

newtype ByOrd a = ByOrd { runByOrd :: a }

instance Ord a => PartialOrd (ByOrd a) where
  partialOrdering (ByOrd left) (ByOrd right) =
    case compare left right of
      EQ -> EQ'
      LT -> LT'
      GT -> GT'

newtype ElWise a = ElWise { runElWise :: a }
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (ElWise (a, b, c, d)) where
    ElWise (a1, a2, a3, a4) <> ElWise (b1, b2, b3, b4) =
      ElWise ( a1 <> b1, a2 <> b2, a3 <> b3, a4 <> b4)

instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
  Monoid (ElWise (a, b, c, d)) where
    mempty = ElWise (mempty, mempty, mempty, mempty)

class Functor4 (f :: * -> * -> * -> * -> *) where
  fmap4
    :: (la -> ra)
    -> (lb -> rb)
    -> (lc -> rc)
    -> (ld -> rd)
    -> f la lb lc ld
    -> f ra rb rc rd

instance Functor4 (,,,) where
  fmap4 fa fb fc fd (a, b, c, d) = (fa a, fb b, fc c, fd d)


instance (PartialOrd a, Show a) => PartialOrd (Node a) where
  partialOrdering Bottom Bottom = EQ'
  partialOrdering Bottom _ = LT'
  partialOrdering _ Bottom = GT'

  partialOrdering (Plain left) (Plain right) = partialOrdering left right
  partialOrdering (Join a1 a2 as) (Join b1 b2 bs) = _
  partialOrdering (Meet a1 a2 as) (Meet b1 b2 bs) = _

  partialOrdering (Plain a) (Join a1 a2 as) =
    if
    | allEQ -> EQ' -- Failure of normalization but w/e
    | allGE -> GT' -- All GE and not-all EQ implies some GT
    |
      (_, True, True, False) -> error . unwords $
        [ "Failed normalization:"
        , show (Plain a)
        , "and"
        , show (Join a1 a2 as)
        , "turned out to be both GE and LE"
        ]
      (_, True, False, False) -> GT'
      (_, False, True, False) -> LT'
      (_, False, False, False) -> error . unwords $
        [ show (Plain a)
        , "is not LE nor GE nor NEQ than any of"
        , show (Join a1 a2 as)
        , "; this shouldn't happen"
        ]
      where
        (allEQ, allGE, anyLE, anyNEQ) = fmap4 getAll getAll getAny getAny . runElWise $
          mconcat $ map
          (ElWise . fmap4 All All Any Any . widen .
            partialOrdering (Plain a)
          )
          (a1 : a2 : as)
        widen :: PartialOrdering -> (Bool, Bool, Bool, Bool) -- EQ, GE, LE, NEQ
        widen = \case
          EQ' -> (,,,) True  True  True  False
          LT' -> (,,,) False True  False False
          GT' -> (,,,) False False True  False
          NEQ -> (,,,) False False False True


  partialOrdering (Join a1 a2 as) (Plain a) =
    opposite $ partialOrdering (Plain a) (Join a1 a2 as)

  partialOrdering (Plain a) (Meet a1 a2 as) = _
  partialOrdering (Meet a1 a2 as) (Plain a) = _
    opposite $ partialOrdering (Plain a) (Meet a1 a2 as)

  partialOrdering (Meet b1 b2 bs) (Join a1 a2 as) = _
  partialOrdering (Join a1 a2 as) (Meet b1 b2 bs) =
    opposite $ partialOrdering (Meet b1 b2 bs) (Join a1 a2 as)

  partialOrdering Top Top = EQ'
  partialOrdering Top _ = GT'
  partialOrdering _ Top = LT'

fromOrdering :: Ord a => [Node a] -> Lattice a
fromOrdering nodes = Lattice
  { nodes = nodes
  , joins = Map.fromList do
      left <- nodes
      right <- nodes
      pure ((left, right), max left right)
  , meets = Map.fromList do
      left <- nodes
      right <- nodes
      pure ((left, right), min left right)
  }

fromOrdering' :: Ord a => [a] -> Lattice a
fromOrdering' preNodes = _