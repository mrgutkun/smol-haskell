module PartialOrdering
  ( PartialOrdering
  , opposite
  , PartialOrd(..)
  , ByOrd(..)
  ) where

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
