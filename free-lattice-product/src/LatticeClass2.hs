module LatticeClass2 where

newtype Join a = Join { getJoin :: a }
newtype Meet a = Meet { getMeet :: a }

class (Monoid (Join a), Monoid (Meet a)) => Lattice a where
  top :: a
  bottom :: a
  join2 :: a -> a -> a
  meet2 :: a -> a -> a

  top = getMeet mempty
  bottom = getJoin mempty

  join2 l r = getJoin $ Join l <> Join r
  meet2 l r = getMeet $ Meet l <> Meet r

  {-# MINIMAL #-}