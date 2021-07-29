module LatticeClass where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NES

class Lattice a where
  top :: a
  bottom :: a
  join2 :: a -> a -> a
  joinMany :: a -> a -> [a] -> a

  meet2 :: a -> a -> a
  meetMany :: a -> a -> [a] -> a

  join2 l r = joinMany l r []
  meet2 l r = meetMany l r []

  joinMany l r rest = foldl' join2 l (r:rest)
  meetMany l r rest = foldl' meet2 l (r:rest)


  {-# MINIMAL top, bottom, (join2 | joinMany), (meet2 | meetMany) #-}
  -- invariants:
  -- joins and meets better be invariant under reordering
  -- join of top is top; join ignores bottoms
  -- meet of bottom is bottom, meet ignores tops

----------------------------------------------------------------

data FlatNode a
  = FTop
  | FBottom
  | FPlain a
  deriving (Show, Eq)

instance Ord a => Ord (FlatNode a) where
  compare FBottom FBottom = EQ
  compare FBottom _ = LT
  compare _ FBottom = GT

  compare (FPlain a) (FPlain b) = compare a b

  compare FTop FTop = EQ
  compare FTop _ = GT
  compare _ FTop = LT

instance Ord a => Lattice (FlatNode a) where
  top = FTop
  bottom = FBottom
  joinMany l r ns = maximum (l : r : ns)
  meetMany l r ns = minimum (l : r : ns)

----------------------------------------------------------------

data Node a
  = Top
  | Bottom

  -- | Join2 a a
  -- | Meet2 a a

  | Join (Node a) (Node a) [Node a]
  | Meet (Node a) (Node a) [Node a]

  | Plain a
  deriving (Show, Eq)

newtype Free a = Free { getFree :: a }

instance Lattice (Free (Node a)) where
  top = Free Top
  bottom = Free Bottom
  joinMany n1 n2 ns = Free $ Join (getFree n1) (getFree n2) (map getFree ns)
  meetMany n1 n2 ns = Free $ Meet (getFree n1) (getFree n2) (map getFree ns)

----------------------------------------------------------------

-- | In which Ord is semantic, compatible w/ lattice operations
normalizeSemOrd :: Ord a => Node a -> FlatNode a
normalizeSemOrd = \case
  Top -> FTop
  Bottom -> FBottom
  Plain a -> FPlain a

  -- Join2 l r -> FPlain (max l r)
  -- Meet2 l r -> FPlain (min l r)

  Join n1 n2 ns -> maximum $ map normalizeSemOrd (n1 : n2 : ns)
  Meet n1 n2 ns -> minimum $ map normalizeSemOrd (n1 : n2 : ns)

embedFlat :: FlatNode a -> Node a
embedFlat = \case
  FTop -> Top
  FBottom -> Bottom
  FPlain a -> Plain a

newtype Ordered a = Ordered { getOrdered :: Node a }

instance Ord a => Lattice (Ordered a) where
  top = Ordered Top
  bottom = Ordered Bottom
  joinMany n1 n2 ns = Ordered . embedFlat . maximum $
    map (normalizeSemOrd . getOrdered) (n1 : n2 : ns)
  meetMany n1 n2 ns = Ordered . embedFlat . minimum $
    map (normalizeSemOrd . getOrdered) (n1 : n2 : ns)

----------------------------------------------------------------

data DistrNode a
  = DTop
  | DBottom
  | DCNF (NESet (NESet a)) -- \/ outside, /\ inside
  deriving (Eq, Show)

instance Lattice (Free (DistrNode a)) where
  top = Free DTop
  bottom = Free DBottom
  meet2 (Free l) (Free r) = case (l, r) of
    (DTop, a) -> Free a
    (a, DTop) -> Free a
    (DBottom, _) -> Free DBottom


  join2 l r = _

----------------------------------------------------------------

-- | In which Ord is a source of canonical forms,
-- but is not supposed to interact with the lattice otherwise
normalizeSyntaxOrd :: Ord a => Node a -> DistrNode a
normalizeSyntaxOrd = \case
  Top -> DTop
  Bottom -> DBottom
  Plain a -> DCNF $ NES.singleton (NES.singleton a)
  Join n1 n2 ns -> _ $ fmap normalizeSyntaxOrd (n1 :| n2 : ns)
  Meet n1 n2 ns -> _ $ fmap normalizeSyntaxOrd (n1 :| n2 : ns)

embedDistr :: DistrNode a -> Node a
embedDistr = \case
  DTop -> Top
  DBottom -> Bottom
  DCNF disjNESet ->
    case NES.elems disjNESet of
      (conj :| []) -> embedConj conj
      (conj1 :| (conj2 : rest)) ->
        Join (embedConj conj1) (embedConj conj2) (map embedConj rest)

  where
    embedConj :: NESet a -> Node a
    embedConj conjNESet =
      case NES.elems conjNESet of
        (conj :| []) -> Plain conj
        (conj1 :| conj2 : rest) ->
          Meet (Plain conj1) (Plain conj2) (map Plain rest)