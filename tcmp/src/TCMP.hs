module TCMP where
-- Type-level metaprogramming

class Flatten a where
  flatten :: a -> [Element a]

instance {-# OVERLAPPABLE #-} Element [a] ~ a => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat


type family Element a where
  Element [[a]] = Element [a]
  Element [a] = a

example :: [Integer]
example = flatten [[[1 :: Integer]], [[2], [3, 4]], []]
-- example == [1,2,3,4]
