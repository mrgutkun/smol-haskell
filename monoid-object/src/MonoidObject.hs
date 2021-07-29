{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module MonoidObject where

{-
  So what we want to do here is: define something like `instance Monad m => Monoid m where`
  How?
  The idea: we need a 
  ```
  class Category sort => MonoidObject (m :: k -> (k :: sort)) where
    eta :: Id a -> m a
    mu :: (m circ m) a -> m a
  ```
  where Id and circ are type-level analogues of id and . of the Category

  And so we'll also need a custom Category in which...
  uuuuuuhhhhhh, I dunno if we will have the type of arrows?
  Id :: k -> k, and circ :: (k -> l) -> (l -> r) -> (k -> r), I think?
  ...oooook, we did not specify the arrow. we could but if we can _not_ do that, that'd be grand.
    I think we'll need _something_ to be a parameter? so maybe we'll say that k l r :: s.
    Or we could make them all = Type?

  ANd then when we have MonoidObject, we can do something like
  ```
  instance Category Type {- Type2 -} where
    type Id a = a
    type (kl . lr) a = lr (kl a)
  ```
  instance Monad m => MonoidObject m where
    eta = pure
    mu mma = mma >>= id
     
-}

class Category (arr :: sort -> sort -> sort) where
  type Id :: forall (k :: sort) . (k `arr` k)
  type (.) :: (k `arr` l) -> (l `arr` r) -> (k `arr` r)

class Category sort => MonoidObject (m :: k -> (k :: sort)) where
  eta :: Id a -> m a
  mu :: (m . m) a -> m a

instance Category * {- Type2 -} where
  type Id a = a
  type (kl . lr) a = lr (kl a)

instance Monad m => MonoidObject m where
  eta = pure
  mu mma = mma >>= id
