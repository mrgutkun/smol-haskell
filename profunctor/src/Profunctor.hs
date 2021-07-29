module Profunctor where

import Data.Profunctor(Profunctor(..))
import Data.Bifunctor(Bifunctor(..))
import Data.Functor.Contravariant(Contravariant(..))

newtype Prof b f g x y = Prof {runProf :: b (f x) (g y)}

instance (Bifunctor b, Contravariant f, Functor g) => Profunctor (Prof b f g) where
    dimap f g (Prof p) = Prof $ bimap (contramap f) (fmap g) p

-- dimap (f . g) (h . i) =? dimap g h . dimap f i
-- dimap (f . g) (h . i) = Prof . bimap (contramap f . g) (fmap h . i) . runProf
--                       = Prof . bimap (contramap g . contramap f) (fmap h . fmap i) . runProf
--                       = Prof . bimap (contramap g) (fmap h) . bimap (contamap f) (fmap i) . runProf
--                       = Prof . bimap (contramap g) (fmap h) . runProf . Prof . bimap (contamap f) (fmap i) . runProf
--                       = dimap g h . dimap f i