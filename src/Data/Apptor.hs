{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
module      : Data.Apptor
description : An abstract representation of argument passing, as a typeclass
copyright   : (c) Isaac van Bakel, 2020
license     : MIT
maintainer  : ivb@vanbakel.io
stability   : Experimental
portability : POSIX

Representing abstract computation in Haskell is typically done through a 'Functor'
(or more specifically, a 'Monad'). However, since functors lift pure Haskell
functions to arrows in the functor, they allow for arbitrary *transformations*
of values in the lifted domain.

If we are instead interested in representing computation through argument passing,
we need a weaker abstraction that doesn't allow for arbitrary transformations.
This motivates the definition of an 'Apptor', a weakening of a functor which
only allows for lifting the domain of a lifted arrow, thus passing arguments
into the lifted domain.
-}

module Data.Apptor
  ( Apptor (..)
  , (<<>)
  )
  where

import Data.Computer
import Data.Pointed

-- | A weaker functor for argument passing
--
-- An @Apptor@ is similar to a 'Functor', except that it only allows for passing
-- arguments to a lifted arrow, rather than applying arrows to a lifted argument.
class (Pointed f) => Apptor f where
  -- | Pass an argument to a lifted arrow. This should satisfy
  --
  -- prop> 'fpass' ('point' f) a == 'point' (f a)
  fpass :: (Computer k) => f k -> Dom k -> f (Codom k)

-- | An instance which justifies the idea that an 'Apptor' is really a
-- weakening of a 'Functor' (provided it is also a 'Pointed').
instance {-# OVERLAPPABLE #-} (Pointed f, Functor f) => Apptor f where
  fpass term arg
    = fmap (`run` arg) term

-- | An infix version of 'fpass' that also restricts to the Haskell arrow as
-- a computer.
(<<>) :: (Apptor f) => f (a -> b) -> a -> f b
(<<>) = fpass

-- This should be definable for any `f`, but due to type family conflicts it
-- cannot be defined for the general `f`.
{- instance {-# OVERLAPPABLE #-} (Computer k, Apptor f) => Computer (f k) where
  type Dom (f k) = Dom k
  type Codom (f k) = f (Codom k)
  run = fpass -}
