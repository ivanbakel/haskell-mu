{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
module      : Data.Embedding
description : A description of surjective lifts in Haskell, as a typeclass.
copyright   : (c) Isaac van Bakel, 2020
license     : MIT
maintainer  : ivb@vanbakel.io
stability   : Experimental
portability : POSIX

-}

module Data.Embedding
  ( Embedding (..)
  )
  where

import Data.Apptor
import Data.Computer
import Data.Functor.Const
import Data.Functor.Identity
import Data.Pointed
import Control.Monad

-- | A lift which models at most as much as the unlifted version.
--
-- In general, 'Monad's have the effect of *expanding* the range of possible
-- computation when going from the domain to the codomain - for example, 'IO'
-- allows for side effects, and 'Maybe' allows for absent arguments or return
-- values.
--
-- An @Embedding@, on the other hand, is *limited* to the range of possible
-- computation in the domain when considering the codomain. It has the property
-- that every arrow in the codomain between the images of objects is itself an
-- image of an arrow in the domain between those objects. This can be seen in the
-- type of 'embed', which yields the lifted preimage of such an arrow.
--
-- Because of this, 'Monad's in general are *not* @Embedding@s - 'Maybe', for
-- example, cannot transform a 'Maybe a -> Maybe b' into a 'Maybe (a -> b)', since
-- it would have to decide if the function is total (i.e. always gives @'Just' b@) on
-- @'Just' a@.
--
-- An @Embedding@ which is also an 'Applicative' is also a 'Monad'.
class (Apptor f) => Embedding f where
  {-# MINIMAL (debind | continue), flatten #-}
  -- | Take the preimage of an arrow between lifted objects. This should satisfy
  --
  -- prop> 'embed' id == 'point' id
  --
  -- If @f@ is also a 'Functor', this should satisfy
  --
  -- prop> 'embed' . 'fmap' == 'point'
  embed :: (f a -> f b) -> f (a -> b)
  embed f = debind (f . point)

  -- | This should satisfy
  --
  -- prop> 'debind' (f <<>) == f
  debind :: (a -> f b) -> f (a -> b)
  debind f = continue (point f)

  -- | This should satisfy
  --
  -- prop> ('continue' f) <<> x == 'flatten' (f <<> x)
  continue :: f (a -> f b) -> f (a -> b)
  continue lifted = debind (flatten . (lifted <<>))

  -- | Flatten a nested embedding. Since the embedding simulates *at most* the
  -- computation of the domain, nesting it should have no effect.
  --
  -- This should satisfy
  --
  -- prop> 'flatten' ('point' ('point' x)) == 'point' x
  flatten :: f (f a) -> f a

instance Apptor Identity where
  fpass (Identity f) x = Identity (run f x)

-- | The identity embedding.
instance Embedding Identity where
  debind f = Identity (runIdentity . f)
  flatten (Identity x) = x

instance Apptor (Const ()) where
  fpass (Const ()) _ = Const ()

-- | The trivial embedding.
--
-- Since every arrow in @'Const' ()@ is the identity, it is trivial to produce
-- a lifted preimage of any arrow in the codomain. The embedding is lawful since
-- the 'embed' of the identity arrow does give its lift (its 'point').
instance Embedding (Const ()) where
  embed _id = Const ()
  continue _yieldsconst = Const ()
  flatten (Const ()) = Const ()

-- | An instance which justifies the statement that 'Embedding's which are
-- also 'Applicative's are stronger 'Monad's.
instance {-# OVERLAPPABLE #-} (Applicative f, Embedding f) => Monad f where
  m >>= f = (debind f) <*> m
