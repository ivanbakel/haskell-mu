{-# LANGUAGE TypeFamilies #-}

{-|
module      : Data.Computer
description : An interface to arrow-ish Haskell values, as a typeclass
copyright   : (c) Isaac van Bakel, 2020
license     : MIT
maintainer  : ivb@vanbakel.io
stability   : Experimental
portability : POSIX

When thinking about argument passing, it is natural to only consider the Haskell
arrow @->@. But in reality, many types (most of which *contain* @->@) also have
sensible argument-passing semantics - like @Identity (a -> b)@, @'Maybe' (a -> b)@
, etc. These types are "arrow-ish" - they behave somewhat like arrows.
-}

module Data.Computer
  ( Computer (..)
  )
  where

-- | An abstraction over Haskell arrows, for things which are "arrow-ish".
class Computer k where
  -- | The domain of the arrow. For lifted arrows, this should normally be the
  -- *unlifted* domain.
  type Dom k :: *
  -- | The codomain of the arrow. For lifted arrows, this should normally be the
  -- *lifted* codomain - though that is normally obvious, since rarely can a value
  -- be lowered.
  type Codom k :: *
  -- | Execution of the arrow.
  run :: k -> Dom k -> Codom k

instance {-# OVERLAPPING #-} Computer (a -> b) where
  type Dom (a -> b) = a
  type Codom (a -> b) = b
  run = ($)
