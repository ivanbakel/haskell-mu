{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
module      : Mu.Apptor
description : A Haskell version of the lambda-mu continuation abstraction, as an embedding
copyright   : (c) Isaac van Bakel, 2020
license     : MIT
maintainer  : ivb@vanbakel.io
stability   : Experimental
portability : POSIX

Lambda-mu is an extension to the lambda calculus with the mu abstraction - a kind
of function which allows for repeated application, and which can be viewed as an
abstract representation of continuations. Specifically,

> (mu a. ... mu _. [a] p) q ~> (mu a. ... mu _. [a] p q)

The mu abstraction can be viewed as a "pointer" into a subterm which allows for
appending function arguments to an incomplete computation.

This module defines a version of the mu abstraction which allows for passing
arguments to a value for as long as it continues to take them. The result is
weaker than a functor, but stronger than a monad.
-}

module Mu.Apptor
  ( Mu (..)
  , empty
  , yield
  )
  where

import Data.Apptor
import Data.Computer
import Data.Embedding
import Data.Functor.Identity
import Data.Ignorant
import Data.Pointed

data Mu k
  = Mu
    { runMu :: forall f x y. (Applicative f, Computer k) => f (Dom k) -> f (Mu (Codom k))
    }


-- | The argument-discarding mu term
--
-- One of lambda-mu's key features is that a mu abstraction does not need to
-- point into a subterm: it can also "throw away" any arguments by not pointing
-- to any valid name. This is normally represented as a mu abstraction with "no"
-- name
--
-- > mu _. [a] ...
--
-- This is the equivalent in 'Computer' terms - a term that discards all its
-- arguments.
empty :: Mu a
empty = Mu \_ -> pure empty

-- | The mu term which passes arguments to the given value
yield :: a -> Mu a
yield val = Mu \arg -> (yield . run val) <$> arg

instance (Computer k) => Computer (Mu k) where
  type Dom (Mu k) = Dom k
  type Codom (Mu k) = Mu (Codom k)
  run = fpass

instance Apptor Mu where
  fpass (Mu runner) arg = runIdentity (runner (Identity arg))

instance Pointed Mu where
  point = yield

instance Embedding Mu where
  debind f = Mu \arg -> f <$> arg
  flatten (Mu f) = Mu ((flatten <$>) . f)

instance Ignorant Mu where
  ignorant = empty
