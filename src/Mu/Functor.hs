{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}

{-|
module      : Mu.Functor
description : A Haskell version of the lambda-mu continuation abstraction, as a monad
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

This module defines the mu abstraction in terms of an "unlimited continuation",
which allows for repeated transformations of a Haskell term. The resulting monad
can be used alongside existing Haskell features to model all features of
lambda-mu's semantics.
-}

module Mu.Functor
  ( Mu (..)
  , empty
  , yield
  )
  where

import Data.Apptor
import Data.Functor.Identity
import Data.Ignorant

-- | The unlimited continuation construction.
--
-- A @Mu@ term allows for possibly side-effecting transformations of its
-- argument type, which then yields a new @Mu@ term of the resultant type.
data Mu k
  = Mu
    { runMu :: forall f r. (Applicative f) => (k -> f r) -> f (Mu r)
      -- ^ Apply the possibly side-effecting transformation, getting back the
      -- wrapped continuation which can then be used to continue applying more
      -- transformations.
      --
      -- Note the similarity to a typical side-effecting continuation,
      --
      -- > forall f r. (k -> f r) -> f r
      --
      -- The key feature of the @Mu@ term is the replacement of the result type with
      -- *a @Mu@ term of that type*, giving an unlimited continuation.
      --
      -- The constraint of 'Applicative' would more accurately be 'Functor' and
      -- 'Pointed', since 'pure' is necessary to build the argument-discarding
      -- mu term.
    }

-- | The transformation-discarding unlimited continuation.
--
-- One of lambda-mu's key features is that a mu abstraction does not need to
-- point into a subterm: it can also "throw away" any arguments by not pointing
-- to any valid name. This is normally represented as a mu abstraction with "no"
-- name
--
-- > mu _. [a] ...
--
-- This is the Haskell equivalent - an unlimited continuation that doesn't ever
-- do any computation, instead discarding all transformations.
empty :: Mu a
empty = Mu \_ -> pure empty

-- | The unlimited continuation starting from the given value.
yield :: a -> Mu a
yield val = Mu \f -> yield <$> f val

-- | Flatten a nested continuation
flatten :: Mu (Mu a) -> Mu a
flatten (Mu f) = Mu \g -> flatten <$> (f \(Mu inner) -> inner g)

instance Functor Mu where
  fmap f (Mu runner) = runIdentity (runner (Identity . f))

instance Applicative Mu where
  pure = yield
  (Mu l) <*> (Mu r) = Mu \cont -> flatten <$> l \left -> r (cont . left)

instance Monad Mu where
  (Mu l) >>= f = flatten (l f)
