{-# LANGUAGE FlexibleInstances #-}

module Data.Ignorant
  ( Ignorant (..)
  )
  where

import Data.Apptor
import Data.Computer
import Data.Embedding
import Data.Functor.Const
import Data.Pointed

-- | The ignorant arrow, which discards arguments
--
-- Every embedding has, in general, arrows which discard a finite number of
-- arguments - since it is possible describe such Haskell arrows, and possible
-- to lift them into the embedding.
--
-- However, embeddings do not have in general a *infinitely* discarding arrow
-- definition - that is, a single term which can be made to discard any number
-- of arguments. For those that do, the unique such arrow (since it does nothing,
-- it must be unique) is called the "ignorant" arrow.
class (Embedding f) => Ignorant f where
  -- | This arrow should satisfy
  --
  -- prop> 'ignorant' <<> a == 'ignorant'
  ignorant :: f a

instance Ignorant (Const ()) where
  ignorant = Const ()
