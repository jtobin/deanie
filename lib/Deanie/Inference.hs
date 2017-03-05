
module Deanie.Inference (

  -- * functions

    mc
  , mcw

  -- * modules

  , module Export
  ) where

import qualified Control.Foldl as L
import Data.Monoid
import Deanie.Inference.Importance as Export
import Deanie.Inference.Metropolis as Export
import Deanie.Inference.Rejection as Export

data Average a = Average !a !a

instance Fractional a => Monoid (Average a) where
  mempty = Average 0 0
  mappend (Average nL xL) (Average nR xR) = Average n x where
    n = nL + nR
    x = (xL * nL + xR * nR) / (nL + nR)

average :: Fractional a => L.Fold a a
average = L.Fold tally mempty summarize where
  tally x a = x <> Average 1 a
  summarize (Average _ x) = x
{-# INLINE average #-}

mc :: (Foldable f, Fractional a) => f a -> a
mc = L.fold average
{-# INLINE mc #-}

mcw :: (Foldable f, Fractional a) => f (a, a) -> a
mcw pts = L.fold (L.premap weight L.sum) pts where
  mass = L.fold (L.premap fst L.sum) pts
  weight (w, v) = w / mass * v
{-# INLINE mcw #-}

