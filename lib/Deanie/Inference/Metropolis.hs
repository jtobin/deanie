
module Deanie.Inference.Metropolis (
    metropolis
  ) where

import Deanie.Language
import qualified Control.Foldl as L
import Control.Monad.Loops

data MHP a = MHP {
    n       :: {-# UNPACK #-} !Int
  , current :: !a
  , ccost   :: {-# UNPACK #-} !Double
  }

metropolis
  :: Foldable f
  => Int -> f a -> Program b -> (b -> a -> Double)
  -> Program [b]
metropolis epochs obs prior model = do
    current <- prior
    unfoldrM mh MHP { n = epochs, ccost = cost current, .. }
  where
    cost param = L.fold (L.premap (model param) L.sum) obs

    mh MHP {..}
      | n <= 0    = return Nothing
      | otherwise = do
          proposal <- prior
          let pcost = cost proposal
              prob  = moveProbability ccost pcost
          accept <- bernoulli prob
          let (nepochs, nlocation, ncost) =
                if   accept
                then (pred n, proposal, pcost)
                else (pred n, current, ccost)
          return (Just (nlocation, MHP nepochs nlocation ncost))

moveProbability :: Double -> Double -> Double
moveProbability current proposal =
    whenNaN 0 (exp (min 0 (proposal - current)))
  where
    whenNaN val x
      | isNaN x   = val
      | otherwise = x
{-# INLINE moveProbability #-}

