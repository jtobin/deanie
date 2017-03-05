
module Model.BetaBernoulli (
    betaBernoulli
  , observations
  ) where

import Deanie

betaBernoulli :: Double -> Double -> Program Bool
betaBernoulli a b = do
  p <- beta a b
  bernoulli p

observations :: [Bool]
observations = [True, True, False, True, False, False, True, True, True]

