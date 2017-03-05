
module Main where

import Deanie
import Model.BetaBernoulli as Model

posterior :: Double -> Double -> Program [Double]
posterior a b =
  metropolis 1000 Model.observations (beta a b) logDensityBernoulli

main :: IO ()
main = do
  samples <- sample (rvar (posterior 1 1))
  mapM_ print samples

  putStrLn "\nestimate:"
  print (mc samples)

