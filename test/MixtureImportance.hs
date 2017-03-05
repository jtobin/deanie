
module Main where

import Control.Monad
import Deanie
import Model.Mixture as Model

logDensity :: Bool -> Double -> Double
logDensity accept x
  | accept    = logDensityGaussian (negate 2) 0.5 x
  | otherwise = logDensityGaussian 2 0.5 x

posterior :: Program (Double, Bool)
posterior = importance Model.observations (bernoulli 0.5) logDensity

main :: IO ()
main = do
  samples <- replicateM 1000 (sample (rvar posterior))
  mapM_ print samples

  putStrLn "\nestimate:"
  print (mcw (fmap (\(w, x) -> if x then (w, 1) else (w, 0)) samples))

