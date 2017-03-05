
module Main where

import Control.Monad
import Deanie
import Model.BetaBernoulli as Model

posterior :: Double -> Double -> Program (Double, Double)
posterior a b =
  importance Model.observations (beta a b) logDensityBernoulli

main :: IO ()
main = do
  samples <- replicateM 1000 (sample (rvar (posterior 1 1)))
  mapM_ print samples

  putStrLn "\nestimate:"
  print (mcw samples)

