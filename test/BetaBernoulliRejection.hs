
module Main where

import Control.Monad
import Deanie
import Model.BetaBernoulli as Model

posterior :: Double -> Double -> Program Double
posterior a b =
    grejection
      (\xs ys -> count xs == count ys)
      Model.observations (beta a b) bernoulli
  where
    count = length . filter id

main :: IO ()
main = do
  samples <- replicateM 1000 (sample (rvar (posterior 1 1)))
  mapM_ print samples

  putStrLn "\nestimate:"
  print (mc samples)

