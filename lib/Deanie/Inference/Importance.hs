
module Deanie.Inference.Importance (
    importance
  ) where

import qualified Control.Foldl as L

importance
  :: (Foldable f, Monad m, Floating w)
  => f a -> m b -> (b -> a -> w)
  -> m (w, b)
importance obs prior model = do
  parameter <- prior
  let cost = L.fold (L.premap (model parameter) L.sum) obs
  return (exp cost, parameter)

