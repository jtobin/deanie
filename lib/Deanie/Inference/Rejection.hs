
module Deanie.Inference.Rejection (

    rejection
  , grejection
  ) where

import Control.Monad
import qualified Data.Foldable as F

grejection
  :: (Foldable f, Monad m)
  => ([a] -> [b] -> Bool) -> f b -> m c -> (c -> m a) -> m c
grejection predicate observed proposal model = loop where
  len  = length observed
  loop = do
    parameters <- proposal
    generated  <- replicateM len (model parameters)
    if   predicate generated (F.toList observed)
    then return parameters
    else loop

rejection :: (Foldable f, Monad m, Eq a) => f a -> m b -> (b -> m a) -> m b
rejection = grejection (==)

