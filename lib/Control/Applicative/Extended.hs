
module Control.Applicative.Extended (
    replicateA

  , module Control.Applicative
  ) where

import Control.Applicative

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA . replicate n

