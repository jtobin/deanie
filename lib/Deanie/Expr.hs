
module Deanie.Expr (
  -- * utilities

    product
  , iid
  , indep

  -- * distributions

  , binomial
  , uniform
  , exponential
  , chisq
  , coin
  , lgaussian
  , invgamma
  , geometric
  ) where

import Control.Applicative.Extended
import qualified Control.Foldl as L
import Control.Monad
import Data.Function (fix)
import Deanie.Language
import Prelude hiding (product)

product :: Ap (Free ProgramF) a -> Program a
product term = liftF (ProgramF (InR term))

iid :: Int -> Program a -> Program [a]
iid n term = product (replicateA n (liftAp term))

indep :: f a -> Ap f a
indep = liftAp

binomial :: Int -> Double -> Program Int
binomial n p = fmap count (replicateM n (bernoulli p)) where
  count = L.fold (L.handles (L.filtered id) L.length)

uniform :: Program Double
uniform = beta 1 1

exponential :: Double -> Program Double
exponential = gamma 1

chisq :: Integral a => a -> Program Double
chisq k = gamma (fromIntegral k / 2) (1 / 2)

coin :: Program Bool
coin = bernoulli 0.5

lgaussian :: Double -> Double -> Program Double
lgaussian m sd = fmap exp (gaussian m sd)

invgamma :: Double -> Double -> Program Double
invgamma a b = fmap recip (gamma a b)

geometric :: Double -> Program Int
geometric p = fix $ \count -> do
  accept <- bernoulli p
  if   accept
  then return 1
  else fmap succ count

