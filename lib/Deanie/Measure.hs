
module Deanie.Measure (
    measure

  -- * queries

  , integrate
  , expectation
  , variance
  , mgf
  , cgf
  , cdf
  ) where

import Control.Monad
import Data.List (foldl')
import Deanie.Language
import Control.Foldl (Fold)
import Numeric.Integration.TanhSinh
import Numeric.SpecFunctions

newtype Measure a = Measure ((a -> Double) -> Double)

integrate :: (a -> Double) -> Measure a -> Double
integrate f (Measure nu) = nu f

expectation :: Measure Double -> Double
expectation = integrate id

variance :: Measure Double -> Double
variance nu = integrate (^ 2) nu - expectation nu ^ 2

mgf :: Measure Double -> Double -> Double
mgf mu t = integrate (\x -> exp (t * x)) mu

cgf :: Measure Double -> Double -> Double
cgf mu = log . mgf mu

cdf :: Measure Double -> Double -> Double
cdf nu x = integrate (negativeInfinity `to` x) nu where
  negativeInfinity :: Double
  negativeInfinity = negate (1 / 0)

  to :: (Num a, Ord a) => a -> a -> a -> a
  to a b x
    | x >= a && x <= b = 1
    | otherwise        = 0

instance Functor Measure where
  fmap f nu = Measure $ \g ->
    integrate (g . f) nu

instance Applicative Measure where
  pure x = Measure (\f -> f x)
  Measure h <*> Measure g = Measure $ \f ->
    h (\k -> g (f . k))

instance Monad Measure where
  return x  = Measure (\f -> f x)
  rho >>= g = Measure $ \f ->
    integrate (\nu -> integrate f (g nu)) rho

fromMassFunction :: Foldable f => (a -> Double) -> f a -> Measure a
fromMassFunction f support = Measure $ \g ->
  foldl' (\acc x -> acc + f x * g x) 0 support

fromDensityFunction :: (Double -> Double) -> Measure Double
fromDensityFunction d = Measure $ \f ->
    quadratureTanhSinh (\x -> f x * d x)
  where
    quadratureTanhSinh = result . last . everywhere trap

mbernoulli :: Double -> Measure Bool
mbernoulli p = fromMassFunction (pmf p) [False, True] where
  pmf p x
    | p < 0 || p > 1 = 0
    | otherwise      = if x then p else 1 - p

mbeta :: Double -> Double -> Measure Double
mbeta a b = fromDensityFunction (density a b) where
  density a b p
    | p < 0 || p > 1 = 0
    | otherwise      = 1 / exp (logBeta a b) * p ** (a - 1) * (1 - p) ** (b - 1)

mgamma :: Double -> Double -> Measure Double
mgamma a b = fromDensityFunction (density a b) where
  density a b x
    | a < 0 || b < 0 = 0
    | otherwise  =
       b ** a / exp (logGamma a) * x ** (a - 1) * exp (negate (b * x))

mgaussian :: Double -> Double -> Measure Double
mgaussian m s = fromDensityFunction (density m s) where
  density m s x
    | s <= 0    = 0
    | otherwise =
        1 / (s * sqrt (2 * pi)) *
          exp (negate ((x - m) ^^ 2) / (2 * (s ^^ 2)))

measure :: Program a -> Measure a
measure = iterM $ \case
    ProgramF (InL term) -> evalAlg term
    ProgramF (InR term) -> join (runAp measure term)
  where
    evalAlg = \case
      BernoulliF p k  -> mbernoulli p >>= k
      BetaF a b k     -> mbeta a b >>= k
      GammaF a b k    -> mgamma a b >>= k
      GaussianF m s k -> mgaussian m s >>= k

