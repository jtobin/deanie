
module Deanie.Density (
    logDensityBernoulli
  , logDensityBeta
  , logDensityGamma
  , logDensityGaussian
  ) where

import Numeric.SpecFunctions (logGamma)

logDensityBernoulli :: Double -> Bool -> Double
logDensityBernoulli p x
    | p < 0 || p > 1 = log 0
    | x              = log p
    | otherwise      = log (1 - p)
{-# INLINE logDensityBernoulli #-}

logDensityBeta :: Double -> Double -> Double -> Double
logDensityBeta a b x
  | x <= 0 || x >= 1 = log 0
  | a < 0 || b < 0   = log 0
  | otherwise        = (a - 1) * log x + (b - 1) * log (1 - x)
{-# INLINE logDensityBeta #-}

-- | The log-density for the gamma distribution with rate parameter b.
--
--     f(x) = b ^ a * x ^ (a - 1) * x ^ (-b * x) / Gamma(a)
logDensityGamma :: Double -> Double -> Double -> Double
logDensityGamma a b x
  | a < 0 || b < 0 = log 0
  | otherwise      = a * log b + (a - 1) * log x - b * x - logGamma a
{-# INLINE logDensityGamma #-}

-- | The density for the normal distribution with specified mean and standard
--   deviation.
logDensityGaussian :: Double -> Double -> Double -> Double
logDensityGaussian m sd x
  | sd <= 0   = log 0
  | otherwise = negate (log sd) - (x - m) * (x - m) / (2 * sd * sd)
{-# INLINE logDensityGaussian #-}

