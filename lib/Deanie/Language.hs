{-# LANGUAGE DeriveFunctor #-}

module Deanie.Language (
  -- * types

    ProbF(..)
  , Prob
  , ProgramF(..)
  , Program

  -- * terms

  , bernoulli
  , beta
  , gaussian
  , gamma

  -- * re-export

  , module Control.Applicative.Free
  , module Control.Monad.Free
  , module Data.Functor.Sum

  ) where

import Control.Applicative.Extended (replicateA)
import Control.Applicative.Free hiding (Pure)
import Control.Monad.Free
import Data.Functor.Sum
import Prelude hiding (product)

data ProbF r =
    BernoulliF {-# UNPACK #-} !Double (Bool -> r)
  | BetaF {-# UNPACK #-} !Double {-# UNPACK #-} !Double (Double -> r)
  | GaussianF {-# UNPACK #-} !Double {-# UNPACK #-} !Double (Double -> r)
  | GammaF {-# UNPACK #-} !Double {-# UNPACK #-} !Double (Double -> r)
  deriving Functor

type Prob = Free ProbF

newtype ProgramF a = ProgramF (Sum ProbF (Ap (Free ProgramF)) a)
  deriving Functor

type Program = Free ProgramF

bernoulli :: Double -> Program Bool
bernoulli p = liftF (ProgramF (InL (BernoulliF p id)))

beta :: Double -> Double -> Program Double
beta a b = liftF (ProgramF (InL (BetaF a b id)))

gamma :: Double -> Double -> Program Double
gamma a b = liftF (ProgramF (InL (GammaF a b id)))

gaussian :: Double -> Double -> Program Double
gaussian m s = liftF (ProgramF (InL (GaussianF m s id)))

