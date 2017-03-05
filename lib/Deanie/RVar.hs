
module Deanie.RVar (
    rvar

  -- * re-exported

  , sample
  ) where

import Control.Monad
import Deanie.Language
import Data.Random hiding (rvar)
import qualified Data.Random.Distribution.Bernoulli as RF
import qualified Data.Random.Distribution.Beta as RF
import qualified Data.Random.Distribution.Gamma as RF
import qualified Data.Random.Distribution.Normal as RF

rvar :: Program a -> RVar a
rvar = iterM $ \case
    ProgramF (InL term) -> evalAlg term
    ProgramF (InR term) -> join (runAp rvar term)
  where
    evalAlg = \case
      BernoulliF p k  -> RF.bernoulli p >>= k
      BetaF a b k     -> RF.beta a b >>= k
      GammaF a b k    -> RF.gamma a b >>= k
      GaussianF m s k -> RF.normal m s >>= k

