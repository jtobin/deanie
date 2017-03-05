
module Deanie.Inference.Comonadic where

import Control.Comonad.Cofree
import Deanie.Language

type Execution a = Cofree Program a

initialize :: ProgramF a -> ()
initialize = \case
    ProgramF branch -> case branch of
      InL term -> foo term
      InR term -> ()
  where
    foo = \case
      BernoulliF p _  -> ()
      BetaF a b _     -> ()
      GammaF a b _    -> ()
      GaussianF m s _ -> ()



