{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Reactive.Hyper.Sodium (
        Sodium
    ) where

import Reactive.Hyper
import qualified FRP.Sodium as S
import Control.Applicative
import Data.Monoid


data Sodium

instance Reactive Sodium where
    newtype Event Sodium a    = Event { unEvent :: S.Event a }
        deriving (Functor, Monoid)
    newtype Behavior Sodium a = Behavior { unBehavior :: S.Behavior a }
        deriving (Functor, Applicative)
    newtype Frame Sodium a = Frame (S.Reactive a)
        deriving (Functor, Applicative, Monad)

    stepper def (Event e) = Frame $ Behavior <$> S.hold def e

