{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Reactive.Hyper.Sodium (
        Sodium
    ) where

import Reactive.Hyper
import qualified FRP.Sodium as S
import Control.Applicative
import Control.Monad.Fix
import Data.Monoid


data Sodium

instance Reactive Sodium where
    newtype Event Sodium a    = Event { unEvent :: S.Event a }
        deriving (Functor)
    newtype Behavior Sodium a = Behavior { unBehavior :: S.Behavior a }
        deriving (Functor, Applicative)
    newtype Frame Sodium a = Frame (S.Reactive a)
        deriving (Functor, Applicative, Monad, MonadFix)

    hold def (Event e) = Frame $ Behavior <$> S.hold def e
    Behavior bf <@> Event ea = Event (S.snapshot (flip ($)) ea bf)
    filterJust (Event ema) = Event (S.filterJust ema)
    mempty_ = Event mempty
    Event a `mappend_` Event b = Event (a `mappend` b)

