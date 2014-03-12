{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Reactive.Hyper.Banana (
        Banana
    ) where

import Reactive.Hyper
import qualified Reactive.Banana as B
import Control.Applicative
import Control.Monad.Fix
import Data.Monoid


data Banana

type T = Double

instance Reactive Banana where
    newtype Event Banana a    = Event (B.Event T a)
        deriving Functor
    newtype Behavior Banana a = Behavior (B.Behavior T a)
        deriving (Functor, Applicative)
    newtype Frame Banana a = Frame (B.Moment T a)
        deriving (Functor, Applicative, Monad, MonadFix)

    hold def (Event e) = Frame $ return $ Behavior $ B.stepper def e
    Behavior bf <@> Event ea = Event (bf B.<@> ea)
    filterJust (Event ema) = Event (B.filterJust ema)
    mempty_ = Event B.never
    Event ea `mappend_` Event eb = Event (ea `B.union` eb) 

