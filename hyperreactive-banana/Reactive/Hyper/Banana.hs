{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Reactive.Hyper.Banana (
        Banana
    ) where

import Reactive.Hyper
import qualified Reactive.Banana as B
import Control.Applicative
import Data.Monoid


data Banana

type T = Double

instance Reactive Banana where
    newtype Event Banana a    = Event (B.Event T a)
        deriving Functor
    newtype Behavior Banana a = Behavior (B.Behavior T a)
        deriving (Functor, Applicative)
    newtype Frame Banana a = Frame (B.Moment T a)
        deriving (Functor, Applicative, Monad)

    stepper def (Event e) = Frame $ return $ Behavior $ B.stepper def e

instance Monoid (Event Banana a) where
    mempty = Event B.never
    Event ea `mappend` Event eb = Event (ea `B.union` eb) 

