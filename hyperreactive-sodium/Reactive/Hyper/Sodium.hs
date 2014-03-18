{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Reactive.Hyper.Sodium (
        Sodium,
        sync,
        newEvent,
        newBehavior,
        listen,
        listenB
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

sync :: Frame Sodium a -> IO a
sync (Frame ma) = S.sync ma

newEvent :: Frame Sodium (Event Sodium a, a -> Frame Sodium ())
newEvent = Frame $ do
    (e, send) <- S.newEvent
    return (Event e, \a -> Frame $ send a)

newBehavior :: a -> Frame Sodium (Behavior Sodium a, a -> Frame Sodium ())
newBehavior a = Frame $ do
    (b, send) <- S.newBehavior a
    return (Behavior b, \a -> Frame $ send a)

listen :: Event Sodium a -> (a -> IO ()) -> Frame Sodium (IO ())
listen (Event e) handle = Frame $ S.listen e handle

listenB :: Behavior Sodium a -> (a -> IO ()) -> Frame Sodium (IO ())
listenB (Behavior b) handle = Frame $ S.listen (S.value b) handle

