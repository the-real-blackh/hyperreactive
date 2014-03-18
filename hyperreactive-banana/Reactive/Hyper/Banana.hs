{-# LANGUAGE TypeFamilies, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving,
        RankNTypes #-}
module Reactive.Hyper.Banana (
        Banana,
        B.Frameworks(..),
        compile,
        B.EventNetwork,
        B.actuate,
        fromAddHandler,
        B.AddHandler(..),
        newEvent,
        reactimate
    ) where

import Reactive.Hyper
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Control.Applicative
import Control.Monad.Fix
import Data.Monoid


data Banana t

instance Reactive (Banana t) where
    newtype Event (Banana t) a    = Event (B.Event t a)
        deriving Functor
    newtype Behavior (Banana t) a = Behavior (B.Behavior t a)
        deriving (Functor, Applicative)
    newtype Frame (Banana t) a = Frame { unFrame :: B.Moment t a }
        deriving (Functor, Applicative, Monad, MonadFix)

    hold def (Event e) = Frame $ return $ Behavior $ B.stepper def e
    Behavior bf <@> Event ea = Event (bf B.<@> ea)
    filterJust (Event ema) = Event (B.filterJust ema)
    mempty_ = Event B.never
    Event ea `mappend_` Event eb = Event (ea `B.union` eb) 

compile :: (forall t . B.Frameworks t => Frame (Banana t) ()) -> IO B.EventNetwork
compile fr = B.compile (unFrame fr)

fromAddHandler :: B.Frameworks t => B.AddHandler a -> Frame (Banana t) (Event (Banana t) a)
fromAddHandler ah = Frame $ Event <$> B.fromAddHandler ah

newEvent :: B.Frameworks t => Frame (Banana t) (Event (Banana t) a, a -> IO ())
newEvent = Frame $ do
    (e, send) <- B.newEvent
    return (Event e, \a -> send a)

reactimate :: B.Frameworks t => Event (Banana t) a -> (a -> IO ()) -> Frame (Banana t) ()
reactimate (Event e) handle = Frame $ B.reactimate (handle <$> e)

