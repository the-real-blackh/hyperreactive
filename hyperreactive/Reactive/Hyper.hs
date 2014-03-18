{-# LANGUAGE TypeFamilies, FlexibleContexts, RecursiveDo #-}
module Reactive.Hyper (
        Reactive(..),
        module Control.Applicative,
        module Data.Monoid
    ) where

import Control.Applicative
import Control.Monad.Fix
import Data.Monoid

infixl 4 <@>
infixl 4 <@

class   (
            Functor (Event r),
            Functor (Behavior r),
            Applicative (Behavior r),
            Functor (Frame r),
            Applicative (Frame r),
            Monad (Frame r),
            MonadFix (Frame r)
        ) =>
        Reactive r where

    data Event r a :: *
    data Behavior r a :: *
    data Frame r a :: *

    hold :: a -> Event r a -> Frame r (Behavior r a)

    (<@>) :: Behavior r (a -> b) -> Event r a -> Event r b

    (<@)  :: Behavior r b -> Event r a -> Event r b
    b <@ e = (const <$> b) <@> e

    accum :: a -> Event r (a -> a) -> Frame r (Behavior r a)
    accum def ef = do
        rec
            s <- hold def ((flip ($) <$> s) <@> ef)
        return s

    filterJust :: Event r (Maybe a) -> Event r a
    
    mempty_ :: Event r a
    mappend_ :: Event r a -> Event r a -> Event r a

instance Reactive r => Monoid (Event r a) where
    mempty = mempty_
    mappend = mappend_

