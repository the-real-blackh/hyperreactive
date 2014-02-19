{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds #-}
module Reactive.Hyper where

import Control.Applicative
import Data.Constraint.Forall
import Data.Monoid

class   (
            Functor (Event r),
            ForallF Monoid (Event r),
            Functor (Behavior r),
            Applicative (Behavior r),
            Monad (Frame r)
        ) =>
        Reactive r where

    data Event r a :: *
    data Behavior r a :: *
    data Frame r a :: *

    stepper :: a -> Event r a -> Frame r (Behavior r a)

