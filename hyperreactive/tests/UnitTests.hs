{-# LANGUAGE OverloadedStrings, RecursiveDo, Rank2Types, FlexibleInstances, FlexibleContexts,
             TupleSections, GADTs, ConstraintKinds #-}
module UnitTests where

import Reactive.Hyper
import Data.Maybe
import Data.Text (Text)
import Data.Traversable


data Result = Pass | Fail String deriving (Eq, Ord, Show)

data Test r = Test {
    teName :: Text,
    teCode :: Event r () -> Frame r (Event r Result)
  }

input :: Reactive r =>
         Event r () -> [a] -> Frame r (Event r a, Event r ())
input eStep values = do
    vs <- accum values (drop 1 <$ eStep)
    let eValue = filterJust $ (listToMaybe <$> vs) <@ eStep
        eEnd   = filterJust $ ((\xs -> if null xs then Just () else Nothing) <$> vs) <@ eStep 
    return (eValue, eEnd)

input2 :: Reactive r =>
          Event r () -> [(Maybe a, Maybe b)] -> Frame r (Event r a, Event r b, Event r ())
input2 eStep values = do
    (ep, eEnd) <- input eStep values
    return (filterJust $ fst <$> ep, filterJust $ snd <$> ep, eEnd)

merge1 :: Reactive r => Test r
merge1 = Test "merge1" $ \eStep -> do
    (e1, e2, eEnd) <- input2 eStep [
        (Just ("hello" :: Text), Nothing),
        (Nothing,      Just ("world" :: Text))
      ]
    answers <- accum [] ((:) <$> e1 <> e2)
    return $ (\as ->
        if as == ["hello", "world"] then Pass
                                    else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

tests :: (Reactive r) =>
         [Test r]
tests = [
    merge1
  ]

suite :: Reactive r =>
         Event r () -> Frame r (Event r [(Text, Result)])
suite eStep = do
    results <- forM tests $ \te -> do
        eAnswer <- (Just . (teName te,) <$>) <$> teCode te eStep
        hold Nothing eAnswer
    let answers = sequenceA results
        
    return mempty

