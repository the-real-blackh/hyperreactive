{-# LANGUAGE OverloadedStrings, RecursiveDo, Rank2Types, FlexibleInstances, FlexibleContexts,
             TupleSections, GADTs, ConstraintKinds, ScopedTypeVariables #-}
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

input3 :: Reactive r =>
          Event r () -> [(Maybe a, Maybe b, Maybe c)] -> Frame r (Event r a, Event r b, Event r c, Event r ())
input3 eStep values = do
    (ep, eEnd) <- input eStep values
    return (filterJust $ c1 <$> ep, filterJust $ c2 <$> ep, filterJust $ c3 <$> ep, eEnd)
  where
    c1 (a, _, _) = a
    c2 (_, b, _) = b
    c3 (_, _, c) = c

merge1 :: Reactive r => Test r
merge1 = Test "merge1" $ \eStep -> do
    (e1, e2, eEnd) <- input2 eStep [
        (Just "hello", Nothing),
        (Nothing,      Just "world")
      ]
    answers <- accum [] ((:) <$> e1 <> e2)
    return $ (\as ->
        if as == ["hello", "world" :: Text] then Pass
                                    else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

-- | Ensure that the merge is left-biased
mergeLeftBias1 :: Reactive r => Test r
mergeLeftBias1 = Test "mergeLeftBias1" $ \eStep -> do
    (e1, e2, e3, eEnd) <- input3 eStep [
        (Nothing,      Nothing,       Just 'A'),  -- select ea
        (Just "left1", Just "right1", Nothing),
        (Nothing,      Nothing,       Just 'B'),  -- select eb
        (Just "right2", Just "left2", Nothing)
      ]
    which <- hold ' ' e3
    let ea = filterJust $ (\b e -> if b == 'A' then Just e else Nothing) <$> which <@> e1 <> e2
        eb = filterJust $ (\b e -> if b == 'B' then Just e else Nothing) <$> which <@> e2 <> e1
    answers <- accum [] ((:) <$> ea <> eb)
    return $ (\as ->
        if as == ["right1", "right2" :: Text] then Pass
                                              else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

filterJust1 :: Reactive r => Test r
filterJust1 = Test "filterJust1" $ \eStep -> do
    (e, eEnd) <- input eStep ([Just "yes", Nothing, Just "no"])
    answers <- accum [] ((:) <$> filterJust e)
    return $ (\as ->
        if as == ["yes", "no" :: Text] then Pass
                               else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

hold1 :: Reactive r => Test r
hold1 = Test "hold1" $ \eStep -> do
    (e1, e2, eEnd) <- input2 eStep [
        (Nothing,           Just ()),
        (Just "banana",     Nothing),
        (Just "cantaloupe", Just ()),
        (Nothing,           Just ())
      ]
    b <- hold "apple" e1
    let eCap = b <@ e2
    answers <- accum [] ((:) <$> eCap)
    return $ (\as ->
        if as == ["apple", "banana", "cantaloupe" :: Text] then Pass
                                                           else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

hold2 :: Reactive r => Test r
hold2 = Test "hold2" $ \eStep -> do
    (e1, e2, e3, eEnd) <- input3 eStep [
        (Nothing,           Nothing,       Just ()),
        (Just "apple",      Just "banana", Nothing),
        (Nothing,           Nothing,       Just ())
      ]
    b <- hold "initial" (e1 <> e2)
    let eCap = b <@ e3
    answers <- accum [] ((:) <$> eCap)
    return $ (\as ->
        if as == ["initial", "banana" :: Text] then Pass
                                               else Fail $ show as
      ) . reverse <$> (answers <@ eEnd)

tests :: (Reactive r) =>
         [Test r]
tests = [
    merge1,
    mergeLeftBias1,
    filterJust1,
    hold1,
    hold2
  ]

suite :: forall r . Reactive r =>
         Event r () -> Frame r (Event r [(Text, Result)])
suite eStep = do
    results <- forM tests $ \te -> do
        eAnswer <- (Just . (teName te,) <$>) <$> teCode te eStep
        hold Nothing eAnswer
    let answers = sequenceA results
    return $ filterJust $ sequenceA <$> answers <@ eStep

