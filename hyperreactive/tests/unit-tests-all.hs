{-# LANGUAGE OverloadedStrings #-}
import UnitTests
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Reactive.Hyper.Sodium as S
import qualified Reactive.Hyper.Banana as B


main = do
    a_sodium <- do
        (eStep, sendStep) <- S.sync S.newEvent
        eAnswers <- S.sync $ suite eStep
        answersRef <- newIORef Nothing
        kill <- S.sync $ S.listen eAnswers (writeIORef answersRef . Just) 
        let run = do
                S.sync $ sendStep ()
                mAnswers <- readIORef answersRef
                case mAnswers of
                     Just answers -> return answers 
                     Nothing      -> run
        answers <- run
        kill
        return $ M.fromList answers

    a_banana <- do
        handlerRef <- newIORef Nothing :: IO (IORef (Maybe (() -> IO ())))
        answersRef <- newIORef Nothing
        en <- B.compile $ do
            eStep <- B.fromAddHandler $ B.AddHandler $ \handler -> do
                writeIORef handlerRef (Just handler)
                return $ writeIORef handlerRef Nothing
            eAnswers <- suite eStep
            B.reactimate eAnswers (writeIORef answersRef . Just)
        B.actuate en
        let run = do
                Just handler <- readIORef handlerRef
                handler ()
                mAnswers <- readIORef answersRef
                case mAnswers of
                     Just answers -> return answers 
                     Nothing      -> run
        run
        Just answers <- readIORef answersRef
        return $ M.fromList answers

    let results = [
              ("sodium", a_sodium),
              ("banana", a_banana)
          ]

        testNames = S.toList $ foldl1' S.union (map (S.fromList . M.keys . snd) results)

    let formatColumn width text = text ++ replicate (max 1 $ width - length text) ' '
        colWid1 = 20
        colWid = 30

    putStr $ formatColumn colWid1 ""
    forM_ (map fst results) $ \name -> putStr $ formatColumn colWid name
    putStrLn ""
    forM_ testNames $ \testName -> do
        putStr $ formatColumn colWid1 (T.unpack testName)
        forM_ (map snd results) $ \m ->
            putStr $ formatColumn colWid $ maybe "" show $ M.lookup testName m
        putStrLn ""

