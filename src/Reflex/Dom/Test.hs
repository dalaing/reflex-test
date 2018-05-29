{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Reflex.Dom.Test  where

import Control.Monad (void, forever, forM_)
import Data.Proxy (Proxy(..))

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Control.Lens

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Types (MonadJSM, JSM)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

import Reflex
import Reflex.Dom (mainWidget)
import Reflex.Dom.Core hiding (mainWidget)

import Reflex.Test.Common
import Reflex.Dom.Test.Id
import Reflex.Dom.Test.Text
import Reflex.Dom.Test.Button
import Reflex.Dom.Test.Checkbox
import Reflex.Dom.Test.Widgets

data DomFrame a = DomFrame { unDomFrame :: MaybeT JSM a }

class ReadFrame a where
  type ReadResult a
  readFrame :: a -> JSM (ReadResult a)

instance ReadFrame (DomFrame a) where
  type ReadResult (DomFrame a) = Maybe a
  readFrame (DomFrame x) = runMaybeT x

instance (ReadFrame a, ReadFrame b) => ReadFrame (a :+: b) where
  type ReadResult (a :+: b) = ReadResult a :+: ReadResult b
  readFrame (a :+: b) = (:+:) <$> readFrame a <*> readFrame b

data TestingEnv a b =
  TestingEnv {
      writeIn    :: TMVar a
    , readOut    :: b
    , readResult :: TVar [ReadResult b]
    , done       :: TMVar ()
    , results    :: TMVar [ReadResult b]
    }

newTestingEnv :: b -> STM (TestingEnv a b)
newTestingEnv b =
  TestingEnv <$> newEmptyTMVar <*> pure b <*> newTVar [] <*> newEmptyTMVar <*> newEmptyTMVar

renderHook :: (ReadFrame a, ReadFrame b) => TestingEnv a b -> JSM x -> JSM x
renderHook testEnv h = do
  x <- h

  y <- readFrame (readOut testEnv)
  liftIO . atomically . modifyTVar (readResult testEnv) $ (y :)

  z <- liftIO . atomically . tryTakeTMVar $ writeIn testEnv
  case z of
    Just z' -> void $ readFrame z'
    Nothing ->  do
      d <- liftIO . atomically . tryTakeTMVar $ done testEnv
      case d of
        Nothing -> pure ()
        Just d' -> liftIO . atomically $ do
          res <- readTVar (readResult testEnv)
          putTMVar (results testEnv) (reverse res)

  pure x

testWidget :: (ReadFrame a, ReadFrame b, Show (ReadResult b))
           => [a]
           -> b
           -> (forall x. Widget x ())
           -> IO [ReadResult b]
testWidget inputs outputs w = do
  testEnv <- atomically $ newTestingEnv outputs

  tId <- forkIO $ mainWidget $ withRenderHook (renderHook testEnv) w

  forM_ inputs $ atomically . putTMVar (writeIn testEnv)
  atomically . putTMVar (done testEnv) $ ()
  xs <- atomically . takeTMVar $ results testEnv

  -- only printing here because we can't kill the main widget once we are done
  print xs
  killThread tId

  pure xs

-- making this into not-an-orphan would be good
instance HasDocument JSM where
  askDocument = currentDocumentUnchecked

example :: MonadWidget t m => m ()
example = do
  eClick <- buttonWithId "count-button" "Count"
  dCount <- count eClick
  displayDivWithId "count" dCount

  eToggle <- buttonWithId "toggle-button" "Toggle"
  dToggle <- toggle False eToggle
  displayDivWithId "toggle" dToggle

  eCheckTrue <- buttonWithId "check-true-button" "Check true"
  eCheckFalse <- buttonWithId "check-true-button" "Check false"
  cb <- checkbox False $ def & attributes .~ pure ("id" =: "check-cb")
                             & setValue .~ leftmost [True <$ eCheckTrue, False <$ eCheckFalse]

  displayDivWithId "check-d" (value cb)

  d <- holdDyn False $ view checkbox_change cb
  displayDivWithId "check-e" d

  pure ()

-- Output (due to printing) is:
-- [ Just 0 :+: Just False
-- , Just 1 :+: Just False
-- , Just 2 :+: Just False
-- , Just 2 :+: Just True
-- , Just 3 :+: Just True
-- ]
  -- [(Just 0 :+: Just False) :+: Just False
  -- ,(Just 1 :+: Just False) :+: Just False
  -- ,(Just 2 :+: Just False) :+: Just False
  -- ,(Just 2 :+: Just True) :+: Just False
  -- ,(Just 3 :+: Just True) :+: Just False
  -- ,(Just 3 :+: Just True) :+: Just True
  -- had to click something to keep it all ticking over
  -- ,(Just 4 :+: Just True) :+: Just True
  -- ,(Just 4 :+: Just True) :+: Just False
  -- ,(Just 4 :+: Just True) :+: Just True
  -- ,(Just 4 :+: Just True) :+: Just False
  -- ]

runExample :: IO [ReadResult (DomFrame Int :+: DomFrame Bool :+: DomFrame Bool :+: DomFrame Bool :+: DomFrame Bool)]
runExample = do
  let
    clickCount =
      idElement "count-button" >>= clickButton
    clickToggle =
      idElement "toggle-button" >>= clickButton
    setCheck b =
      idElement "check-cb" >>= setChecked b
    toggleCheck =
      idElement "check-cb" >>= toggleChecked

    inputs = fmap DomFrame
      [ clickCount
      , clickCount
      , clickToggle
      , clickCount
      , setCheck True
      , setCheck True
      , setCheck False
      , toggleCheck
      , toggleCheck
      ]

    readCount =
      idElement "count" >>= readText (Proxy :: Proxy Int)
    readToggle =
      idElement "toggle" >>= readText (Proxy :: Proxy Bool)
    readCheck1 =
      idElement "check-cb" >>= getChecked
    readCheck2 =
      idElement "check-d" >>= readText (Proxy :: Proxy Bool)
    readCheck3 =
      idElement "check-e" >>= readText (Proxy :: Proxy Bool)

    outputs = DomFrame readCount :+: DomFrame readToggle :+: DomFrame readCheck1 :+: DomFrame readCheck2 :+: DomFrame readCheck3

  testWidget inputs outputs example
