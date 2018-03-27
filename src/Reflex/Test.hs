{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Test where

import Data.Proxy (Proxy(..))
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Foldable (traverse_)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadIO, liftIO)

import Reflex
import Reflex.Host.Class

import Reflex.Host.Basic

import Reflex.Test.Common

wrapNetwork :: forall t m a b.
               ( FrameData a
               , FrameData b
               , Reflex t
               , MonadFix m
               , MonadHold t m
               )
            => (ReflexData t a -> m (ReflexData t b))
            -> a :+: EventFrame ()
            -> b
            -> Event t (a :+: EventFrame (), a :+: EventFrame ())
            -> m (Event t b)
wrapNetwork fn initialIn initialOut inputs = do
  input <- mkIn initialIn inputs
  let f (a :+: e) = do
        b <- fn a
        pure (b, leftmost [e, gatherEvents (Proxy :: Proxy a) a, gatherEvents (Proxy :: Proxy b) b])
  (output, eAll) <- f input
  out <- foldDyn ($) initialOut $ mkOut output eAll
  pure $ updated out

testNetwork :: forall a b.
               ( FrameData a
               , FrameData b
               )
            => (forall t m. ReflexData t a -> BasicGuest t m (ReflexData t b))
            -> a
            -> b
            -> [a]
            -> IO [b]
testNetwork fn initialIn initialOut inputs = do
  let
    initialIn' = initialIn :+: EventFrame (Just ())
    inputs' = fmap (:+: EventFrame (Just ())) inputs

  outRef <- atomically newEmptyTMVar

  (fIn, fQuit) :: ((a :+: EventFrame (), a :+: EventFrame ()) -> IO (), () -> IO ()) <- basicHostWithQuit $ do
    (eIn, fireIn) <- newTriggerEvent
    (eQuit, fireQuit) <- newTriggerEvent
    eOut <- wrapNetwork fn initialIn' initialOut eIn
    dOut <- foldDyn (:) [] eOut
    performEvent $ liftIO . atomically . putTMVar outRef . reverse <$> current dOut <@ eQuit
    pure ((fireIn, fireQuit), eQuit)

  _ <- forkIO $ do
    -- this is dodgy / not quite right
    traverse_ fIn $ zip (initialIn' : inputs') (inputs' ++ [initialIn'])
    fQuit ()

  atomically . takeTMVar $ outRef

testMe :: (Reflex t, MonadHold t m, MonadFix m)
       => Event t () :+: Behavior t Int
       -> m (Event t () :+: Event t () :+: Behavior t Int :+: Behavior t Int)
testMe (e :+: b) = do
  dCount <- count e
  let
    e1 = gate (current $ even <$> dCount) e
    e2 = gate (current $ odd <$> dCount) e
    b1 = (* 5) <$> b
    b2 = current dCount
  pure (e1 :+: e2 :+: b1 :+: b2)

in1 :: EventFrame () :+: BehaviorFrame Int
in1 = EventFrame Nothing :+: BehaviorFrame 0

out1 :: EventFrame () :+: EventFrame () :+: BehaviorFrame Int :+: BehaviorFrame Int
out1 = EventFrame Nothing :+: EventFrame Nothing :+: BehaviorFrame 0 :+: BehaviorFrame 0

inputs :: [EventFrame () :+: BehaviorFrame Int]
inputs = [ EventFrame Nothing   :+: BehaviorFrame 1
         , EventFrame Nothing   :+: BehaviorFrame 2
         , EventFrame (Just ()) :+: BehaviorFrame 3
         , EventFrame (Just ()) :+: BehaviorFrame 4
         , EventFrame (Just ()) :+: BehaviorFrame 5
         , EventFrame (Just ()) :+: BehaviorFrame 6
         , EventFrame Nothing   :+: BehaviorFrame 7
         , EventFrame Nothing   :+: BehaviorFrame 8
         ]
