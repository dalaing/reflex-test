{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
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

data a :+: b = a :+: b
  deriving (Eq, Ord, Show, Read)

newtype EventFrame a = EventFrame { unEventFrame :: Maybe a }
  deriving (Eq, Ord, Show, Read)

newtype BehaviorFrame a = BehaviorFrame { unBehaviorFrame :: a }
  deriving (Eq, Ord, Show, Read)

class FrameData a where
  type ReflexData t a
  gatherEvents :: Reflex t => Proxy a -> ReflexData t a -> Event t ()
  mkIn :: (Reflex t, MonadHold t m, MonadFix m) => a -> Event t (a, a) -> m (ReflexData t a)
  mkOut :: Reflex t => ReflexData t a -> Event t () -> Event t (a -> a)

instance FrameData (EventFrame a) where
  type ReflexData t (EventFrame a) = Event t a
  gatherEvents  _ e = () <$ e
  mkIn _ = pure . fmapMaybe (unEventFrame . fst)
  mkOut e eOther =
    const . EventFrame <$> leftmost [Just <$> e, Nothing <$ difference eOther e]

instance FrameData (BehaviorFrame a) where
  type ReflexData t (BehaviorFrame a) = Behavior t a
  gatherEvents _ _ = never
  mkIn z = hold (unBehaviorFrame z) . fmap (unBehaviorFrame . snd)
  mkOut b eOther =
    const . BehaviorFrame <$> b <@ eOther

instance (FrameData a, FrameData b) => FrameData (a :+: b) where
  type ReflexData t (a :+: b) = ReflexData t a :+: ReflexData t b
  gatherEvents _ (a :+: b) = leftmost [gatherEvents (Proxy :: Proxy a) a, gatherEvents (Proxy :: Proxy b) b]
  mkIn (za :+: zb) e =
    (:+:) <$>
      mkIn za (fmap (\((a1 :+: _), (a2 :+: _)) -> (a1, a2)) e) <*>
      mkIn zb (fmap (\((_ :+: b1), (_ :+: b2)) -> (b1, b2)) e)
  mkOut (a :+: b) eOther =
    mergeWith (.) [
      (\f (x :+: o) -> f x :+: o) <$> mkOut a eOther
    , (\f (o :+: x) -> o :+: f x) <$> mkOut b eOther
    ]

wrapNetwork :: forall t m a b.
               ( FrameData a
               , FrameData b
               )
            => (ReflexData t a -> BasicGuest t m (ReflexData t b))
            -> a :+: EventFrame ()
            -> b
            -> Event t (a :+: EventFrame (), a :+: EventFrame ())
            -> BasicGuest t m (Event t b)
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
