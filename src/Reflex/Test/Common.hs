{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Test.Common where

import Control.Monad.Fix (MonadFix)
import Data.Proxy (Proxy(..))

import Reflex

infixr 4 :+:

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
