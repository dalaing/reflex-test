{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Test.Class (
    classElementsSingle
  , classElementsMultiple
  , classElementsIx
  ) where

import Control.Monad (forM)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Text (Text)

import Reflex.Dom.Core (HasDocument(..))

import qualified GHCJS.DOM.Document as Document (getElementsByClassName)
import GHCJS.DOM.Element (Element)
import qualified GHCJS.DOM.Element as Element (getElementsByClassName)
import GHCJS.DOM.HTMLCollection (HTMLCollection(..), getLength, item)
import GHCJS.DOM.Types (MonadJSM)

classElements ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  m (Word, HTMLCollection)
classElements Nothing eclass = do
  doc <- askDocument
  c <- Document.getElementsByClassName doc eclass
  l <- getLength c
  pure (l, c)
classElements (Just e) eclass = do
  c <- Element.getElementsByClassName e eclass
  l <- getLength c
  pure (l, c)

classElementsSingle ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  MaybeT m Element
classElementsSingle mParent eclass = do
  (l, c) <- lift $ classElements mParent eclass
  MaybeT $
    if (l /= 1)
    then pure Nothing
    else item c 0

classElementsMultiple ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  MaybeT m [Element]
classElementsMultiple mParent eclass = do
  (l, c) <- lift $ classElements mParent eclass
  if (l == 0)
  then pure []
  else forM [0..l-1] $ MaybeT . item c

classElementsIx ::
  ( MonadJSM m
  , HasDocument m
  ) =>
  Maybe Element ->
  Text ->
  Word ->
  MaybeT m Element
classElementsIx mParent eclass i = do
  (l, c) <- lift $ classElements mParent eclass
  MaybeT $
    if (i < 0 || l <= i)
    then pure Nothing
    else item c i
