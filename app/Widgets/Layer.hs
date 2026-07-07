{-# LANGUAGE LambdaCase #-}

{- | Generic layer and strict extent helpers.
The layer list is the single source of truth for top-level
rendering order. Strict extents add occlusion awareness on top
of Brick's raw reported extents.
-}
module Widgets.Layer (
  LayerName (..),
  StrictExtent (..),
  activeLayerNames,
  activeOccluderNames,
  lookupStrictExtent,
) where

import Brick.Main qualified as M
import Brick.Types (EventM, Extent, Location (..), extentSize, extentUpperLeft)
import Brick.Widgets.Center qualified as C
import Data.Maybe (catMaybes, mapMaybe)
import Lens.Micro ((^.))
import Types
import Widgets.Lists (drawMenuLayer)
import Widgets.Views (drawDialogView, drawView)

data LayerName
  = ViewLayer ViewName
  | DialogLayer ViewName
  | MenuLayer

data StrictExtent n = StrictExtent
  { strictRawExtent :: Extent n
  , strictOccluders :: [Extent n]
  }

instance Drawable St LayerName where
  draw (ViewLayer view) st = drawView view st
  draw (DialogLayer view) st = C.centerLayer $ drawDialogView view st
  draw MenuLayer st = drawMenuLayer st
  willReportExtent (DialogLayer _) = True
  willReportExtent MenuLayer = True
  willReportExtent _ = False
  layerSurface layer@(DialogLayer _) = Just (mName layer)
  layerSurface layer@MenuLayer = Just (mName layer)
  layerSurface _ = Nothing
  variant (ViewLayer view) = viewIndex view
  variant (DialogLayer view) = 100 + viewIndex view
  variant MenuLayer = 200

-- | Top-level layers in Brick's topmost-first order.
activeLayerNames :: St -> [MName St]
activeLayerNames st =
  menuLayer
    <> maybe [] (pure . mName . DialogLayer) (st ^. stDialogView)
    <> maybe [] (pure . mName . ViewLayer) (st ^. stCurrentView)
 where
  menuLayer = case st ^. stMenu of
    Just _ -> [mName MenuLayer]
    Nothing -> []

-- | Extent-reporting widgets that cover lower layers.
activeOccluderNames :: St -> [MName St]
activeOccluderNames =
  mapMaybe (named layerSurface) . activeLayerNames

lookupStrictExtent :: (Eq n) => n -> [n] -> EventM n s (Maybe (StrictExtent n))
lookupStrictExtent name occluderNames =
  M.lookupExtent name >>= \case
    Nothing ->
      pure Nothing
    Just rawExtent -> case extentBox rawExtent of
      Nothing ->
        pure Nothing
      Just rawBox -> do
        occluders <- catMaybes <$> traverse M.lookupExtent occluderNames
        pure . Just $
          StrictExtent
            rawExtent
            [ occluder
            | occluder <- occluders
            , Just occluderBox <- [extentBox occluder]
            , boxesIntersect rawBox occluderBox
            ]
 where
  extentBox extent
    | w <= 0 || h <= 0 = Nothing
    | otherwise = Just (x, y, w, h)
   where
    Location (x, y) = extentUpperLeft extent
    (w, h) = extentSize extent

  boxesIntersect (ax, ay, aw, ah) (bx, by, bw, bh) =
    ax < bx + bw
      && bx < ax + aw
      && ay < by + bh
      && by < ay + ah

viewIndex :: ViewName -> Int
viewIndex MainView = 0
viewIndex DebugView = 1
viewIndex WelcomeDialog = 2
viewIndex SimpleDialog = 3
