{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.MainView where

import Brick
import Brick qualified as B
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Common
import Control.Monad (when)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V
import Lens.Micro ((<&>), (^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Widgets

draw :: St -> Widget FullName
draw st =
  W.vBox
    [ W.hBox
        [ drawControlPanel st,
          W.padLeft (W.Pad 2) . W.padRight (W.Pad 1) $ drawSongPanel st,
          Widgets.image st (MainView, AlbumArtPlaying) albumArtPlayingSize
        ],
      W.padTop (W.Pad 1) $ drawAllAlbumList st
    ]

drawAllAlbumList :: St -> Widget FullName
drawAllAlbumList st =
  albumList st (MainView, AllAlbumList) AllAlbumListEntry (st ^. stConfig . csAllAlbums)

drawSongPanel :: St -> Widget FullName
drawSongPanel st =
  W.vBox
    [ W.hBox
        [ W.padRight (W.Max) $ withAttr (attrName "header") $ strClippedWithEllipsis title,
          W.padLeft (W.Max) $ withAttr (attrName "meta") $ strClippedWithEllipsis ("by " <> artist)
        ],
      strClippedWithEllipsis album,
      songProgressBar st (MainView, SongProgressBar)
    ]
  where
    title = NonEmpty.head $ st ^. stCurrentSongMeta MPD.Title
    artist = concat . NonEmpty.intersperse ", " $ st ^. stCurrentSongMeta MPD.Artist
    album = concat . NonEmpty.intersperse " - " $ st ^. stCurrentSongMeta MPD.Album

drawControlPanel :: St -> Widget FullName
drawControlPanel st =
  W.hLimit 24 $
    W.hBox
      [ W.vBox
          [ iconButton st (MainView, IncreaseVolumeButton) " + ",
            iconButton st (MainView, DecreaseVolumeButton) " - "
          ],
        W.vBox
          [ W.str $ "Time " <> formatSecs (floor elapsed) <> "/" <> formatSecs (floor total),
            W.hBox
              [ W.str $ "Vol  " <> show (st ^. stConfig . csVolume) <> "%",
                W.padLeft (W.Max) $ iconButton st (MainView, RewindButton) "<<",
                W.padLeft (W.Pad 1) $ iconButton st (MainView, PlayButton) "||",
                W.padLeft (W.Pad 1) $ iconButton st (MainView, ForwardButton) ">>"
              ],
            volumeBar st (MainView, VolumeBar)
          ]
      ]
  where
    (elapsed, total) = fromMaybe (0, 0) $ st ^. stPlaying . psCurrentTime

    formatSecs :: Integer -> String
    formatSecs totalSecs = show mins ++ ":" ++ ensureTwoDigits secs
      where
        (mins, secs) = totalSecs `divMod` 60
        ensureTwoDigits n = if n < 10 then "0" ++ show n else show n

handleMouseDown :: Location -> WidgetName -> EventM FullName St ()
handleMouseDown (Location (ax, ay)) VolumeBar = when (ay == 0) $ do
  let volume =
        max 0 . min 100 $
          if volumeBarWidth <= 1
            then 100
            else (ax * 100) `div` (volumeBarWidth - 1)
  stConfig . csVolume .= fromIntegral volume
  sendRequest $ MPDOperation [MPD.setVolume (fromIntegral volume) <&> pure]
handleMouseDown (Location (ax, ay)) (ScrollBar name) =
  when (ax == 0) $
    B.lookupViewport name >>= \case
      Nothing ->
        pure ()
      Just viewport' -> do
        let visibleHeight = V.regionHeight $ viewport' ^. B.vpSize
            totalHeight = V.regionHeight $ viewport' ^. B.vpContentSize
            currentTop = viewport' ^. B.vpTop
            thumbHeight = scrollbarThumbHeight visibleHeight totalHeight
            maxThumbTop = max 0 (visibleHeight - thumbHeight)
            clickThumbTop =
              min maxThumbTop $
                max 0 $
                  ay - thumbHeight `div` 2
            targetTop = thumbTopToScrollTop visibleHeight totalHeight clickThumbTop
            delta = targetTop - currentTop
        stBars %= Map.insert name (targetTop, totalHeight)
        M.vScrollBy (B.viewportScroll name) delta
handleMouseDown _ _ =
  pure ()

handleMouseUp :: WidgetName -> EventM FullName St ()
handleMouseUp IncreaseVolumeButton = stConfig . csVolume += 1
handleMouseUp DecreaseVolumeButton = stConfig . csVolume -= 1
handleMouseUp _ = pure ()

scrollbarThumbHeight :: Int -> Int -> Int
scrollbarThumbHeight visibleHeight totalHeight
  | visibleHeight <= 0 = 0
  | totalHeight <= visibleHeight = visibleHeight
  | otherwise =
      max 1 $
        min visibleHeight $
          ceilingDiv (visibleHeight * visibleHeight) totalHeight

thumbTopToScrollTop :: Int -> Int -> Int -> Int
thumbTopToScrollTop visibleHeight totalHeight thumbTop
  | visibleHeight <= 0 = 0
  | totalHeight <= visibleHeight = 0
  | otherwise =
      min maxScrollTop $
        max 0 $
          (maxScrollTop * clampedThumbTop) `div` maxThumbTop
  where
    thumbHeight = scrollbarThumbHeight visibleHeight totalHeight
    maxThumbTop = max 1 (visibleHeight - thumbHeight)
    maxScrollTop = totalHeight - visibleHeight
    clampedThumbTop = min (visibleHeight - thumbHeight) $ max 0 thumbTop
