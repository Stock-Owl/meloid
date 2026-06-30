module Widgets where

import Brick hiding (image)
import Brick.Widgets.Core qualified as W
import Common
import Compat.Image qualified as Image
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vec
import Lens.Micro (to, (&), (^.))

button :: St -> FullName -> String -> Widget FullName
button st name label =
  W.clickable name $
    W.withAttr (attrName "button" <> isFocused') $
      W.str label
  where
    isFocused'
      | st ^. stPressed == Just name = attrName "pressed"
      | otherwise = mempty

iconButton :: St -> FullName -> String -> Widget FullName
iconButton st name label =
  W.clickable name $
    W.withAttr (attrName "iconButton" <> isFocused') $
      W.str label
  where
    isFocused'
      | st ^. stPressed == Just name = attrName "pressed"
      | otherwise = mempty

generalButton :: St -> FullName -> Widget FullName -> Widget FullName
generalButton st name inner =
  W.clickable name . isFocused' $ inner
  where
    isFocused'
      | st ^. stPressed == Just name = W.withDefAttr (attrName "focused")
      | otherwise = id

imageSlot :: FullName -> ImageSize -> Image.ImageSlot FullName
imageSlot = Image.slot

image :: St -> FullName -> ImageSize -> Widget FullName
image st name size =
  case lookupRenderedImage st name size of
    Just (InlineSymbols art) ->
      Image.inlineWidget (imageSlot name size) art
    _ ->
      Image.placeholderWidget (imageSlot name size)

viewportWithBar :: St -> FullName -> Widget FullName -> Widget FullName
viewportWithBar st name inner =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let height = max 0 $ ctx ^. availHeightL
        (scrollTop, total) = fromMaybe (0, 0) $ st ^. stBars . to (Map.!? name)
        thumb = scrollbarThumb height total scrollTop
    render $
      W.hBox
        [ W.clickable (fst name, ScrollBar name) $ W.vBox $ fmap drawTrackCell thumb,
          W.viewport name Vertical inner
        ]
  where
    drawTrackCell True =
      W.withAttr (attrName "scrollBarThumb") $
        W.str "│"
    drawTrackCell False =
      W.withAttr (attrName "scrollBarTrack") $
        W.str " "

scrollbarThumb :: Int -> Int -> Int -> [Bool]
scrollbarThumb height total scrollTop
  | height <= 0 = []
  | total <= 0 = replicate height True
  | total <= height = replicate height True
  | otherwise =
      [ i >= thumbTop && i < thumbTop + thumbHeight
      | i <- [0 .. height - 1]
      ]
  where
    thumbHeight =
      max 1 $
        min height $
          ceilingDiv (height * height) total
    maxThumbTop = height - thumbHeight
    maxScrollTop = max 1 (total - height)
    thumbTop =
      min maxThumbTop $
        max 0 $
          (maxThumbTop * max 0 scrollTop) `div` maxScrollTop

albumList :: St -> FullName -> (Int -> WidgetName) -> Vec.Vector Album -> Widget FullName
albumList st name@(vName, _) wName albums =
  W.reportExtent name $
    viewportWithBar st name . W.vBox $
      Vec.toList $
        Vec.imap draw albums
  where
    draw i album =
      W.vLimit (snd albumArtThumbSize) $
        W.hBox
          [ image st (MainView, AlbumArtThumb i) albumArtThumbSize,
            W.padLeft (W.Pad 1) . W.vBox $
              [ generalButton st (vName, wName i) $
                  W.withAttr (attrName "header") $
                    strClippedWithEllipsis (albumName album),
                W.withAttr (attrName "meta") $
                  strClippedWithEllipsis ("by " <> albumArtistsLine album),
                W.withAttr (attrName "text") $
                  strClippedWithEllipsis (albumReleaseDate album)
              ]
          ]

    albumArtistsLine album =
      case albumArtists album of
        [] -> "Unknown Artist"
        artists -> intercalate ", " artists

volumeBarWidth :: Int
volumeBarWidth = 21

volumeBar :: (Ord n) => St -> n -> Widget n
volumeBar st name =
  W.clickable name $
    W.withAttr (attrName "progressBarIncomplete") $
      W.hLimit width $
        W.withAttr (attrName "progressBarComplete") $
          W.reportExtent name . W.str $
            makeBar width (st ^. stConfig . csVolume & fromIntegral) 100
  where
    width = volumeBarWidth

songProgressBar :: (Ord n) => St -> n -> Widget n
songProgressBar st _ =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx ^. availWidthL
        (current, total) = fromMaybe (0, 0) $ st ^. stPlaying . psCurrentTime
        bar = makeBar' width (floor current) (floor total)
        (filled, rest) = span (/= ' ') bar
    render $
      W.hBox
        [ W.withAttr (attrName "progressBarComplete") $ W.str filled,
          W.withAttr (attrName "progressBarIncomplete") $ W.str rest
        ]

strClippedWithEllipsis :: String -> Widget n
strClippedWithEllipsis s =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx ^. availWidthL
    render . W.str $
      if width <= 0
        then ""
        else
          if length s > width && width > 3
            then take (width - 3) s <> "..."
            else take width s

makeBar :: Int -> Integer -> Integer -> String
makeBar = makeBarWith 8 '█' "▏▎▍▌▋▊▉"

makeBar' :: Int -> Integer -> Integer -> String
makeBar' = makeBarWith 8 '⣿' "⠁⠃⠇⡇⡏⡟⡿"

makeBarWith :: Int -> Char -> String -> Int -> Integer -> Integer -> String
makeBarWith steps fullChar partialChars fullWidth count total
  | fullWidth <= 0 = ""
  | total <= 0 = blank
  | clampedCount <= 0 = blank
  | otherwise = prefix <> replicate (fullWidth - length prefix) ' '
  where
    blank = replicate fullWidth ' '
    fullWidth' = toInteger fullWidth
    steps' = toInteger steps
    clampedCount = max 0 (min count total)
    scaled = (clampedCount * fullWidth' * steps' + total - 1) `div` total
    fullCells = fromInteger $ scaled `div` steps'
    remainder = fromInteger $ scaled `mod` steps'
    partial
      | remainder == 0 = ""
      | otherwise = [partialChars !! (remainder - 1)]
    prefix = replicate fullCells fullChar <> partial

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator = (numerator + denominator - 1) `div` denominator
