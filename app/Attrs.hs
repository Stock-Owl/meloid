module Attrs where

import Brick
import Brick.Themes qualified as T
import Graphics.Vty

a :: String -> AttrName
a = attrName

primary :: Color
primary = white

secondary :: Color
secondary = hex2RGB 0x6F6F6F

accent :: Color
accent = hex2RGB 0xCCBBCC

defaultTheme :: T.Theme
defaultTheme =
  T.newTheme
    (fg primary)
    [ (a "button", currentAttr `withForeColor` primary `withStyle` underline),
      (a "iconButton", currentAttr `withForeColor` primary `withStyle` bold),
      (a "button" <> a "pressed", black `on` primary),
      (a "iconButton" <> a "pressed", black `on` accent),
      (a "focused", white `on` secondary),
      (a "dialog", primary `on` secondary),
      (a "header", currentAttr `withForeColor` primary `withStyle` bold),
      (a "meta", currentAttr `withForeColor` accent `withStyle` italic),
      (a "text", currentAttr `withForeColor` accent),
      (a "scrollBarThumb", accent `on` secondary),
      (a "scrollBarTrack", currentAttr `withBackColor` secondary),
      (a "progressBarIncomplete", black `on` secondary),
      (a "progressBarComplete", primary `on` secondary),
      -- Log
      (a "debugLog", fg $ hex2RGB 0xAAAAAA),
      (a "infoLog", fg $ white),
      (a "warnLog", fg $ hex2RGB 0xFFA500),
      (a "errorLog", fg $ red)
    ]

hex2RGB :: Int -> Color
hex2RGB i =
  let r = i `mod` 256
      g = (i `div` 256) `mod` 256
      b = i `div` 256 `div` 256
   in srgbColor r g b
