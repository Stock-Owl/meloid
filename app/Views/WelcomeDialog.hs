{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.WelcomeDialog where

import Brick
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Common
import Data.Maybe (fromJust)
import Lens.Micro ((^?))
import Lens.Micro.Mtl
import Text.RawString.QQ (r)
import Widgets

draw :: St -> Widget FullName
draw st =
  C.center $
    W.withAttr (attrName "dialog") $
      W.padAll 2 . W.vBox $
        [ C.hCenter $ W.str "Welcome to Gaze Player",
          C.hCenter (pageWidget),
          W.padTop (W.Pad 2) $
            W.hBox $
              [ skipButton,
                padLeft Max . W.hBox $
                  [ prevButton,
                    padLeft (Pad 1) $ nextOrFinish
                  ]
              ]
        ]
  where
    -- SAFETY: dsPage is not Nothing while WelcomeDialog is open
    page = fromJust $ st ^? stDialog .? dsPage
    prevButton
      | page > 1 = button st (WelcomeDialog, PrevButton) "   PREV   "
      | otherwise = W.emptyWidget

    skipButton
      | page < 3 = button st (WelcomeDialog, SkipButton) "   SKIP   "
      | otherwise = W.emptyWidget

    nextOrFinish
      | page < 3 = button st (WelcomeDialog, NextButton) "   NEXT   "
      | otherwise = button st (WelcomeDialog, FinishButton) "   FINISH   "

    pageWidget
      | page == 1 =
          W.str
            [r|
This is a simple video player made in Haskell.
It aims to be fast, simple, and easy to use.  
|]
      | page == 2 =
          W.str
            [r|
This is the next page.
|]
      | otherwise = W.str "\nUnknown page"

handleMouseUp :: WidgetName -> EventM FullName St ()
handleMouseUp = \case
  PrevButton -> stDialog .? dsPage %= (subtract 1)
  NextButton -> stDialog .? dsPage %= (+ 1)
  SkipButton -> do closeDialog; stDialog .? dsPage .= 1
  FinishButton -> do closeDialog; stDialog .? dsPage .= 1
  _ -> return ()