module Views.SimpleDialog where

import Brick
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Common
import Lens.Micro ((^.))
import Widgets

draw :: St -> Widget FullName
draw st =
  C.center $
    W.withAttr (attrName "dialog") $
      W.padAll 2 . W.vBox $
        [ C.hCenter $ W.str "Welcome to Gaze Player",
          C.hCenter $ W.strWrap content,
          W.padTop (W.Pad 2) . W.padLeft (W.Max) $ okButton
        ]
  where
    okButton = button st (SimpleDialog, OkButton) "    OK    "
    content = st ^. stDialog .? dsText

handleMouseUp :: WidgetName -> EventM FullName St ()
handleMouseUp OkButton = closeDialog
handleMouseUp _ = return ()