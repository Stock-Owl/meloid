{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Attrs (defaultTheme)
import Brick qualified as B
import Brick.BChan
import Brick.Main as M
import Brick.Themes qualified as T
import Brick.Types
  ( BrickEvent (..),
    EventM,
    Widget,
  )
import Brick.Widgets.Core qualified as W
import Common
import Compat.Image qualified as Image
import Compat.Term qualified as Term
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execState)
import Data.Foldable (for_, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (^.), (^?), _2)
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import Sys qualified
import Views.MainView qualified as MainView
import Views.SimpleDialog qualified as SimpleDialog
import Views.WelcomeDialog qualified as WelcomeDialog

drawUI :: St -> [Widget FullName]
drawUI st =
  maybe [] (pure . drawDialog) (st ^? stDialog .? dsCurrent)
    <> [drawView (st ^. stCurrentView)]
  where
    drawDialog WelcomeDialog = WelcomeDialog.draw st
    drawDialog SimpleDialog = SimpleDialog.draw st
    drawDialog _ = W.emptyWidget

    drawView MainView = MainView.draw st
    drawView DebugView =
      W.viewport (DebugView, DebugViewport) B.Vertical $
        W.vBox $
          W.str "Debug view\n\n"
            : reverse
              [ W.withAttr (B.attrName style) $ W.strWrap msg
              | (logLevel, msg) <- st ^. stLogs,
                let style = case logLevel of
                      Debug -> "debugLog"
                      Info -> "infoLog"
                      Warn -> "warnLog"
                      Error -> "errorLog"
              ]
    drawView _ = W.emptyWidget

handleEvent :: BChan Event -> BrickEvent FullName Event -> EventM FullName St ()
handleEvent chan = \case
  VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
    M.halt
  VtyEvent (V.EvResize _ _) ->
    queueMainViewRefresh
  VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) ->
    (,) <$> use stCurrentView <*> use stPanic >>= \case
      (DebugView, False) ->
        switchViewAndSyncImages =<< use stLastView
      (DebugView, True) ->
        M.continueWithoutRedraw
      _ ->
        switchViewAndSyncImages DebugView
  MouseDown name V.BScrollDown _ _ -> do
    let scrollName = name & _2 %~ parentScrollable
    scrollBy scrollName (scrollStep scrollName)
    queueMainViewRefresh
  MouseDown name V.BScrollUp _ _ -> do
    let scrollName = name & _2 %~ parentScrollable
    scrollBy scrollName (negate $ scrollStep scrollName)
    queueMainViewRefresh
  MouseDown name@(vName, wName) V.BLeft _ location -> do
    stPressed .= Just name
    case vName of
      MainView -> MainView.handleMouseDown location wName >> queueMainViewRefresh
      _ -> pure ()
  MouseUp (vName, wName) (Just V.BLeft) _ -> do
    case vName of
      WelcomeDialog -> WelcomeDialog.handleMouseUp wName
      SimpleDialog -> SimpleDialog.handleMouseUp wName
      MainView -> MainView.handleMouseUp wName >> queueMainViewRefresh
      _ -> pure ()
    stPressed .= Nothing
  AppEvent (Log entry) -> do
    when (fst entry == Error) $
      panic >> switchViewAndSyncImages DebugView
    stLogs %= (entry :)
  AppEvent RefreshImages ->
    whenMainView $ do
      vty <- M.getVtyHandle
      Image.refreshScene (V.outputIface vty)
  AppEvent event ->
    case event of
      UpdateStatus status ->
        stConfig . csVolume ?.= MPD.stVolume status
      UpdateSong song -> do
        stPlaying . psCurrentSong .= song
        use stCurrentAlbum >>= mapM_ Image.ensureAlbumArtRequested
        queueMainViewRefresh
      UpdateTime dur ->
        stPlaying . psCurrentTime .= dur
      UpdateConfig config -> do
        stConfig .= config
        format <- use (stEnv . envImageFormat)
        case format of
          Term.Symbols ->
            mapM_ Image.ensureAlbumArtRequested (toList $ config ^. csAllAlbums)
          _ ->
            pure ()
        use stCurrentAlbum >>= mapM_ Image.ensureAlbumArtRequested
        queueMainViewRefresh
      LoadAlbumArt pair -> do
        stPicCache %= uncurry Map.insert pair
        stPicPending %= Set.delete (fst pair)
        queueMainViewRefresh
  _ ->
    pure ()
  where
    field ?.= maybeValue = for_ maybeValue $ \value -> field .= value
    infix 4 ?.=

    whenMainView action = do
      currentView <- use stCurrentView
      when (currentView == MainView) action

    queueRefreshIfOutOfBand = do
      format <- use (stEnv . envImageFormat)
      when (Term.isOutOfBandFormat format) $
        Image.queueRefreshImages chan

    queueMainViewRefresh =
      whenMainView queueRefreshIfOutOfBand

    switchViewAndSyncImages nextView = do
      previousView <- use stCurrentView
      switchView nextView
      currentView <- use stCurrentView
      vty <- M.getVtyHandle
      when (previousView == MainView && currentView /= MainView) $
        Image.clearScene (V.outputIface vty)
      when (currentView == MainView) $
        queueRefreshIfOutOfBand

    scrollStep (_, AllAlbumList) = snd albumArtThumbSize
    scrollStep _ = 1

    scrollBy :: FullName -> Int -> EventM FullName St ()
    scrollBy name delta = do
      viewport <- B.lookupViewport name
      let nextBar = do
            vp <- viewport
            let top = vp ^. B.vpTop
                total = V.regionHeight $ vp ^. B.vpContentSize
                visible = V.regionHeight $ vp ^. B.vpSize
                nextTop = min (max 0 $ total - visible) $ max 0 (top + delta)
            pure (nextTop, total)
      maybe (pure ()) ((stBars %=) . Map.insert name) nextBar
      M.vScrollBy (B.viewportScroll name) delta

handleStartEvent :: EventM FullName St ()
handleStartEvent = do
  sendRequest SignalInit
  sendRequest GetConfig
  termType <- liftIO Term.deduceTerminalType
  stEnv .= Environment termType (Term.deduceFormat termType)

  sendRequest . LogConfig Info $
    "Terminal environment: \n"
      <> "- Terminal type: "
      <> show termType
      <> "\n"
      <> "- Image format: "
      <> show (Term.deduceFormat termType)

app :: BChan Event -> B.AttrMap -> M.App St Event FullName
app chan attrMap =
  M.App
    { M.appDraw = drawUI,
      M.appStartEvent = do
        vty <- M.getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True
        handleStartEvent,
      M.appChooseCursor = M.showFirstCursor,
      M.appAttrMap = const attrMap,
      M.appHandleEvent = handleEvent chan
    }

main :: IO ()
main = do
  chan <- newBChan 2048
  requestChan <- newBChan 2048
  void $ forkIO $ Sys.musicPlayerThread requestChan chan
  let st = flip execState defaultSt $ do
        stChannel .= Just requestChan
  void $
    M.customMainWithDefaultVty
      (Just chan)
      (app chan (T.themeToAttrMap defaultTheme))
      st
