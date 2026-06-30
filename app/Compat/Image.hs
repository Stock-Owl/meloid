{-# LANGUAGE LambdaCase #-}

module Compat.Image where

import Brick hiding (cached)
import Brick.BChan (BChan, writeBChan)
import Brick.Main qualified as M
import Brick.Widgets.Core qualified as W
import Common
import Compat.Term qualified as Term
import Control.Exception (IOException, try)
import Control.Monad (forM, unless, void)
import Control.Monad.State (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Graphics.Vty.Output qualified as Output
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Network.MPD qualified as MPD

data ImageSlot n = ImageSlot
  { imageSlotName :: n,
    imageSlotSize :: ImageSize
  }

slot :: n -> ImageSize -> ImageSlot n
slot name size =
  ImageSlot
    { imageSlotName = name,
      imageSlotSize = size
    }

placeholderWidget :: (Ord n) => ImageSlot n -> Widget n
placeholderWidget imageSlot =
  W.reportExtent (imageSlotName imageSlot) $
    W.vLimit h $
      W.hLimit w $
        W.fill ' '
  where
    (w, h) = imageSlotSize imageSlot

inlineWidget :: (Ord n) => ImageSlot n -> String -> Widget n
inlineWidget imageSlot art =
  W.reportExtent (imageSlotName imageSlot) $
    W.vLimit h $
      W.hLimit w $
        W.vBox $
          fmap W.str (normalizeInline (w, h) art)
  where
    (w, h) = imageSlotSize imageSlot

lookupSlotExtent :: (Ord n) => ImageSlot n -> EventM n s (Maybe (Extent n))
lookupSlotExtent = M.lookupExtent . imageSlotName

queueRefreshImages :: BChan Event -> EventM FullName St ()
queueRefreshImages chan = liftIO $ writeBChan chan RefreshImages

ensureAlbumArtRequested :: Album -> EventM FullName St ()
ensureAlbumArtRequested album = do
  let key = albumArtKey album
  cached <- Map.member key <$> use stPicCache
  pending <- Set.member key <$> use stPicPending
  format <- use (stEnv . envImageFormat)
  for_ (listToMaybe $ albumSongs album) $ \song ->
    unless (cached || pending) $ do
      stPicPending %= Set.insert key
      sendRequest $ ProcessAlbumArt key format (MPD.toString $ MPD.sgFilePath song)

clearScene :: Output.Output -> EventM FullName St ()
clearScene output = do
  format <- use (stEnv . envImageFormat)
  previous <- use stPaintedScene
  liftIO $ clearPaintedScene output format previous
  stPaintedScene .= Map.empty

refreshScene :: Output.Output -> EventM FullName St ()
refreshScene output = do
  format <- use (stEnv . envImageFormat)
  if not (Term.isOutOfBandFormat format)
    then stPaintedScene .= Map.empty
    else do
      desired <- buildDesiredScene
      previous <- use stPaintedScene
      liftIO $ syncOutOfBandScene output format previous desired
      stPaintedScene .= desired

buildDesiredScene :: EventM FullName St PaintedScene
buildDesiredScene = do
  st <- get
  playing <- case st ^. stCurrentAlbum of
    Nothing -> pure []
    Just album ->
      case lookupRenderedImage st (MainView, AlbumArtPlaying) albumArtPlayingSize of
        Just art@(TerminalGraphic _ _) ->
          pure [((MainView, AlbumArtPlaying), art, slot (MainView, AlbumArtPlaying) albumArtPlayingSize)]
        Just InlineSymbols {} ->
          pure []
        Nothing ->
          ensureAlbumArtRequested album >> pure []

  thumbs <-
    fmap concat . forM (zip [0 ..] $ Vec.toList (st ^. stConfig . csAllAlbums)) $ \(i, album) ->
      let name = (MainView, AlbumArtThumb i)
          imageSlot = slot name albumArtThumbSize
       in lookupSlotExtent imageSlot >>= \case
            Nothing ->
              pure []
            Just extent ->
              case lookupRenderedImage st name albumArtThumbSize of
                Just art@(TerminalGraphic _ _) ->
                  pure [(name, (extent, art))]
                Just InlineSymbols {} ->
                  pure []
                Nothing ->
                  ensureAlbumArtRequested album >> pure []

  playingEntries <-
    fmap concat . forM playing $ \(name, art, imageSlot) ->
      lookupSlotExtent imageSlot >>= \case
        Nothing -> pure []
        Just extent -> pure [(name, (extent, art))]

  pure $ Map.fromList (playingEntries <> thumbs)

syncOutOfBandScene :: Output.Output -> Term.ImageFormat -> PaintedScene -> PaintedScene -> IO ()
syncOutOfBandScene output format previous desired = do
  clearStale output format previous desired
  mapM_ (renderPainted output) (Map.elems desired)

clearPaintedScene :: Output.Output -> Term.ImageFormat -> PaintedScene -> IO ()
clearPaintedScene output format painted =
  case format of
    Term.Symbols -> pure ()
    Term.Kitty -> ignoreIOException $ Output.outputByteBuffer output (BS8.pack "\ESC_Ga=d\ESC\\")
    _ -> mapM_ (clearExtent output . fst) (Map.elems painted)

clearStale :: Output.Output -> Term.ImageFormat -> PaintedScene -> PaintedScene -> IO ()
clearStale output format previous desired =
  case format of
    Term.Symbols -> pure ()
    Term.Kitty -> clearPaintedScene output format previous
    _ -> mapM_ (clearExtent output . fst) stale
  where
    stale =
      Map.elems $
        Map.differenceWith
          ( \old new ->
              if paintedEntryMatches old new
                then Nothing
                else Just old
          )
          previous
          desired

paintedEntryMatches :: (Extent FullName, RenderedImage) -> (Extent FullName, RenderedImage) -> Bool
paintedEntryMatches (oldExtent, oldImage) (newExtent, newImage) =
  oldImage == newImage
    && extentUpperLeft oldExtent == extentUpperLeft newExtent
    && extentSize oldExtent == extentSize newExtent

renderPainted :: Output.Output -> (Extent FullName, RenderedImage) -> IO ()
renderPainted output (extent, art) =
  case art of
    TerminalGraphic _ payload ->
      ignoreIOException $
        Output.outputByteBuffer output $
          BS.concat [saveCursor, moveCursor (extentUpperLeft extent), payload, restoreCursor]
    InlineSymbols {} ->
      pure ()

clearExtent :: Output.Output -> Extent FullName -> IO ()
clearExtent output extent =
  ignoreIOException $
    Output.outputByteBuffer output $
      BS.concat $
        [saveCursor]
          <> fmap clearRow [0 .. h - 1]
          <> [restoreCursor]
  where
    Location (x, y) = extentUpperLeft extent
    (w, h) = extentSize extent
    blankRow = BS8.pack (replicate w ' ')
    clearRow row = moveCursor (Location (x, y + row)) <> blankRow

moveCursor :: Location -> BS.ByteString
moveCursor (Location (x, y)) =
  BS8.pack $ "\ESC[" <> show (y + 1) <> ";" <> show (x + 1) <> "H"

saveCursor :: BS.ByteString
saveCursor = BS8.pack "\ESC7"

restoreCursor :: BS.ByteString
restoreCursor = BS8.pack "\ESC8"

ignoreIOException :: IO () -> IO ()
ignoreIOException action =
  void (try action :: IO (Either IOException ()))

normalizeInline :: ImageSize -> String -> [String]
normalizeInline (w, h) art =
  take h $
    fmap normalizeRow (lines art <> repeat "")
  where
    normalizeRow row = take w (row <> replicate w ' ')
