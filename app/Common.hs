{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common where

import Brick.BChan (BChan, writeBChan)
import Brick.Types (EventM, Extent)
import Compat.Term (ImageFormat, TermType)
import Compat.Term qualified as Term
import Control.Monad (unless)
import Control.Monad.State (liftIO)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Time
import Data.Vector qualified as Vec
import Lens.Micro (to, (<&>), (^.), _Just)
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (SimpleGetter)
import Network.MPD qualified as MPD
import Text.Printf (printf)

data Event' a
  = Log (LogLevel, String)
  | RefreshImages
  | UpdateSong (Maybe MPD.Song)
  | UpdateStatus MPD.Status
  | UpdateTime (Maybe (Double, Double))
  | LoadAlbumArt (AlbumArtKey, AlbumArt)
  | UpdateConfig a

data Request
  = MPDOperation [MPD.MPD (Either MPD.MPDError ())]
  | SignalInit
  | LogConfig LogLevel String
  | ProcessAlbumArt AlbumArtKey ImageFormat FilePath
  | GetConfig

data ViewName
  = MainView
  | DebugView
  | WelcomeDialog
  | SimpleDialog
  deriving (Show, Eq, Ord)

data WidgetName
  = DebugViewport
  | AllAlbumList
  | AllAlbumListEntry Int
  | ScrollBar FullName
  | VolumeBar
  | SongProgressBar
  | AlbumArtPlaying
  | AlbumArtThumb Int
  | PlayButton
  | PauseButton
  | RewindButton
  | ForwardButton
  | IncreaseVolumeButton
  | DecreaseVolumeButton
  | OkButton
  | SkipButton
  | CancelButton
  | NextButton
  | FinishButton
  | PrevButton
  | ExampleButton
  deriving (Show, Eq, Ord)

parentScrollable :: WidgetName -> WidgetName
parentScrollable AllAlbumListEntry {} = AllAlbumList
parentScrollable n = n

type FullName = (ViewName, WidgetName)

type ImageSize = (Int, Int)

type AlbumArtKey = String

data RenderedImage
  = InlineSymbols String
  | TerminalGraphic ImageFormat ByteString
  deriving (Eq, Show)

type AlbumArt = Map.Map ImageSize RenderedImage

type PaintedScene = Map.Map FullName (Extent FullName, RenderedImage)

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord)

data DialogSt s = DialogSt
  { _dsPage :: Int,
    _dsText :: String,
    _dsCurrent :: ViewName,
    _dsCallbacks :: [(WidgetName, EventM FullName s ())]
  }

makeLenses ''DialogSt

data Album = Album
  { albumName :: String,
    albumArtists :: [String],
    albumSongs :: [MPD.Song],
    albumGenre :: String,
    albumReleaseDate :: String
  }

defaultAlbum :: Album
defaultAlbum =
  Album "" [] [] "" ""

data ConfigSt = ConfigSt
  { _csVolume :: MPD.Volume,
    _csMusicDir :: FilePath,
    _csAllPlaylists :: Vec.Vector MPD.PlaylistName,
    _csAllDirs :: Vec.Vector FilePath,
    _csAllAlbums :: Vec.Vector Album
  }

makeLenses ''ConfigSt

data PlayingSt = PlayingSt
  { _psCurrentSong :: Maybe MPD.Song,
    _psCurrentTime :: Maybe (Double, Double),
    _psPaused :: Bool
  }

makeLenses ''PlayingSt

data Environment = Environment
  { _envTermType :: TermType,
    _envImageFormat :: ImageFormat
  }

makeLenses ''Environment

data St
  = St
  { _stPressed :: Maybe FullName,
    _stBars :: Map.Map FullName (Int, Int),
    _stCurrentView :: ViewName,
    _stLastView :: ViewName,
    _stDialog :: Maybe (DialogSt St),
    _stConfig :: ConfigSt,
    _stPlaying :: PlayingSt,
    _stLogs :: [(LogLevel, String)],
    _stChannel :: Maybe (BChan Request),
    _stPicCache :: Map.Map AlbumArtKey AlbumArt,
    _stPicPending :: Set.Set AlbumArtKey,
    _stPaintedScene :: PaintedScene,
    _stPanic :: Bool,
    _stEnv :: Environment
  }

makeLenses ''St

type Event = Event' ConfigSt

albumArtPlayingSize :: ImageSize
albumArtPlayingSize = (6, 3)

albumArtThumbSize :: ImageSize
albumArtThumbSize = (6, 3)

songMeta :: MPD.Metadata -> MPD.Song -> NonEmpty String
songMeta meta song =
  fromMaybe (pure $ unknown meta) (MPD.sgTags song !? meta <&> fromList . fmap MPD.toString)
  where
    unknown MPD.Artist = "Unknown Artist"
    unknown MPD.Album = "Unknown Album"
    unknown MPD.Title = "Unknown Title"
    unknown _ = "Unknown"

songAlbumArtKey :: MPD.Song -> AlbumArtKey
songAlbumArtKey song =
  case tag MPD.Album of
    Just album -> "album:" <> fromMaybe "" (tag MPD.Artist) <> "\0" <> album
    Nothing -> "file:" <> MPD.toString (MPD.sgFilePath song)
  where
    tag meta = MPD.toString <$> (MPD.sgTags song !? meta >>= listToMaybe)

albumArtKey :: Album -> AlbumArtKey
albumArtKey album =
  case albumSongs album of
    song : _ -> songAlbumArtKey song
    [] -> "album:" <> concat (albumArtists album) <> "\0" <> albumName album

stCurrentAlbum :: SimpleGetter St (Maybe Album)
stCurrentAlbum = to $ \st -> do
  song <- st ^. stPlaying . psCurrentSong
  let key = songAlbumArtKey song
  listToMaybe
    [ album
    | album <- Vec.toList (st ^. stConfig . csAllAlbums),
      albumArtKey album == key
    ]

stCurrentAlbumArt :: SimpleGetter St (Maybe AlbumArt)
stCurrentAlbumArt = to $ \st ->
  st ^. stCurrentAlbum >>= \album ->
    (st ^. stPicCache) !? albumArtKey album

lookupRenderedImage :: St -> FullName -> ImageSize -> Maybe RenderedImage
lookupRenderedImage st (_, AlbumArtPlaying) size =
  st ^. stCurrentAlbumArt >>= (Map.!? size)
lookupRenderedImage st (MainView, AlbumArtThumb i) size = do
  album <- st ^. stConfig . csAllAlbums . to (Vec.!? i)
  art <- st ^. stPicCache . to (Map.!? albumArtKey album)
  art !? size
lookupRenderedImage _ _ _ = Nothing

stCurrentSongMeta' :: MPD.Metadata -> SimpleGetter St (Maybe (NonEmpty MPD.Value))
stCurrentSongMeta' meta = stPlaying . psCurrentSong . to f
  where
    f (Just s) = fromList <$> MPD.sgTags s !? meta
    f Nothing = Nothing

stCurrentSongMeta :: MPD.Metadata -> SimpleGetter St (NonEmpty String)
stCurrentSongMeta meta = stPlaying . psCurrentSong . to (fromList . f)
  where
    f (Just s) = fromMaybe [unknown meta] (MPD.sgTags s !? meta <&> fmap MPD.toString)
    f Nothing = [unknown meta]
    unknown MPD.Artist = "Unknown Artist"
    unknown MPD.Album = "Unknown Album"
    unknown MPD.Title = "Unknown Title"
    unknown _ = "Unknown"

defaultSt :: St
defaultSt =
  St
    { _stPressed = Nothing,
      _stBars = Map.empty,
      _stCurrentView = MainView,
      _stLastView = MainView,
      _stDialog = Nothing,
      _stConfig =
        ConfigSt
          { _csVolume = 0,
            _csMusicDir = "",
            _csAllPlaylists = Vec.empty,
            _csAllDirs = Vec.empty,
            _csAllAlbums = Vec.empty
          },
      _stPlaying =
        PlayingSt
          { _psCurrentSong = Nothing,
            _psCurrentTime = Nothing,
            _psPaused = False
          },
      _stLogs = [],
      _stChannel = Nothing,
      _stPicCache = Map.empty,
      _stPicPending = Set.empty,
      _stPaintedScene = Map.empty,
      _stPanic = False,
      _stEnv =
        Environment
          { _envTermType = Term.Unknown,
            _envImageFormat = Term.Symbols
          }
    }

panic :: EventM FullName St ()
panic = stPanic .= True

closeDialog :: EventM FullName St ()
closeDialog = stDialog .= Nothing

openSimpleDialog :: String -> [(WidgetName, EventM FullName St ())] -> EventM FullName St ()
openSimpleDialog text callbacks = stDialog .= Just (DialogSt 0 text SimpleDialog callbacks)

switchView :: ViewName -> EventM FullName St ()
switchView v = do
  current <- use stCurrentView
  unless (current == v) $ do
    stLastView .= current
    stCurrentView .= v

returnToLastView :: EventM FullName St ()
returnToLastView = switchView =<< use stLastView

sendRequest :: Request -> EventM FullName St ()
sendRequest r = do
  chan <- use stChannel
  case chan of
    Nothing -> return ()
    Just c -> liftIO $ writeBChan c r

formatLog :: String -> String -> IO String
formatLog from msg = do
  timestamp <- getZonedTime
  let timeFormat = "%H:%M:%S"
      timeStr = formatTime defaultTimeLocale timeFormat timestamp
  pure $ printf "[%s] [%s]: %s" timeStr from msg

logEv :: BChan Event -> LogLevel -> String -> String -> IO ()
logEv chan level from msg = do
  formatted <- formatLog from msg
  writeBChan chan (Log (level, formatted))

logInfo :: BChan Event -> String -> String -> IO ()
logInfo chan from msg = logEv chan Info from msg

logError :: BChan Event -> String -> String -> IO ()
logError chan from msg = logEv chan Error from msg

logWarn :: BChan Event -> String -> String -> IO ()
logWarn chan from msg = logEv chan Warn from msg

logDebug :: BChan Event -> String -> String -> IO ()
logDebug chan from msg = logEv chan Debug from msg

(.?) :: (Applicative f) => ((Maybe a1 -> f (Maybe a')) -> c) -> (a2 -> a1 -> f a') -> a2 -> c
a .? b = a . _Just . b

infixr 9 .?

defaultHead :: (Monoid a) => [a] -> a
defaultHead [] = mempty
defaultHead (x : _) = x
