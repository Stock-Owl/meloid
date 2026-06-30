{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Sys where

import Brick.BChan
import Common hiding (panic)
import Compat.Term
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (isSpace)
import Data.Function (on)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector qualified as Vec
import Image qualified
import Lens.Micro ((<&>))
import Network.MPD qualified as MPD
import Network.MPD.Core qualified as Core
import Prelude hiding (log)

songProgressLoopThread :: BChan Event -> IO (MPD.Response ())
songProgressLoopThread evChan = MPD.withMPD $ forever $ do
  status <- MPD.status
  liftIO $ do
    writeBChan evChan $ UpdateTime (MPD.stTime status)
    threadDelay 500000

songChangeLoopThread :: BChan Event -> IO (MPD.Response ())
songChangeLoopThread evChan = MPD.withMPD $ forever $ do
  _ <- MPD.idle [MPD.PlayerS]
  status <- MPD.status
  curSong <- MPD.currentSong
  liftIO $ do
    postEvent $ UpdateStatus status
    postEvent $ UpdateSong curSong
  where
    postEvent :: Event -> IO ()
    postEvent = writeBChan evChan

musicPlayerThread :: BChan Request -> BChan Event -> IO ()
musicPlayerThread reqChan evChan = do
  res0 <- MPD.withMPD MPD.status

  case res0 of
    Left _ ->
      panic $
        unlines
          [ "MPD is not available.",
            "Do you have MPD installed and running?",
            "You can follow the instructions at https://mpd.readthedocs.io/en/stable/user.html to install it."
          ]
    Right MPD.Status {stError = Just err} ->
      panic $ "MPD is not available. Error: \n" <> err
    Right _ ->
      log "MPD is available."

  _ <- MPD.withMPD $ MPD.rescan Nothing

  forever $ do
    req <- readBChan reqChan
    res <- case req of
      LogConfig level msg ->
        logEv evChan level "Setup" msg >> pure Nothing
      SignalInit ->
        forkIO
          ( songChangeLoopThread evChan >>= \case
              Right _ -> pure ()
              Left err -> panic $ "Error while starting song change loop: \n" <> show err
          )
          >> forkIO
            ( songProgressLoopThread evChan >>= \case
                Right _ -> pure ()
                Left err -> panic $ "Error while starting song progress loop: \n" <> show err
            )
          >> pure Nothing
      MPDOperation op ->
        Just <$> MPD.withMPD (sequence op)
      ProcessAlbumArt key format uri -> do
        void $ forkIO $ processAlbumArt key format uri
        pure Nothing
      GetConfig -> do
        let socket = "/run/user/1000/mpd/socket"
        result <- runExceptT $ do
          dir <- ExceptT $ MPD.withMPD_ (Just socket) Nothing getMusicDirectory
          vol <- ExceptT $ MPD.withMPD $ MPD.status <&> MPD.stVolume
          all' <- ExceptT $ MPD.withMPD $ MPD.listAllInfo ""
          let songs = [song | MPD.LsSong song <- all']
              playlists = [playlist | MPD.LsPlaylist playlist <- all']
              dirs = [dir' | MPD.LsDirectory dir' <- all']
              albums' = groupBy ((==) `on` songAlbumArtKey) $ sortOn songAlbumArtKey songs
              albums =
                albums' <&> \tracks -> case listToMaybe tracks of
                  Just cand ->
                    Album
                      { albumName = NonEmpty.head $ songMeta MPD.Album cand,
                        albumArtists = nub . concat $ NonEmpty.toList . songMeta MPD.Artist <$> tracks,
                        albumGenre = NonEmpty.head $ songMeta MPD.Genre cand,
                        albumReleaseDate = NonEmpty.head $ songMeta MPD.Date cand,
                        albumSongs = tracks
                      }
                  Nothing ->
                    defaultAlbum
          liftIO $
            postEvent $
              UpdateConfig $
                ConfigSt
                  { _csVolume = fromMaybe 0 vol,
                    _csMusicDir = fromMaybe "" dir,
                    _csAllPlaylists = Vec.fromList playlists,
                    _csAllDirs = Vec.fromList (fmap MPD.toString dirs),
                    _csAllAlbums = Vec.fromList albums
                  }
        either (pure . Just . Left) (const $ pure Nothing) result

    case res of
      Just (Left x) ->
        panic $ "An error occurred with MPD:\n" <> show x
      _ ->
        pure ()
  where
    panic = logEv evChan Error "MPD"
    log = logEv evChan Info "MPD"
    warn = logEv evChan Warn "MPD"

    postEvent :: Event -> IO ()
    postEvent = writeBChan evChan

    processAlbumArt :: AlbumArtKey -> ImageFormat -> FilePath -> IO ()
    processAlbumArt key format uri = do
      result <- runExceptT $ do
        bytes <- ExceptT $ Image.readAlbumArtBytes uri
        arts <-
          forM
            (nub [albumArtPlayingSize, albumArtThumbSize])
            ( \size -> do
                art <- Image.renderAlbumArt format size bytes
                pure (size, art)
            )
        liftIO $ postEvent (LoadAlbumArt (key, Map.fromList arts))
      either (warn . ("Error while processing album art:\n" <>) . show) (const $ pure ()) (result :: Either IOException ())

    getMusicDirectory :: MPD.MPD (Maybe FilePath)
    getMusicDirectory = do
      lines_ <- Core.getResponse "config"
      pure $ lookupConfig "music_directory" (map UTF8.toString lines_)

    lookupConfig :: String -> [String] -> Maybe String
    lookupConfig key lines_ =
      case find ((key ++ ":") `prefixOf`) lines_ of
        Nothing -> Nothing
        Just line ->
          let value =
                dropWhile isSpace $
                  drop 1 $
                    dropWhile (/= ':') line
           in Just value

    prefixOf :: String -> String -> Bool
    prefixOf prefix s =
      take (length prefix) s == prefix
