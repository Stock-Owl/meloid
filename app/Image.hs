{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Image where

import Common
import Compat.Term qualified as Term
import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State (liftIO)
import Control.Monad.Trans.Except
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode, openBinaryTempFile)
import System.Process

readAlbumArtBytes :: String -> IO (Either IOException BS.ByteString)
readAlbumArtBytes uri = do
  embedded <- runExceptT $ runMpcBytes ["readpicture", uri]
  case embedded of
    Right bs -> pure (Right bs)
    Left _ -> runExceptT $ runMpcBytes ["albumart", uri]

renderAlbumArt :: Term.ImageFormat -> ImageSize -> BS.ByteString -> ExceptT IOException IO RenderedImage
renderAlbumArt format size bs = do
  rendered <- chafaOutput format size bs
  pure $
    case format of
      Term.Symbols -> InlineSymbols (UTF8.toString rendered)
      _ -> TerminalGraphic format rendered

runMpcBytes :: [String] -> ExceptT IOException IO BS.ByteString
runMpcBytes args =
  readRawProcess "mpc" args handleMpcResult
  where
    handleMpcResult = \case
      (ExitSuccess, out, err)
        | looksLikeMpcStatus out ->
            throwE $ userError $ "mpc returned status text instead of image bytes: " <> UTF8.toString err
        | otherwise ->
            pure out
      (ExitFailure n, _, err) ->
        throwE $ userError $ "mpc failed, exit " <> show n <> ": " <> UTF8.toString err

    looksLikeMpcStatus out =
      BS.isPrefixOf (UTF8.fromString "volume:") out

chafaOutput :: Term.ImageFormat -> ImageSize -> BS.ByteString -> ExceptT IOException IO BS.ByteString
chafaOutput format (w, h) bs = do
  let sizeArg = show w <> "x" <> show h
  tmpPath <- writeTempImage bs
  output <-
    runChafa format sizeArg tmpPath `catchE` const do
      pngPath <- convertToPng tmpPath
      cleanupTempImage tmpPath
      retried <- runChafa format sizeArg pngPath
      cleanupTempImage pngPath
      pure retried
  cleanupTempImage tmpPath
  pure output

writeTempImage :: BS.ByteString -> ExceptT IOException IO FilePath
writeTempImage bs = do
  (tmpPath, handle) <- ExceptT . try $ openBinaryTempFile "/tmp" "gaze-player-image-input"
  liftIO $ hSetBinaryMode handle True
  ExceptT (try $ BS.hPut handle bs >> hClose handle) `catchE` \err -> do
    cleanupTempImage tmpPath
    throwE err
  pure tmpPath

convertToPng :: FilePath -> ExceptT IOException IO FilePath
convertToPng inputPath = do
  (tmpPath, handle) <- ExceptT . try $ openBinaryTempFile "/tmp" "gaze-player-image-converted.png"
  liftIO $ hClose handle
  readRawProcess "magick" [inputPath, tmpPath] (handleMagickResult tmpPath) `catchE` \err -> do
    cleanupTempImage tmpPath
    throwE err
  where
    handleMagickResult tmpPath = \case
      (ExitSuccess, _, _) ->
        pure tmpPath
      (_, _, err) -> do
        cleanupTempImage tmpPath
        throwE $ userError $ UTF8.toString err

runChafa :: Term.ImageFormat -> String -> FilePath -> ExceptT IOException IO BS.ByteString
runChafa format sizeArg imagePath =
  readRawProcess "chafa" (chafaArgs format sizeArg imagePath) handleChafaResult
  where
    handleChafaResult = \case
      (ExitSuccess, out, _) ->
        pure $ sanitizeChafaOutput format out
      (_, _, err) ->
        throwE $ userError $ UTF8.toString err

chafaArgs :: Term.ImageFormat -> String -> FilePath -> [String]
chafaArgs format sizeArg imagePath =
  commonArgs <> formatArgs format <> [imagePath]
  where
    commonArgs = ["-s", sizeArg]
    formatArgs Term.Symbols = ["-f", Term.formatArg Term.Symbols]
    formatArgs fmt = ["-f", Term.formatArg fmt]

readRawProcess ::
  String ->
  [String] ->
  ((ExitCode, BS.ByteString, BS.ByteString) -> ExceptT IOException IO a) ->
  ExceptT IOException IO a
readRawProcess prog args handleResult =
  rawProcess prog args >>= handleResult

rawProcess :: String -> [String] -> ExceptT IOException IO (ExitCode, BS.ByteString, BS.ByteString)
rawProcess prog args =
  (ExceptT . try $ createProcess (proc prog args) {std_out = CreatePipe, std_err = CreatePipe})
    >>= \case
      (_, Just hout, Just herr, ph) -> do
        out <- liftIO $ BS.hGetContents hout
        err <- liftIO $ BS.hGetContents herr
        code <- liftIO $ waitForProcess ph
        pure (code, out, err)
      _ -> throwE $ userError "failed to run process: unexpected process pipe setup"

sanitizeChafaOutput :: Term.ImageFormat -> BS.ByteString -> BS.ByteString
sanitizeChafaOutput Term.Symbols = trimTrailingLineBreaks
sanitizeChafaOutput Term.Kitty = trimTrailingLineBreaks . pinKittyPlacement . extractKittyGraphics
sanitizeChafaOutput _ = trimTrailingLineBreaks

trimTrailingLineBreaks :: BS.ByteString -> BS.ByteString
trimTrailingLineBreaks = BS.reverse . BS.dropWhile isLineBreak . BS.reverse
  where
    isLineBreak b = b == 10 || b == 13

pinKittyPlacement :: BS.ByteString -> BS.ByteString
pinKittyPlacement bs =
  case BS.breakSubstring marker bs of
    (prefix, rest)
      | BS.null rest -> bs
      | otherwise ->
          prefix
            <> marker
            <> cursorStatic
            <> BS.drop (BS.length marker) rest
  where
    marker = BS.pack [27, 95, 71, 97, 61, 84, 44]
    cursorStatic = BS.pack [67, 61, 49, 44]

extractKittyGraphics :: BS.ByteString -> BS.ByteString
extractKittyGraphics = BS.concat . go
  where
    start = BS.pack [27, 95, 71]
    endST = BS.pack [27, 92]

    go bs =
      let (_, rest) = BS.breakSubstring start bs
       in if BS.null rest
            then []
            else
              let (chunk, remaining) = takeKittySequence rest
               in chunk : go remaining

    takeKittySequence bs =
      case kittySequenceEnd bs of
        Nothing -> (bs, BS.empty)
        Just end -> (BS.take end bs, BS.drop end bs)

    kittySequenceEnd bs =
      earliest (stEnd bs) (belEnd bs)

    stEnd bs =
      let (before, after) = BS.breakSubstring endST bs
       in if BS.null after then Nothing else Just (BS.length before + BS.length endST)

    belEnd bs = (+ 1) <$> BS.elemIndex 7 bs

    earliest Nothing y = y
    earliest x Nothing = x
    earliest (Just x) (Just y) = Just (min x y)

cleanupTempImage :: FilePath -> ExceptT IOException IO ()
cleanupTempImage path =
  liftIO $ void (try (removeFile path) :: IO (Either IOException ()))
