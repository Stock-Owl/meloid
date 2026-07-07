{- | A module that provides data types and functions to determine
the terminal environment.
It checks which image format to use based on the terminal type.
-}
module Compat.Term (
  ImageFormat (..),
  TermType (..),
  deduceTerminalType,
  deduceFormat,
  isOutOfBandFormat,
  formatArg,
  -- Raw terminal helpers
  emitBytes,
  moveCursor,
  saveCursor,
  restoreCursor,
)
where

import Brick
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Graphics.Vty.Output qualified as Output
import System.Environment (lookupEnv)

{- | A data type to represent different terminal types.
This is mainly used to determine which image format to use.
-}
data TermType
  = Tmux
  | GNUScreen
  | Zellj
  | KittyTerm
  | Foot
  | ITerm2
  | MLTerm
  | WezTerm
  | Alaacritty
  | Ghostty
  | Konsole
  | Gnome
  | Tilix
  | XTerm
  | Unknown
  deriving (Eq, Show)

-- | A data type to represent different image formats
data ImageFormat
  = Kitty
  | Sixel
  | ITerm
  | Symbols
  deriving (Eq, Ord, Show)

{- | Determine which image format to use based on the terminal type.
In the current implementation, high-res formats are supported only
for kitty and foot terminals. For other terminals, symbols are
used to paint the image.
-}
deduceFormat :: TermType -> ImageFormat
deduceFormat t
  | t `elem` [KittyTerm, Ghostty] = Kitty
  | t `elem` [WezTerm, Foot, MLTerm, Konsole, Zellj] = Sixel
  | t == ITerm2 = ITerm
  | otherwise = Symbols

-- | Check if the format is out-of-band.
isOutOfBandFormat :: ImageFormat -> Bool
isOutOfBandFormat Symbols = False
isOutOfBandFormat _ = True

-- | Get the argument for the image format, used by chafa.
formatArg :: ImageFormat -> String
formatArg Compat.Term.Kitty = "kitty"
formatArg Sixel = "sixel"
formatArg ITerm = "iterm"
formatArg Symbols = "symbols"

{- | Determine the terminal type heuristically by checking environ-
ment variables. If no match is found, 'Unknown' is returned.
-}
deduceTerminalType :: IO TermType
deduceTerminalType =
  fromMaybe Unknown . selectMost . catMaybes
    <$> sequence
      [ lookupEnv "TMUX" &&> Tmux
      , lookupEnv "STY" &&> GNUScreen
      , lookupEnv "ZELlj" &&> Zellj
      , lookupEnv "KITTY_WINDOW_ID" &&> KittyTerm
      , assertEnv "TERM" "foot" &&> Foot
      , assertEnv "TERM_PROGRAM" "iTerm.app" &&> ITerm2
      , assertEnv "TERM" "mlterm" &&> MLTerm
      , lookupEnv "WEZTERM_PANE" &&> WezTerm
      , lookupEnv "ALACRITTY_WINDOW_ID" &&> Alaacritty
      , lookupEnv "GHOSTTY_RESOURCE_DIR" &&> Ghostty
      , lookupEnv "KONSOLE_VERSION" &&> Konsole
      , lookupEnv "GNOME_TERMINAL_SCREEN" &&> Gnome
      , lookupEnv "TILIX_ID" &&> Tilix
      , lookupEnv "XTERM_VERSION" &&> XTerm
      ]
 where
  m &&> v = fmap (fmap (const v)) m

  assertEnv env val = do
    v <- lookupEnv env
    pure $
      if v == Just val
        then v
        else Nothing

  -- Pick the terminal type with the most evidence from the
  -- environment variables
  selectMost [] = Nothing
  selectMost xs = listToMaybe $ maximumBy (comparing length) $ group xs

emitBytes :: Output.Output -> BS.ByteString -> IO ()
emitBytes output =
  Output.outputByteBuffer output

moveCursor :: Location -> BS.ByteString
moveCursor (Location (x, y)) =
  BS8.pack $ "\ESC[" <> show (y + 1) <> ";" <> show (x + 1) <> "H"

saveCursor :: BS.ByteString
saveCursor = BS8.pack "\ESC7"

restoreCursor :: BS.ByteString
restoreCursor = BS8.pack "\ESC8"
