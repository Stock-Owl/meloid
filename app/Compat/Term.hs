module Compat.Term where

import Data.List
import Data.Maybe
import Data.Ord (comparing)
import System.Environment (lookupEnv)

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

data ImageFormat
  = Kitty
  | Sixel
  | ITerm
  | Symbols
  deriving (Eq, Ord, Show)

deduceFormat :: TermType -> ImageFormat
deduceFormat t
  | t `elem` [KittyTerm, Ghostty] = Kitty
  | t `elem` [WezTerm, Foot, MLTerm, Konsole, Zellj] = Sixel
  | t == ITerm2 = ITerm
  | otherwise = Symbols

isOutOfBandFormat :: ImageFormat -> Bool
isOutOfBandFormat Symbols = False
isOutOfBandFormat _ = True

formatArg :: ImageFormat -> String
formatArg Compat.Term.Kitty = "kitty"
formatArg Sixel = "sixel"
formatArg ITerm = "iterm"
formatArg Symbols = "symbols"

deduceTerminalType :: IO TermType
deduceTerminalType =
  fromMaybe Unknown . selectMost . catMaybes
    <$> sequence
      [ lookupEnv "TMUX" &&> Tmux,
        lookupEnv "STY" &&> GNUScreen,
        lookupEnv "ZELlj" &&> Zellj,
        lookupEnv "KITTY_WINDOW_ID" &&> KittyTerm,
        assertEnv "TERM" "foot" &&> Foot,
        assertEnv "TERM_PROGRAM" "iTerm.app" &&> ITerm2,
        assertEnv "TERM" "mlterm" &&> MLTerm,
        lookupEnv "WEZTERM_PANE" &&> WezTerm,
        lookupEnv "ALACRITTY_WINDOW_ID" &&> Alaacritty,
        lookupEnv "GHOSTTY_RESOURCE_DIR" &&> Ghostty,
        lookupEnv "KONSOLE_VERSION" &&> Konsole,
        lookupEnv "GNOME_TERMINAL_SCREEN" &&> Gnome,
        lookupEnv "TILIX_ID" &&> Tilix,
        lookupEnv "XTERM_VERSION" &&> XTerm
      ]
  where
    m &&> v = fmap (fmap (const v)) m

    assertEnv env val = do
      v <- lookupEnv env
      pure $
        if v == Just val
          then v
          else Nothing

    selectMost [] = Nothing
    selectMost xs = listToMaybe $ maximumBy (comparing length) $ group xs
