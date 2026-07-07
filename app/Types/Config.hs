{-# LANGUAGE TemplateHaskell #-}

{- | Checked-in config defaults loaded from the YAML asset.
These helpers live outside `Types.Model` so the compile-time
splices can reuse `ConfigValue`'s normal instances generically.
-}
module Types.Config (
  defaultConfigStr,
  defaultConfigValue,
) where

import Data.ByteString qualified as BS
import Data.Yaml qualified as YAML
import Language.Haskell.TH.Syntax (addDependentFile, lift, runIO)
import Types.Model (ConfigValue)

-- | The checked-in default config text, copied verbatim on first run.
defaultConfigStr :: String
defaultConfigStr =
  $( do
       let fp = "assets/default-config.yaml"
       addDependentFile fp
       content <- runIO (readFile fp)
       lift content
   )

-- | The checked-in default config, decoded at compile time.
defaultConfigValue :: ConfigValue
defaultConfigValue =
  $( do
       let fp = "assets/default-config.yaml"
       addDependentFile fp
       content <- runIO (BS.readFile fp)
       case (YAML.decodeEither' content :: Either YAML.ParseException ConfigValue) of
         Left err ->
           fail $
             "Failed to decode " <> fp <> " as ConfigValue:\n"
               <> YAML.prettyPrintParseException err
         Right value ->
           lift value
   )
