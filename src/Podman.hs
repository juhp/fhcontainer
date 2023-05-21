{-# LANGUAGE CPP, OverloadedStrings #-}

module Podman (
  podman,
  podman_,
  containerID,
  imageExists,
  imageShell,
  latestImage
  )
where

import Control.Monad (when)
import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (word1)
import Data.Maybe (fromMaybe, listToMaybe)
import SimpleCmd (cmd, cmd_, cmdBool)
import qualified Data.Text as T

podman :: String -> [String] -> IO String
podman c as = cmd "podman" (c:as)

podman_ :: Bool -> String -> [String] -> IO ()
podman_ verbose c as = do
  when verbose $
    putStrLn $! unwords ("podman" : c : as)
  cmd_ "podman" (c:as)


-- imageOfContainer :: String -> IO String
-- imageOfContainer name =
--   podman "ps" ["-a", "--filter", "name=" ++ name, "--format", "{{.Image}}"]

imageExists :: String -> IO Bool
imageExists name =
  cmdBool "podman" ["image", "exists", name]

-- containerExists :: String -> IO Bool
-- containerExists name =
--   cmdBool "podman" ["container", "exists", name]

containerID :: String -- ^ name
            -> IO (Maybe String) -- ^ id
containerID name =
  listToMaybe . map fst . filter ((== name) . snd) . map word1 . lines <$> podman "ps" ["-a", "--noheading", "--format", "{{.ID}}  {{.Names}}", "--filter", "name=" ++ name]

imageShell :: String -> IO [String]
imageShell name = do
  cfg <- podman "inspect" [name]
  -- podman inspect outputs an Array of Object's
  -- was ContainerConfig
  let mccmd =
        case decode (B.pack cfg) of
          Just [obj] -> lookupKey "Config" obj >>= lookupKey "Cmd"
          _ -> Nothing
  return $ fromMaybe ["/usr/bin/bash"] mccmd

latestImage :: String -> IO (Maybe String)
latestImage name =
    listToMaybe . lines <$> podman "images" ["--format", "{{.ID}}", name]

-- from http-query
-- | Look up key in object
lookupKey :: FromJSON a => T.Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: fromText k)
#if !MIN_VERSION_aeson(2,0,0)
  where
    fromText :: T.Text -> T.Text
    fromText = id
#endif
