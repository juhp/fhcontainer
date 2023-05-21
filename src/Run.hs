{-# LANGUAGE CPP, OverloadedStrings #-}

module Run (runCmd) where

import Control.Monad.Extra (unless, when, whenJust)
import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (word1)
import Data.Maybe
import qualified Data.Text as T
import Text.Read (readMaybe)
import SimpleCmd (cmd_, cmdBool, error')

import Dist
import Podman

runCmd :: Maybe String -> Bool -> Bool -> Maybe String -> String
       -> [String] -> IO ()
runCmd mname pull verbose mmount target args = do
  let request = maybe target distContainer $ readMaybe target
  mcid <- containerID request
  cmd_ "echo" ["-ne", "\ESC[22;0t"] -- save term title to title stack
  case mcid of
    Just cid -> do
      when (isJust mname) $
        error' "Cannot specify name for existing container"
      whenJust mmount $ const $
        error' "Cannot mount volume in existing container"
      podman_ verbose "start" ["-i", cid]
      let (copts, cargs) = splitCtrArgs args
      let com = if null cargs then "attach" else "exec"
      podman_ verbose com $ copts ++ cid : cargs
      podman_ verbose "stop" [cid]
    Nothing -> do
      let image = request
      putStr image
      if pull
        then podman_ verbose "pull" [image]
        else do
        haveImage <- imageExists image
        unless haveImage $
          podman_ verbose "pull" [image]
      imageId <- fromMaybe image <$> latestImage image
      let vol = maybe [] (\dir -> ["--volume", dir ++ if ':' `elem` dir then "" else ":/mnt"]) mmount
      when (imageId /= image) $
        putStrLn $ " " ++ imageId
      let (copts, cargs) = splitCtrArgs args
      case mname of
        Nothing ->
          podman_ verbose "run" $ ["--rm", "-it"] ++ vol ++ copts ++ imageId:cargs
        Just name -> do
          com <- if null cargs then imageShell imageId else return args
          podman_ verbose "create" $ vol ++ ["-it", "--name=" ++ name, imageId] ++ com
  cmd_ "echo" ["-ne", "\ESC[23;0t"] -- restore term title from stack
  where
    splitCtrArgs :: [String] -> ([String], [String])
    splitCtrArgs =
      span (\ a -> a /= "" && head a == '-')

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
