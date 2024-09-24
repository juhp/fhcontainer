{-# LANGUAGE CPP, OverloadedStrings #-}

module Podman (
  podman,
  podman_,
  containerID,
  imageExists,
  imageShell,
  latestImage,
  displayImageDate
  )
where

import Control.Monad.Extra (when, whenJust)
import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (word1)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import SimpleCmd (cmd_, cmdBool, error')
import System.Process.Typed (proc, readProcess,
#if MIN_VERSION_typed_process(0,2,8)
                             ExitCode(ExitSuccess)
#endif
                            )
#if !MIN_VERSION_typed_process(0,2,8)
import System.Exit (ExitCode(ExitSuccess))
#endif

podman :: String -> [String] -> IO B.ByteString
podman c as = do
  (ok,out,err) <- readProcess $ proc "podman" (c:as)
  if ok == ExitSuccess
    then return out
    else error' $ B.unpack err

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
  listToMaybe . map fst . filter ((== name) . snd) . map (word1 . B.unpack) . B.lines <$> podman "ps" ["-a", "--noheading", "--format", "{{.ID}}  {{.Names}}", "--filter", "name=" ++ name]

imageShell :: String -> IO [String]
imageShell name = do
  cfg <- podman "inspect" [name]
  -- podman inspect outputs an Array of Object's
  -- was ContainerConfig
  let mccmd =
        case decode cfg of
          Just [obj] -> lookupKey "Config" obj >>= lookupKey "Cmd"
          _ -> Nothing
  return $ fromMaybe ["/usr/bin/bash"] mccmd

latestImage :: String -> IO (Maybe String)
latestImage name =
    fmap B.unpack . listToMaybe . B.lines <$> podman "images" ["--format", "{{.ID}}", name]

-- from http-query
-- | Look up key in object
lookupKey :: FromJSON a => T.Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: fromText k)
#if !MIN_VERSION_aeson(2,0,0)
  where
    fromText :: T.Text -> T.Text
    fromText = id
#endif

displayImageDate :: String -> IO ()
displayImageDate image = do
  cfg <- podman "inspect" [image]
  -- podman inspect outputs an Array of Object's
  -- was ContainerConfig
  case decode cfg of
    Just [obj] ->
      whenJust (lookupKey "Created" obj) printTime
    _ -> return ()
  where
    printTime u = do
      t <- utcToLocalZonedTime u
      putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" t
