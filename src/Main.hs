{-# LANGUAGE CPP, OverloadedStrings #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,4)
  many,
#endif
-- remove with newer simple-cmd-args
#if !MIN_VERSION_base(4,8,0)
  (<$>), (<*>)
#endif
  )
import Control.Monad.Extra (unless, when, whenJust)
import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (breakEnd, groupSort, intercalate, sort, word1)
import Data.Maybe
import qualified Data.Text as T
import Text.Read (readMaybe)
import SimpleCmd (cmd, cmd_, cmdBool, error', needProgram)
import SimpleCmdArgs

import Dist
import Paths_fhcontainer (version)

-- FIXME --list command: lists local available images and containers
main :: IO ()
main = do
  simpleCmdArgs' (Just version) "Fedora container tool" "" $
    runContainer
    <$> optional (strOptionWith 'n' "name" "NAME" "Container name")
    <*> switchWith 'p' "pull" "Pull latest image"
    <*> switchWith 'V' "verbose" "output more details"
    <*> optional (strOptionWith 'm' "mount" "DIR" "mount directory into container")
    <*> (flagWith' Nothing 'l' "list" "List local images" <|>
         Just <$> strArg "DIST/IMAGE/CONTAINER")
    <*> many (strArg "CMD+ARGs...")

runContainer :: Maybe String -> Bool -> Bool -> Maybe String -> Maybe String
             -> [String] -> IO ()
runContainer mname pull verbose mmount mtarget args = do
  needProgram "podman"
  case mtarget of
    Nothing -> do
      imgs <- lines <$> podman "images"
              ["--noheading", "--sort", "repository",
               "--format", "table {{.Repository}} {{.Tag}}",
               "--filter", "dangling=false"]
      mapM_ printImage $ combineTags imgs
    Just target -> do
      let mdist = readMaybe target :: Maybe Dist
          request = maybe target distContainer mdist
          givenName = isJust mname
      mcid <- containerID request
      cmd_ "echo" ["-ne", "\ESC[22;0t"] -- save term title to title stack
      case mcid of
        Just cid -> do
          when givenName $
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

podman_ :: Bool -> String -> [String] -> IO ()
podman_ verbose c as = do
  when verbose $
    putStrLn $! unwords ("podman" : c : as)
  cmd_ "podman" (c:as)

podman :: String -> [String] -> IO String
podman c as = cmd "podman" (c:as)

-- imageOfContainer :: String -> IO String
-- imageOfContainer name =
--   podman "ps" ["-a", "--filter", "name=" ++ name, "--format", "{{.Image}}"]

imageExists :: String -> IO Bool
imageExists name =
  cmdBool "podman" ["image", "exists", name]

-- containerExists :: String -> IO Bool
-- containerExists name =
--   cmdBool "podman" ["container", "exists", name]

containerID ::
  String ->
  -- ^ name
  IO (Maybe String)
  -- ^ id
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

combineTags :: [String] -> [(String,[String])]
combineTags = groupSort . map (breakEnd (== ' '))

printImage :: (String,[String]) -> IO ()
printImage (img,tags) =
  putStrLn $ img ++ intercalate "," (sort tags)
