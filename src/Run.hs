module Run (runCmd) where

import Control.Monad.Extra (unless, when, whenJust)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import SimpleCmd (cmd_, error')

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
