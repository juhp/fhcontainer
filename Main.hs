{-# LANGUAGE OverloadedStrings #-}

-- base libray
import Control.Monad (unless, when)
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable (traverse)
import Text.Read (readMaybe)

import Lens.Micro
import Lens.Micro.Aeson

import Options.Applicative

import Paths_fhcontainer (version)

import FedoraDists (Dist(..))
import SimpleCmd (cmd, cmd_, cmdBool, error')
import SimpleCmdArgs
#if (defined(MIN_VERSION_optparse_applicative) && MIN_VERSION_optparse_applicative(0,13,0))
--import Data.Semigroup ((<>))
#endif

data ProgOptions = ProgOptions {nameOpt :: Maybe String,
                                pullOpt :: Bool}

main :: IO ()
main = do
  cmd_ "echo" ["-ne", "\ESC[22;0t"] -- save term title to title stack
  simpleCmdArgs' (Just version) "Fedora container tool" "" $
    runContainer <$> opts <*> strArg "DIST/IMAGE/CONTAINER" <*> many (strArg "CMDARGs...")
  cmd_ "echo" ["-ne", "\ESC[23;0t"] -- restore term title from stack
  where
    opts = ProgOptions <$>
      optional (strOptionWith 'n' "name" "NAME" "Container name") <*>
      switchWith 'p' "pull" "Pull latest image"

runContainer :: ProgOptions -> String -> [String] -> IO ()
runContainer opts target args = do
  let mdist = readMaybe target :: Maybe Dist
      request = maybe target distContainer mdist
      mayName = nameOpt opts
      givenName = isJust mayName
  mcid <- containerID request
  case mcid of
    Just cid -> do
      when givenName $
        error' "Cannot specify name for existing container"
      podman_ "start" ["-i", cid]
      let (copts, cargs) = splitCtrArgs args
      let com = if null cargs then "attach" else "exec"
      podman_ com $ copts ++ cid : cargs
      podman_ "stop" [cid]
    Nothing -> do
      let image = request
      if pullOpt opts
        then podman_ "pull" [image]
        else do
        haveImage <- imageExists image
        unless haveImage $
          podman_ "pull" [image]
      let (copts, cargs) = splitCtrArgs args
      com <- if null cargs then imageShell image else return args
      if not givenName
        then podman_ "run" $ ["--rm", "-it"] ++ copts ++ image:cargs
        else do
        let name = fromJust mayName
        podman_ "create" $ ["-it", "--name=" ++ name, image] ++ com
  where
    splitCtrArgs :: [String] -> ([String], [String])
    splitCtrArgs =
      span (\ a -> a /= "" && head a == '-')


distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer (EPEL n) = "centos:" ++ show n
distContainer (RHEL n) = "rhel" ++ show n

podman :: String -> [String] -> IO String
podman c as = cmd "podman" (c:as)

podman_ :: String -> [String] -> IO ()
podman_ c as = cmd_ "podman" (c:as)

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
  listToMaybe . map head . filter ((== name) . (!! 1)) . filter ((== 2) . length) . map words . lines <$> podman "ps" ["-a", "--format", "{{.ID}}  {{.Names}}", "--filter", "name=" ++ name]

imageShell :: String -> IO [String]
imageShell name = do
  cfg <- podman "inspect" [name]
  -- podman inspect outputs an Array of Object's
  let ccmd = cfg ^.. nth 0 . key "ContainerConfig" . key "Cmd" . stringArray  & map T.unpack
  return $ if null ccmd then ["/usr/bin/bash"] else ccmd
  where
    stringArray = _Array . traverse . _String
