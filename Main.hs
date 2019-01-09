{-# LANGUAGE OverloadedStrings #-}

-- base libray
import Control.Monad (when)
import Data.Maybe
import qualified Data.Text as T
import Text.Read (readMaybe)

import Lens.Micro
import Lens.Micro.Aeson

import Options.Applicative

import Paths_fhcontainer (version)

import FedoraDists
import SimpleCmd (cmd, cmd_, cmdBool, {-(+-+)-})
import SimpleCmdArgs
#if (defined(MIN_VERSION_optparse_applicative) && MIN_VERSION_optparse_applicative(0,13,0))
import Data.Semigroup ((<>))
#endif

data ProgOptions = ProgOptions {nameOpt :: Maybe String,
                                pullOpt :: Bool}

main :: IO ()
main =
  simpleCmdArgs (Just version) "Fedora Haskell container tool" "" $
  runContainer <$> opts <*> strArg "DIST/IMAGE/CONTAINER" <*> many (strArg "CMDARGs...")
  where
    opts = ProgOptions <$> 
      optional (strOption (short 'n' <> long "name" <> metavar "NAME" <> help "Container name")) <*> 
      switch (short 'p' <> long "pull" <> help "Pull latest image")

error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif

runContainer :: ProgOptions -> String -> [String] -> IO ()
runContainer opts target args = do
  let mdist = readMaybe target :: Maybe Dist
      request = maybe target distContainer mdist
      haveName = isJust $ nameOpt opts
  haveContainer <- containerExists request
  if haveContainer
    then do
    when haveName $
      error' "Cannot specify name for existing container"
    let name = request
    podman_ "start" ["-i", name]
    let com = if null args then "attach" else "exec"
    podman_ com $ name : args
    podman_ "stop" [name]
    else do
    let image = request
    if (not haveName)
      then do
      com <- if null args then imageShell image else return args
      when (pullOpt opts) $ podman_ "pull" [image]
      podman_ "run" $ ["--rm", "-it", image] ++ com
      else do
      let name = fromJust $ nameOpt opts
      com <- if null args then imageShell image else return args
      when (pullOpt opts) $ podman_ "pull" [image]
      podman_ "create" $ ["-it", "--name=" ++ name, image] ++ com

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

-- imageExists :: String -> IO Bool
-- imageExists name =
--   cmdBool "podman" ["image", "exists", name]

containerExists :: String -> IO Bool
containerExists name =
  cmdBool "podman" ["container", "exists", name]

imageShell :: String -> IO [String]
imageShell name = do
  cfg <- podman "inspect" [name]
  -- podman inspect outputs an Array of Object's
  let ccmd = cfg ^.. nth 0 . key "ContainerConfig" . key "Cmd" . stringArray  & map T.unpack
  return $ if null ccmd then ["/usr/bin/bash"] else ccmd
  where
    stringArray = _Array . traverse . _String
