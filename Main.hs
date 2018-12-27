{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import qualified Data.Text as T
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

import FedoraDists
import SimpleCmd (cmd, cmd_, cmdBool, (+-+))

import Lens.Micro
import Lens.Micro.Aeson

main :: IO ()
main = do
  (opts, dist, args) <- getArgs >>= parseOpts
  runContainer opts dist args

data Flag = Clean | Delete | Pull | Temp
   deriving (Eq)

options :: [OptDescr Flag]
options =
 [ Option "" ["clean"]  (NoArg Clean)  "start a clean container"
 , Option "" ["delete"] (NoArg Delete) "delete container and exit"
 , Option "" ["pull"] (NoArg Pull) "pull latest (when creating)"
 , Option "" ["rm"] (NoArg Temp) "run a temporary container"
 ]

parseOpts :: [String] -> IO ([Flag], Dist, [String])
parseOpts argv =
   case getOpt Permute options argv of
      (os,d:as,[]) -> return (os,read d,as)
      (_,_,errs) -> usage errs
  where
    usage errs = do
      prog <- getProgName
      error' $ (if null errs then [] else concat errs ++ "\n") ++ usageInfo (header prog) options

    header prog =
      "Usage:" +-+ prog +-+ "[option] DIST [cmd]...\n"

error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif

runContainer :: [Flag] -> Dist -> [String] -> IO ()
runContainer opts dist args = do
  let release = releaseVersion dist
      image = "fedora:" ++ release
      name = "fh" ++ release
  if Delete `elem` opts
    then do
    exists <- containerExists name
    if exists
      then podman_ "rm" [name]
      else putStrLn $ "Container does not exist:" +-+ name
    else
    if Temp `elem` opts
    then do
      command <- if null args then imageShell image else return args
      when (Pull `elem` opts) $ podman_ "pull" [image]
      podman_ "run" $ ["--rm", "-it", image] ++ command
    else do
    exists <- containerExists name
    if exists
      then
      when (Clean `elem` opts) $ podman_ "rm" [name]
      else do
      command <- if null args then imageShell image else return args
      when (Pull `elem` opts) $ podman_ "pull" [image]
      podman_ "create" $ ["-it", "--name=" ++ name, image] ++ command
    podman_ "start" ["-i", name]
    let com = if null args then "attach" else "exec"
    podman_ com $ name : args
    podman_ "stop" [name]

podman :: String -> [String] -> IO String
podman c as = cmd "podman" (c:as)

podman_ :: String -> [String] -> IO ()
podman_ c as = cmd_ "podman" (c:as)

-- imageOfContainer :: String -> IO String
-- imageOfContainer name =
--   podman "ps" ["-a", "--filter", "name=" ++ name, "--format", "{{.Image}}"]

containerExists :: String -> IO Bool
containerExists name =
  cmdBool "podman" ["container", "exists", name]

imageShell :: String -> IO [String]
imageShell name = do
  cfg <- podman "inspect" [name]
  -- podman inspect ... outputs an Array of Object's
  let ccmd = cfg ^.. nth 0 . key "ContainerConfig" . key "Cmd" . stringArray  & map T.unpack
  return $ if null ccmd then ["/usr/bin/bash"] else ccmd
  where
    stringArray = _Array . traverse . _String
