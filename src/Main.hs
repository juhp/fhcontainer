{-# LANGUAGE CPP #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,4)
  many,
#endif
-- remove with newer simple-cmd-args
#if !MIN_VERSION_base(4,8,0)
  (<$>), (<*>)
#endif
  )
import SimpleCmd (needProgram)
import SimpleCmdArgs

import List
import Paths_fhcontainer (version)
import Run

-- FIXME --list command: lists local available images and containers
main :: IO ()
main = do
  simpleCmdArgs' (Just version) "Fedora container tool" "" $
    runMain
    <$> optional (strOptionWith 'n' "name" "NAME" "Container name")
    <*> switchWith 'p' "pull" "Pull latest image"
    <*> switchWith 'V' "verbose" "output more details"
    <*> optional (strOptionWith 'm' "mount" "DIR" "mount directory into container")
    <*> (flagWith' Nothing 'l' "list" "List local images" <|>
         Just <$> strArg "DIST/IMAGE/CONTAINER")
    <*> many (strArg "CMD+ARGs...")

runMain :: Maybe String -> Bool -> Bool -> Maybe String -> Maybe String
             -> [String] -> IO ()
runMain mname pull verbose mmount mtarget args = do
  needProgram "podman"
  case mtarget of
    Nothing -> listCmd
    Just target -> runCmd mname pull verbose mmount target args
