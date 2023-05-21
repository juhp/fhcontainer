module Podman (
  podman,
  podman_
  )
where

import Control.Monad (when)
import SimpleCmd (cmd, cmd_)

podman :: String -> [String] -> IO String
podman c as = cmd "podman" (c:as)

podman_ :: Bool -> String -> [String] -> IO ()
podman_ verbose c as = do
  when verbose $
    putStrLn $! unwords ("podman" : c : as)
  cmd_ "podman" (c:as)
