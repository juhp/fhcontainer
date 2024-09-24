module List (listCmd) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (breakEnd, groupSort, intercalate, sort)

import Podman

listCmd :: IO ()
listCmd = do
  imgs <- B.lines <$> podman "images"
          ["--noheading", "--sort", "repository",
           "--format", "table {{.Repository}} {{.Tag}}",
           "--filter", "dangling=false"]
  mapM_ printImage $ combineTags imgs

combineTags :: [B.ByteString] -> [(String,[String])]
combineTags = groupSort . map (breakEnd (== ' ') . B.unpack)

printImage :: (String,[String]) -> IO ()
printImage (img,tags) =
  putStrLn $ img ++ intercalate "," (sort tags)
