module List (listCmd) where

import Data.List.Extra (breakEnd, groupSort, intercalate, sort)

import Podman

listCmd :: IO ()
listCmd = do
  imgs <- lines <$> podman "images"
          ["--noheading", "--sort", "repository",
           "--format", "table {{.Repository}} {{.Tag}}",
           "--filter", "dangling=false"]
  mapM_ printImage $ combineTags imgs

combineTags :: [String] -> [(String,[String])]
combineTags = groupSort . map (breakEnd (== ' '))

printImage :: (String,[String]) -> IO ()
printImage (img,tags) =
  putStrLn $ img ++ intercalate "," (sort tags)
