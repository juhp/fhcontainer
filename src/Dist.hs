{-# LANGUAGE CPP #-}

module Dist where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (*>))
#endif

import Data.Monoid.Extra (mwhen)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor (($>))
import Numeric.Natural
import SimpleCmd (error')

data Dist = Fedora Natural -- ^ Fedora release
          | Centos Natural Bool Bool -- ^ Centos version devel minimal
          | Ubi Natural    -- ^ RHEL version
          | Other String
  deriving (Eq)

parseNat :: Parser Natural
parseNat = read <$> many1 digit

parseFedora :: Parser Dist
parseFedora = Fedora <$> (char 'f' *> parseNat)

parseCentos :: Parser Dist
parseCentos =
  Centos
  <$> (longShort "centos" 'c' *> parseNat) -- version
  <*> option False (longShort "-devel" 'd' $> True)
  <*> option False (longShort "-minimal" 'm' $> True)
  where
    longShort :: String -> Char -> Parser String
    longShort str c =
      try (string str) <|> string [c]

parseUbi :: Parser Dist
parseUbi = Ubi <$> (string "ubi" *> parseNat)

parseOther :: Parser Dist
parseOther = Other <$> many1 anyChar

parseDist :: Parser Dist
parseDist = try parseFedora <|> try parseCentos <|> try parseUbi <|> parseOther

readDist :: String -> Dist
readDist s =
  case parse parseDist "" s of
    Left err -> error' $ show err
    Right d -> d

distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
-- FIXME make 11 default to development
distContainer (Centos n d m) = "centos:stream" ++ show n ++ (mwhen d "-development") ++ (mwhen m "-minimal")
distContainer (Ubi v) = "ubi" ++ show v
distContainer (Other s) = s
