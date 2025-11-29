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
          | ELN -- ^ Fedora eln
          | Centos Natural Bool Bool -- ^ Centos version devel minimal
          | Ubi Natural    -- ^ RHEL version
          | VersionNumber Natural -- ^ map to fedora or centos
          | Other String -- ^ other distros
  deriving (Eq)

parseNat :: Parser Natural
parseNat = read <$> many1 digit

parseFedora :: Parser Dist
parseFedora = Fedora <$> (char 'f' *> parseNat)

parseELN :: Parser Dist
parseELN = string "eln" $> ELN

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

parseVersion :: Parser Dist
parseVersion =
  VersionNumber <$> parseNat

parseOther :: Parser Dist
parseOther = Other <$> many1 anyChar

-- FIXME also handle bare numbers
parseDist :: Parser Dist
parseDist = try parseFedora <|> try parseELN <|> try parseCentos
            <|> try parseUbi <|> parseVersion <|> parseOther

readDist :: String -> Dist
readDist s =
  case parse parseDist "" s of
    Left err -> error' $ show err
    Right d -> d

distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer ELN = "registry.fedoraproject.org/eln:latest"
-- FIXME make 11 default to development
distContainer (Centos n d m) = "centos:stream" ++ show n ++ (mwhen d "-development") ++ (mwhen m "-minimal")
distContainer (VersionNumber n) =
  distContainer $
  case compare n 11 of
    LT -> Centos n False False
    EQ -> ELN
    GT -> Fedora n
distContainer (Ubi v) = "ubi" ++ show v
distContainer (Other s) = s
