module Dist where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (*>))
#endif
import Text.Read
import Text.ParserCombinators.ReadP (char, string)

data Dist = Fedora Int -- ^ Fedora release
          | Centos Int -- ^ Centos version
          | Ubi Int    -- ^ RHEL version
  deriving (Eq)

instance Read Dist where
  readPrec = choice [pFedora, pC, pCentos, pUBI] where
    pFedora = Fedora <$> (lift (char 'f') *> readPrec)
    pC = Centos <$> (lift (char 'c') *> readPrec)
    pCentos = Centos <$> (lift (string "centos") *> readPrec)
    pUBI = Ubi <$> (lift (string "ubi") *> readPrec)

distContainer :: Dist -> String
distContainer (Fedora n) = "fedora:" ++ show n
distContainer (Centos n) = "centos:" ++ show n
distContainer (Ubi v) = "ubi" ++ show v
