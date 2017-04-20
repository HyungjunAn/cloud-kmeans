{-# LANGUAGE DeriveGeneric #-}

module PointSum where

import Data.Binary
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Vector

--data PointSum = PointSum {-# UNPACK #-} !Int {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Generic)

data PointSum = PointSum Int Double Double deriving (Generic)

instance Binary PointSum
