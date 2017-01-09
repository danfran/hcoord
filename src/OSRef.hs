module OSRef where

data OSRef = OSRef { easting :: Double -- ^ The easting in metres relative to the origin of the British National Grid.
                   , northing :: Double  -- ^ The northing in metres relative to the origin of the British National Grid.
                   } deriving (Eq, Show)
