module Datum where

import Ellipsoid

{- |
   To represent a set of parameters for describing a particular datum,
   including a name, the reference ellipsoid used and the seven parameters
   required to translate co-ordinates in this datum to the WGS84 datum.
-}
data Datum = Datum { name :: String -- ^ The name of this Datum.
                   , ellipsoid :: Ellipsoid -- ^ The reference ellipsoid associated with this Datum.
                   {-|
                     Translation along the x-axis for use in 7-parameter Helmert
                     transformations. This value should be used to convert a co-ordinate in a
                     given datum to the WGS84 datum.
                   -}
                   , dx :: Double
                   {-|
                     Translation along the y-axis for use in 7-parameter Helmert
                     transformations. This value should be used to convert a co-ordinate in a
                     given datum to the WGS84 datum.
                   -}
                   , dy :: Double
                   {-|
                     Translation along the z-axis for use in 7-parameter Helmert
                     transformations. This value should be used to convert a co-ordinate in a
                     given datum to the WGS84 datum.
                   -}
                   , dz :: Double
                   {-|
                     Scale factor for use in 7-parameter Helmert transformations. This value
                     should be used to convert a co-ordinate in a given datum to the WGS84
                     datum.
                   -}
                   , ds :: Double
                   {-|
                     Rotation about the x-axis for use in 7-parameter Helmert transformations.
                     This value should be used to convert a co-ordinate in a given datum to the
                     WGS84 datum.
                   -}
                   , rx :: Double
                   {-|
                     Rotation about the y-axis for use in 7-parameter Helmert transformations.
                     This value should be used to convert a co-ordinate in a given datum to the
                     WGS84 datum.
                   -}
                   , ry :: Double
                   {-|
                     Rotation about the z-axis for use in 7-parameter Helmert transformations.
                     This value should be used to convert a co-ordinate in a given datum to the
                     WGS84 datum.
                   -}
                   , rz :: Double
                   } deriving (Eq, Show)

-- instance Show Datum where
--   show Datum{name = n, ellipsoid = ell, dx = dx, dy = dy, dz = dz,
--       ds = ds, rx = rx, ry = ry, rz = rz} =
--     n ++ " " ++ show ell ++ " dx=" ++ show dx ++ " dy=" ++ show dy ++
--     " dz=" ++ show dz ++ " ds=" ++ show ds ++ " rx=" ++ show rx ++ " ry=" ++ show ry ++ " rz=" ++ show rz


-- | Pre-determined data:

etrf89Datum :: Datum
etrf89Datum = Datum { name = "European Terrestrial Reference Frame (ETRF89)"
                    , ellipsoid = wgs84Ellipsoid
                    , dx = 0.0
                    , dy = 0.0
                    , dz = 0.0
                    , ds = 0.0
                    , rx = 0.0
                    , ry = 0.0
                    , rz = 0.0
                    }

ireland1965Datum :: Datum
ireland1965Datum = Datum { name = "Ireland 1965"
                         , ellipsoid = modifiedAiryEllipsoid
                         , dx = 482.53
                         , dy = -130.596
                         , dz = 564.557
                         , ds = 8.15
                         , rx = -1.042
                         , ry = -0.214
                         , rz = -0.631
                         }

osgb36Datum :: Datum
osgb36Datum = Datum { name = "Ordnance Survey of Great Britain 1936 (OSGB36)"
                    , ellipsoid = airy1830Ellipsoid
                    , dx = 446.448
                    , dy = -125.157
                    , dz = 542.06
                    , ds = -20.4894
                    , rx = 0.1502
                    , ry = 0.2470
                    , rz = 0.8421
                    }

wgs84Datum :: Datum
wgs84Datum = Datum { name = "World Geodetic System 1984 (WGS84)"
                   , ellipsoid = wgs84Ellipsoid
                   , dx = 0.0
                   , dy = 0.0
                   , dz = 0.0
                   , ds = 1.0
                   , rx = 0.0
                   , ry = 0.0
                   , rz = 0.0
                   }

nad27GreenlandDatum :: Datum
nad27GreenlandDatum = Datum { name = "North American Datum 1927 (NAD27) - Greenland"
                            , ellipsoid = clarke1866Ellipsoid
                            , dx = 11.0
                            , dy = 114.0
                            , dz = 195.0
                            , ds = 0.0
                            , rx = 0.0
                            , ry = 0.0
                            , rz = 0.0
                            }


nad27WesternUSDatum :: Datum
nad27WesternUSDatum = Datum { name = "North American Datum 1927 (NAD27) - Western US"
                            , ellipsoid = clarke1866Ellipsoid
                            , dx = -8.0
                            , dy = 159.0
                            , dz = 175.0
                            , ds = 0.0
                            , rx = 0.0
                            , ry = 0.0
                            , rz = 0.0
                            }


nad27AlaskaDatum :: Datum
nad27AlaskaDatum = Datum { name = "North American Datum 1927 (NAD27) - Alaska"
                         , ellipsoid = clarke1866Ellipsoid
                         , dx = -5.0
                         , dy = 135.0
                         , dz = 172.0
                         , ds = 0.0
                         , rx = 0.0
                         , ry = 0.0
                         , rz = 0.0
                         }


nad27CentralAmericaDatum :: Datum
nad27CentralAmericaDatum = Datum { name = "North American Datum 1927 (NAD27) - Central America"
                                 , ellipsoid = clarke1866Ellipsoid
                                 , dx = 0.0
                                 , dy = 125.0
                                 , dz = 194.0
                                 , ds = 0.0
                                 , rx = 0.0
                                 , ry = 0.0
                                 , rz = 0.0
                                 }


nad27SanSalvadorDatum :: Datum
nad27SanSalvadorDatum = Datum { name = "North American Datum 1927 (NAD27) - San Salvador"
                              , ellipsoid = clarke1866Ellipsoid
                              , dx = 1.0
                              , dy = 140.0
                              , dz = 165.0
                              , ds = 0.0
                              , rx = 0.0
                              , ry = 0.0
                              , rz = 0.0
                              }


nad27AlbertaBritishColumbiaDatum :: Datum
nad27AlbertaBritishColumbiaDatum = Datum { name = "North American Datum 1927 (NAD27) - Alberta and British Columbia"
                                         , ellipsoid = clarke1866Ellipsoid
                                         , dx = -7.0
                                         , dy = 162.0
                                         , dz = 188.0
                                         , ds = 0.0
                                         , rx = 0.0
                                         , ry = 0.0
                                         , rz = 0.0
                                         }


nad27CanadaEastDatum :: Datum
nad27CanadaEastDatum = Datum { name = "North American Datum 1927 (NAD27) - Canada East"
                             , ellipsoid = clarke1866Ellipsoid
                             , dx = -22.0
                             , dy = 160.0
                             , dz = 190.0
                             , ds = 0.0
                             , rx = 0.0
                             , ry = 0.0
                             , rz = 0.0
                             }


nad27ContiguousUSDatum :: Datum
nad27ContiguousUSDatum = Datum { name = "North American Datum 1927 (NAD27) - Contiguous United States"
                               , ellipsoid = clarke1866Ellipsoid
                               , dx = -8.0
                               , dy = 160.0
                               , dz = 176.0
                               , ds = 0.0
                               , rx = 0.0
                               , ry = 0.0
                               , rz = 0.0
                               }


nad27MexicoDatum :: Datum
nad27MexicoDatum = Datum { name = "North American Datum 1927 (NAD27) - Mexico"
                         , ellipsoid = clarke1866Ellipsoid
                         , dx = -12.0
                         , dy = 130.0
                         , dz = 190.0
                         , ds = 0.0
                         , rx = 0.0
                         , ry = 0.0
                         , rz = 0.0
                         }


nad27BahamasDatum :: Datum
nad27BahamasDatum = Datum { name = "North American Datum 1927 (NAD27) - Bahamas"
                          , ellipsoid = clarke1866Ellipsoid
                          , dx = -4.0
                          , dy = 154.0
                          , dz = 178.0
                          , ds = 0.0
                          , rx = 0.0
                          , ry = 0.0
                          , rz = 0.0
                          }


nad27CanadaNWTerritoryDatum :: Datum
nad27CanadaNWTerritoryDatum = Datum { name = "North American Datum 1927 (NAD27) - Canada NW Territory"
                                    , ellipsoid = clarke1866Ellipsoid
                                    , dx = 4.0
                                    , dy = 159.0
                                    , dz = 188.0
                                    , ds = 0.0
                                    , rx = 0.0
                                    , ry = 0.0
                                    , rz = 0.0
                                    }


nad27CanadaManitobaOntarioDatum :: Datum
nad27CanadaManitobaOntarioDatum = Datum { name = "North American Datum 1927 (NAD27) - Canada Manitoba/Ontario"
                                        , ellipsoid = clarke1866Ellipsoid
                                        , dx = -9.0
                                        , dy = 157.0
                                        , dz = 184.0
                                        , ds = 0.0
                                        , rx = 0.0
                                        , ry = 0.0
                                        , rz = 0.0
                                        }


nad27CanadaDatum :: Datum
nad27CanadaDatum = Datum { name = "North American Datum 1927 (NAD27) - Canada"
                         , ellipsoid = clarke1866Ellipsoid
                         , dx = -10.0
                         , dy = 158.0
                         , dz = 187.0
                         , ds = 0.0
                         , rx = 0.0
                         , ry = 0.0
                         , rz = 0.0
                         }


nad27CanadaYukonDatum :: Datum
nad27CanadaYukonDatum = Datum { name = "North American Datum 1927 (NAD27) - Canada Yukon"
                              , ellipsoid = clarke1866Ellipsoid
                              , dx = -7.0
                              , dy = 139.0
                              , dz = 181.0
                              , ds = 0.0
                              , rx = 0.0
                              , ry = 0.0
                              , rz = 0.0
                              }


nad27CubaDatum :: Datum
nad27CubaDatum = Datum { name = "North American Datum 1927 (NAD27) - Cuba"
                       , ellipsoid = clarke1866Ellipsoid
                       , dx = -9.0
                       , dy = 152.0
                       , dz = 178.0
                       , ds = 0.0
                       , rx = 0.0
                       , ry = 0.0
                       , rz = 0.0
                       }


nad27AleutianWestDatum :: Datum
nad27AleutianWestDatum = Datum { name = "North American Datum 1927 (NAD27) - Aleutian West"
                               , ellipsoid = clarke1866Ellipsoid
                               , dx = 2.0
                               , dy = 204.0
                               , dz = 105.0
                               , ds = 0.0
                               , rx = 0.0
                               , ry = 0.0
                               , rz = 0.0
                               }


nad27AleutianEastDatum :: Datum
nad27AleutianEastDatum = Datum { name = "North American Datum 1927 (NAD27) - Aleutian East"
                               , ellipsoid = clarke1866Ellipsoid
                               , dx = -2.0
                               , dy = 152.0
                               , dz = 149.0
                               , ds = 0.0
                               , rx = 0.0
                               , ry = 0.0
                               , rz = 0.0
                               }


nad27CanalZoneDatum :: Datum
nad27CanalZoneDatum = Datum { name = "North American Datum 1927 (NAD27) - Canal Zone"
                            , ellipsoid = clarke1866Ellipsoid
                            , dx = 0.0
                            , dy = 125.0
                            , dz = 201.0
                            , ds = 0.0
                            , rx = 0.0
                            , ry = 0.0
                            , rz = 0.0
                            }


nad27EasternUSDatum :: Datum
nad27EasternUSDatum = Datum { name = "North American Datum 1927 (NAD27) - Eastern US"
                            , ellipsoid = clarke1866Ellipsoid
                            , dx = -9.0
                            , dy = 161.0
                            , dz = 179.0
                            , ds = 0.0
                            , rx = 0.0
                            , ry = 0.0
                            , rz = 0.0
                            }


nad27CaribbeanDatum :: Datum
nad27CaribbeanDatum = Datum { name = "North American Datum 1927 (NAD27) - Caribbean"
                            , ellipsoid = clarke1866Ellipsoid
                            , dx = -3.0
                            , dy = 142.0
                            , dz = 183.0
                            , ds = 0.0
                            , rx = 0.0
                            , ry = 0.0
                            , rz = 0.0
                            }
