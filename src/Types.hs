{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import Ephemeris.Types ( Latitude, Longitude )
import System.Envy (Var(..), FromEnv)
import RIO.Time (LocalTime)
import RIO.Char (toLower)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneDatabase)
import Utils ( maybeBetween )

-- | Reader context for the server

type EphemeridesPath = FilePath
type EphemerisDatabase = FilePath

data Environment 
  = Development
  | Test
  | Production
  deriving stock (Eq, Show, Enum, Read)

instance Var Environment where
  toVar = show
  fromVar = readMaybe

data AppContext = AppContext
  { appLogFunc :: !LogFunc
  , appPort :: !Int
  , appEphePath :: !EphemeridesPath
  , appGeocodeApiKey :: !String
  , appTimeZoneDatabase :: !TimeZoneDatabase
  , appEnvironment :: !Environment
  , appStaticRoot :: !FilePath
  , appEphemerisDatabase :: !EphemerisDatabase
  -- Add other app-specific configuration information here
  }

instance HasLogFunc AppContext where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class HasEphePath env where
  ephePathL :: Lens' env FilePath
instance HasEphePath AppContext where
  ephePathL = lens appEphePath (\x y -> x { appEphePath = y})

class HasPort env where
  portL :: Lens' env Int
instance HasPort AppContext where
  portL = lens appPort (\x y -> x { appPort = y})

class HasGeocodeApiKey env where
  geocodeApiKeyL :: Lens' env String
instance HasGeocodeApiKey AppContext where
  geocodeApiKeyL = lens appGeocodeApiKey (\x y -> x { appGeocodeApiKey = y})

class HasTimeZoneDatabase env where
  timeZoneDatabaseL :: Lens' env TimeZoneDatabase
instance HasTimeZoneDatabase AppContext where
  timeZoneDatabaseL = lens appTimeZoneDatabase (\x y -> x { appTimeZoneDatabase = y})

class HasEnvironment env where
  environmentL :: Lens' env Environment
instance HasEnvironment AppContext where
  environmentL = lens appEnvironment (\x y -> x { appEnvironment = y})

class HasStaticRoot env where
  staticRootL :: Lens' env FilePath
instance HasStaticRoot AppContext where
  staticRootL = lens appStaticRoot (\x y -> x {appStaticRoot = y})

class HasEphemerisDatabase env where
  ephemerisDatabaseL :: Lens' env EphemerisDatabase
instance HasEphemerisDatabase AppContext where
  ephemerisDatabaseL = lens appEphemerisDatabase (\x y -> x {appEphemerisDatabase = y})

-- | Options that can be set as environment variables
data AppOptions = AppOptions
  {
    port :: Int
  , ephePath :: FilePath
  , geocodeApiKey :: String
  , timezoneDatabaseFile :: FilePath
  , epheDbFile :: FilePath
  , deployEnv :: Environment
  } deriving (Generic, Show)

defaultConfig :: AppOptions
defaultConfig = AppOptions { 
  port = 3000,
  ephePath = "./config", 
  geocodeApiKey = "",
  timezoneDatabaseFile = "./config/timezone21.bin",
  epheDbFile = "./config/precalculated_ephemeris.db",
  deployEnv = Development
}

instance FromEnv AppOptions

-- | Reader context for chart rendering

data ChartContext = ChartContext
  {
    chartAscendantOffset :: !Double
  , chartZodiacCircleRadius :: !Double
  , chartAspectCircleRadius :: !Double
  , chartPlanetCircleRadius :: !Double
  , chartHouseClassPrefix :: !String
  , chartPlanetClassPrefix :: !String
  } deriving (Show)

class HasAscendantOffset env where
  ascendantOffsetL :: Lens' env Double
instance HasAscendantOffset ChartContext where
  ascendantOffsetL = lens chartAscendantOffset
                          (\x y -> x {chartAscendantOffset = y})

class HasZodiacCircleRadius env where
  zodiacCircleRadiusL :: Lens' env Double
instance HasZodiacCircleRadius ChartContext where
  zodiacCircleRadiusL = lens chartZodiacCircleRadius
                             (\x y -> x{chartZodiacCircleRadius = y})

class HasAspectCircleRadius env where
  aspectCircleRadiusL :: Lens' env Double
instance HasAspectCircleRadius ChartContext where
  aspectCircleRadiusL = lens chartAspectCircleRadius
                             (\x y -> x{chartAspectCircleRadius = y})

class HasPlanetCircleRadius env where
  planetCircleRadiusL :: Lens' env Double
instance HasPlanetCircleRadius ChartContext where
  planetCircleRadiusL = lens chartPlanetCircleRadius
                             (\x y -> x{chartPlanetCircleRadius = y})



-- "safe by construction" newtypes



newtype Year = Year Int
    deriving newtype (Eq, Show, Num, Read, Ord)

mkYear :: Int -> Maybe Year
mkYear y = 
  maybeBetween (1800, 2399) y >>= (Just . Year)

newtype Month = Month Int
    deriving newtype (Eq, Show, Read, Num, Ord)

mkMonth :: Int -> Maybe Month
mkMonth m =
  maybeBetween (1, 12) m >>= (Just . Month)

newtype Day = Day Int
    deriving newtype (Eq, Show, Num, Read, Ord)

mkDay :: Int -> Maybe Day
mkDay d =
  maybeBetween (1, 31) d >>= (Just . Day)

newtype Hour = Hour Int
    deriving newtype (Eq, Show, Num, Read, Ord)

mkHour :: Int -> Maybe Hour
mkHour h =
  maybeBetween (1, 12) h >>= (Just . Hour)

newtype Minute = Minute Int
    deriving newtype (Eq, Show, Num)

mkMinute :: Int -> Maybe Minute
mkMinute m =
  maybeBetween (0, 60) m >>= (Just . Minute)

newtype DayPart = DayPart { unDayPart :: String }
    deriving newtype (Eq, Show, IsString)

mkDayPart :: String -> Maybe DayPart
mkDayPart x = 
  if (map toLower s) `elem` ["am", "pm"] then
    Just $ DayPart s
  else
    Nothing
  where 
    s = take 2 x

data DateParts = DateParts 
    {
        year :: Year
    ,   month :: Month
    ,   day :: Day
    ,   hour :: Hour
    ,   minute :: Minute
    ,   dayPart :: DayPart
    } deriving (Eq, Show)

data Location = Location
    {
        locationInput :: Text
    ,   locationLatitude :: Latitude
    ,   locationLongitude :: Longitude
    } deriving (Eq, Show)

data BirthData = BirthData
    {
        birthLocation :: Location
    ,   birthLocalTime :: LocalTime
    } deriving (Eq, Show)
