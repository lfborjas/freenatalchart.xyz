{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import SwissEphemeris (JulianTime, HouseSystem, Angles(..), Planet(..), Coordinates(..))
import System.Envy (FromEnv)
import RIO.Time (UTCTime, LocalTime)
import RIO.Char (toLower)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneDatabase)

-- | Reader context for the server

type EphemeridesPath = FilePath

data AppContext = AppContext
  { appLogFunc :: !LogFunc
  , appPort :: !Int
  , appEphePath :: !EphemeridesPath
  , appAlgoliaAppId :: !String
  , appAlgoliaAppKey :: !String
  , appTimeZoneDatabase :: !TimeZoneDatabase
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

class HasAlgoliaAppId env where
  algoliaAppIdL :: Lens' env String
instance HasAlgoliaAppId AppContext where
  algoliaAppIdL = lens appAlgoliaAppId (\x y -> x { appAlgoliaAppId = y})

class HasAlgoliaAppKey env where
  algoliaAppKeyL :: Lens' env String
instance HasAlgoliaAppKey AppContext where
  algoliaAppKeyL = lens appAlgoliaAppKey (\x y -> x { appAlgoliaAppKey = y})

class HasTimeZoneDatabase env where
  timeZoneDatabaseL :: Lens' env TimeZoneDatabase
instance HasTimeZoneDatabase AppContext where
  timeZoneDatabaseL = lens appTimeZoneDatabase (\x y -> x { appTimeZoneDatabase = y})


data AppOptions = AppOptions
  {
    port :: Int
  , ephePath :: FilePath
  , algoliaAppId :: String
  , algoliaAppKey :: String
  , timezoneDatabaseFile :: FilePath
  } deriving (Generic, Show)

defaultConfig :: AppOptions
defaultConfig = AppOptions { 
  port = 3000,
  ephePath = "./config", 
  algoliaAppId = "", 
  algoliaAppKey = "", 
  timezoneDatabaseFile = "./config/timezone21.bin"
}

instance FromEnv AppOptions

-- | Reader context for chart rendering

data ChartContext = ChartContext
  {
    chartAscendantOffset :: !Double
  , chartZodiacCircleRadius :: !Double
  , chartAspectCircleRadius :: !Double
  , chartPlanetCircleRadius :: !Double
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

-- domain specific types
class HasLongitude a where
  getLongitude :: a -> Longitude
  getLongitudeRaw :: a -> Double
  getLongitudeRaw = unLongitude . getLongitude

data ZodiacSignName
  = Aries
  | Taurus
  | Gemini
  | Cancer
  | Leo
  | Virgo
  | Libra
  | Scorpio
  | Sagittarius
  | Capricorn
  | Aquarius
  | Pisces
  deriving (Eq, Show, Enum, Bounded)

data Element
  = Earth
  | Air
  | Fire
  | Water
  deriving (Eq, Show, Enum, Bounded)

data ZodiacSign = ZodiacSign {
  name :: ZodiacSignName
, zodiacLongitude :: Longitude
, zodiacElement :: Element
} deriving (Eq, Show)

data ZodiacPosition = ZodiacPosition 
  {
    positionSign :: ZodiacSignName
  , positionDegrees :: Double
  , positionMinutes :: Int
  , positionSeconds :: Int
  } deriving (Eq, Show)

westernZodiacSigns :: [ZodiacSign]
westernZodiacSigns =
    [ZodiacSign { name = Aries, zodiacLongitude = Longitude 0.0, zodiacElement = Fire }
    ,ZodiacSign { name = Taurus, zodiacLongitude = Longitude 30.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Gemini, zodiacLongitude = Longitude 60.0, zodiacElement = Air }
    ,ZodiacSign { name = Cancer, zodiacLongitude = Longitude 90.0, zodiacElement = Water }
    ,ZodiacSign { name = Leo, zodiacLongitude = Longitude 120.0, zodiacElement = Fire }
    ,ZodiacSign { name = Virgo, zodiacLongitude = Longitude 150.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Libra, zodiacLongitude = Longitude 180.0, zodiacElement = Air }
    ,ZodiacSign { name = Scorpio, zodiacLongitude = Longitude 210.0, zodiacElement = Water }
    ,ZodiacSign { name = Sagittarius, zodiacLongitude = Longitude 240.0, zodiacElement = Fire }
    ,ZodiacSign { name = Capricorn, zodiacLongitude = Longitude 270.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Aquarius, zodiacLongitude = Longitude 300.0, zodiacElement = Air }
    ,ZodiacSign { name = Pisces, zodiacLongitude = Longitude 330.0, zodiacElement = Water }
    ]

data HouseNumber 
  = I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  | VIII
  | IX
  | X
  | XI
  | XII
  deriving (Eq, Show, Ord, Enum, Bounded)

data House = House
  {
    houseNumber :: HouseNumber
  , houseCusp :: Longitude
  } deriving (Eq, Show)

-- TODO(luis) fix this to be `Longitude houseCusp`?
instance HasLongitude House where
  getLongitude =  houseCusp

-- see: https://en.wikipedia.org/wiki/Astrological_aspect

data AspectName
    = Conjunction
    | Sextile
    | Square
    | Trine
    | Opposition
    | Quincunx
    | SemiSextile
    | Quintile
    | BiQuintile
    | Septile
    | SemiSquare
    | Novile
    | Sesquisquare -- Trioctile
    deriving (Eq, Show, Ord, Enum, Bounded)


data AspectTemperament
    = Analytical -- "Disharmonious"
    | Synthetic -- "Harmonious"
    | Neutral
    deriving (Eq, Show, Ord, Enum, Bounded)


data Aspect =
    Aspect { aspectName :: AspectName, maxOrb :: Double, angle :: Double, temperament :: AspectTemperament }
    deriving (Eq, Show)

majorAspects :: [Aspect]
majorAspects =
    [Aspect{ aspectName = Conjunction, angle = 0.0, maxOrb = 10.0, temperament = Synthetic }
    ,Aspect{ aspectName = Sextile, angle = 60.0, maxOrb = 6.0, temperament = Synthetic }
    ,Aspect{ aspectName = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
    ,Aspect{ aspectName = Trine, angle = 120.0, maxOrb = 10.0, temperament = Synthetic }
    ,Aspect{ aspectName = Opposition, angle = 180.0, maxOrb = 10.0, temperament = Analytical }
    ]


minorAspects :: [Aspect]
minorAspects =
    [ Aspect { aspectName = SemiSquare, angle = 45.0, maxOrb = 3.0, temperament = Analytical }
    , Aspect { aspectName = Sesquisquare, angle = 135.0, maxOrb = 3.0, temperament = Analytical }
    , Aspect { aspectName = SemiSextile, angle = 30.0, maxOrb = 3.0, temperament = Neutral }
    , Aspect { aspectName = Quincunx, angle = 150.0, maxOrb = 3.0, temperament = Neutral }
    , Aspect { aspectName = Quintile, angle = 72.0, maxOrb = 2.0, temperament = Synthetic }
    , Aspect { aspectName = BiQuintile, angle = 144.0, maxOrb = 2.0, temperament = Synthetic }
    ]

defaultAspects :: [Aspect]
defaultAspects = majorAspects <> minorAspects

data HoroscopeAspect a b = HoroscopeAspect
    { aspect :: Aspect
    , bodies :: ( a, b )
    , aspectAngle :: Double
    , orb :: Double
    } deriving (Eq, Show)

-- extensions to SwissEphemeris

data PlanetPosition = PlanetPosition 
  { 
    planetName :: Planet
  , planetCoordinates :: Coordinates
  } deriving (Eq, Show)

instance HasLongitude PlanetPosition where
    getLongitude (PlanetPosition _ coords) = Longitude $ lng coords

data HoroscopeData = HoroscopeData
  {
    horoscopePlanetPositions :: [PlanetPosition]
  , horoscopeAngles :: Angles
  , horoscopeHouses :: [House]
  , horoscopeSystem :: HouseSystem
  , horoscopePlanetaryAspects :: [HoroscopeAspect PlanetPosition PlanetPosition]
  , horoscopeAngleAspects :: [HoroscopeAspect PlanetPosition House]
  , horoscopeUniversalTime :: UTCTime
  , horoscopeJulianTime :: JulianTime
  } deriving (Eq, Show)


-- "safe by construction" newtypes

between :: Ord a => (a, a) -> a -> Bool
between (begin, end) x =
  x >= begin && x<= end

maybeBetween :: Ord a => (a, a) -> a -> Maybe a
maybeBetween range x = 
  if (between range x) then Just x else Nothing

newtype Year = Year Int
    deriving  (Eq, Show, Num, Read, Ord)

mkYear :: Int -> Maybe Year
mkYear y = 
  maybeBetween (1800, 2399) y >>= (Just . Year)

newtype Month = Month Int
    deriving (Eq, Show, Read, Num, Ord)

mkMonth :: Int -> Maybe Month
mkMonth m =
  maybeBetween (1, 12) m >>= (Just . Month)

newtype Day = Day Int
    deriving (Eq, Show, Num, Read, Ord)

mkDay :: Int -> Maybe Day
mkDay d =
  maybeBetween (1, 31) d >>= (Just . Day)

newtype Hour = Hour Int
    deriving (Eq, Show, Num, Read, Ord)

mkHour :: Int -> Maybe Hour
mkHour h =
  maybeBetween (1, 12) h >>= (Just . Hour)

newtype Minute = Minute Int
    deriving (Eq, Show, Num)

mkMinute :: Int -> Maybe Minute
mkMinute m =
  maybeBetween (0, 60) m >>= (Just . Minute)

newtype DayPart = DayPart { unDayPart :: String }
    deriving (Eq, Show, IsString)

mkDayPart :: String -> Maybe DayPart
mkDayPart x = 
  if (map toLower s) `elem` ["am", "pm"] then
    Just $ DayPart s
  else
    Nothing
  where 
    s = take 2 x

newtype Latitude = Latitude {unLatitude :: Double}
    deriving (Eq, Show, Num)

-- ranges from this wrong answer that turned out to be right for me:
-- https://stackoverflow.com/a/23914607
mkLatitude :: Double -> Maybe Latitude
mkLatitude l = 
   maybeBetween ((-90.0), 90.0) l >>= (Just . Latitude)

newtype Longitude = Longitude {unLongitude :: Double}
    deriving (Eq, Show, Num)

instance HasLongitude Longitude where
  getLongitude = id
  getLongitudeRaw = unLongitude

mkLongitude :: Double -> Maybe Longitude
mkLongitude l =
  maybeBetween ((-180.0), 180.0) l >>= (Just . Longitude)

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
