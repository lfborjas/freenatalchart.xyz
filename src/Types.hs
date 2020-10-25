{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import SwissEphemeris
import System.Envy (Var(..), FromEnv)
import RIO.Time (UTCTime, LocalTime)
import RIO.Char (toLower)
import Data.Time.LocalTime.TimeZone.Detect (TimeZoneDatabase)

-- | Reader context for the server

type EphemeridesPath = FilePath

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
  , appAlgoliaAppId :: !String
  , appAlgoliaAppKey :: !String
  , appTimeZoneDatabase :: !TimeZoneDatabase
  , appEnvironment :: !Environment
  , appStaticRoot :: !FilePath
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

class HasEnvironment env where
  environmentL :: Lens' env Environment
instance HasEnvironment AppContext where
  environmentL = lens appEnvironment (\x y -> x { appEnvironment = y})

class HasStaticRoot env where
  staticRootL :: Lens' env FilePath
instance HasStaticRoot AppContext where
  staticRootL = lens appStaticRoot (\x y -> x {appStaticRoot = y})

-- | Options that can be set as environment variables
data AppOptions = AppOptions
  {
    port :: Int
  , ephePath :: FilePath
  , algoliaAppId :: String
  , algoliaAppKey :: String
  , timezoneDatabaseFile :: FilePath
  , deployEnv :: Environment
  } deriving (Generic, Show)

defaultConfig :: AppOptions
defaultConfig = AppOptions { 
  port = 3000,
  ephePath = "./config", 
  algoliaAppId = "", 
  algoliaAppKey = "", 
  timezoneDatabaseFile = "./config/timezone21.bin",
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
class Eq a => HasLongitude a where
  getLongitude :: a -> Longitude
  getLongitudeRaw :: a -> Double
  getLongitudeRaw = unLongitude . getLongitude

class Show a => HasLabel a where
  label :: a -> String
  label = show

instance HasLabel Planet where
  label MeanApog = "Lilith"
  label MeanNode = "Mean Node"
  label p = show p

instance HasLabel ZodiacSignName

data Element
  = Earth
  | Air
  | Fire
  | Water
  deriving stock (Eq, Show, Enum, Bounded)
  deriving anyclass HasLabel

data ZodiacSign = ZodiacSign {
  name :: ZodiacSignName
, zodiacLongitude :: Longitude
, zodiacElement :: Element
} deriving stock (Eq, Show)

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
  deriving stock (Eq, Show, Ord, Enum, Bounded)
  deriving anyclass HasLabel

data House = House
  {
    houseNumber :: HouseNumber
  , houseCusp :: Longitude
  , houseDeclination :: Double
  } deriving stock (Eq, Show)

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
    deriving stock (Eq, Show, Ord, Enum, Bounded)
    deriving anyclass HasLabel


data AspectTemperament
    = Analytical -- "Disharmonious"
    | Synthetic -- "Harmonious"
    | Neutral
    deriving stock (Eq, Show, Ord, Enum, Bounded)
    deriving anyclass HasLabel

data AspectType
  = Major
  | Minor
  deriving stock (Eq, Show, Ord, Enum)

data Aspect = Aspect 
  { aspectName :: AspectName
  , maxOrb :: Double
  , angle :: Double
  , temperament :: AspectTemperament 
  , aspectType :: AspectType
  }
    deriving stock (Eq, Show)

majorAspects :: [Aspect]
majorAspects =
    [ Aspect{ aspectType = Major, aspectName = Conjunction, angle = 0.0, maxOrb = 10.0, temperament = Synthetic }
    , Aspect{ aspectType = Major, aspectName = Sextile, angle = 60.0, maxOrb = 6.0, temperament = Synthetic }
    , Aspect{ aspectType = Major, aspectName = Square, angle = 90.0, maxOrb = 10.0, temperament = Analytical }
    , Aspect{ aspectType = Major, aspectName = Trine, angle = 120.0, maxOrb = 10.0, temperament = Synthetic }
    , Aspect{ aspectType = Major, aspectName = Opposition, angle = 180.0, maxOrb = 10.0, temperament = Analytical }
    ]


minorAspects :: [Aspect]
minorAspects =
    [ Aspect { aspectType = Minor, aspectName = SemiSquare, angle = 45.0, maxOrb = 3.0, temperament = Analytical }
    , Aspect { aspectType = Minor, aspectName = Sesquisquare, angle = 135.0, maxOrb = 3.0, temperament = Analytical }
    , Aspect { aspectType = Minor, aspectName = SemiSextile, angle = 30.0, maxOrb = 3.0, temperament = Neutral }
    , Aspect { aspectType = Minor, aspectName = Quincunx, angle = 150.0, maxOrb = 3.0, temperament = Neutral }
    , Aspect { aspectType = Minor, aspectName = Quintile, angle = 72.0, maxOrb = 2.0, temperament = Synthetic }
    , Aspect { aspectType = Minor, aspectName = BiQuintile, angle = 144.0, maxOrb = 2.0, temperament = Synthetic }
    ]

defaultAspects :: [Aspect]
defaultAspects = majorAspects <> minorAspects

data HoroscopeAspect a b = HoroscopeAspect
    { aspect :: Aspect
    , bodies :: ( a, b )
    , aspectAngle :: Double
    , orb :: Double
    } deriving stock (Eq, Show)

-- extensions to SwissEphemeris

-- in addition to the traditional solar system planets (+ Pluto,)
-- we're interested in the MeanNode, the Mean Lunar Apogee (Lilith)
-- and the asteroid Chiron.
defaultPlanets :: [Planet]
defaultPlanets = [Sun .. Pluto] <> [MeanNode, MeanApog, Chiron]

data PlanetPosition = PlanetPosition 
  { 
    planetName :: Planet
  , planetLat :: Latitude
  , planetLng :: Longitude
  , planetLngSpeed :: Double
  , planetDeclination :: Double
  } deriving stock (Eq, Show)

instance HasLongitude PlanetPosition where
    getLongitude = planetLng

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
  -- TODO: delta time?
  } deriving (Eq, Show)


mkEcliptic :: EclipticPosition
mkEcliptic = EclipticPosition 0 0 0 0 0 0

-- "safe by construction" newtypes

between :: Ord a => (a, a) -> a -> Bool
between (begin, end) x =
  x >= begin && x<= end

maybeBetween :: Ord a => (a, a) -> a -> Maybe a
maybeBetween range x = 
  if (between range x) then Just x else Nothing

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

newtype Latitude = Latitude {unLatitude :: Double}
    deriving newtype (Eq, Show, Num, Ord)

-- ranges from this wrong answer that turned out to be right for me:
-- https://stackoverflow.com/a/23914607
mkLatitude :: Double -> Maybe Latitude
mkLatitude l = 
   maybeBetween ((-90.0), 90.0) l >>= (Just . Latitude)

newtype Longitude = Longitude {unLongitude :: Double}
    deriving newtype (Eq, Show, Num, Ord)

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
