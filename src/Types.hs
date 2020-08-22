{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process
import SwissEphemeris (HouseSystem, Angles(..), Planet(..), Coordinates(..))

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

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

type Longitude = Double

data ZodiacSign = ZodiacSign {
  name :: ZodiacSignName
, zodiacLongitude :: Longitude
, zodiacElement :: Element
} deriving (Eq, Show)

westernZodiacSigns :: [ZodiacSign]
westernZodiacSigns =
    [ZodiacSign { name = Aries, zodiacLongitude = 0.0, zodiacElement = Fire }
    ,ZodiacSign { name = Taurus, zodiacLongitude = 30.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Gemini, zodiacLongitude = 60.0, zodiacElement = Air }
    ,ZodiacSign { name = Cancer, zodiacLongitude = 90.0, zodiacElement = Water }
    ,ZodiacSign { name = Leo, zodiacLongitude = 120.0, zodiacElement = Fire }
    ,ZodiacSign { name = Virgo, zodiacLongitude = 150.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Libra, zodiacLongitude = 180.0, zodiacElement = Air }
    ,ZodiacSign { name = Scorpio, zodiacLongitude = 210.0, zodiacElement = Water }
    ,ZodiacSign { name = Sagittarius, zodiacLongitude = 240.0, zodiacElement = Fire }
    ,ZodiacSign { name = Capricorn, zodiacLongitude = 270.0, zodiacElement = Types.Earth }
    ,ZodiacSign { name = Aquarius, zodiacLongitude = 300.0, zodiacElement = Air }
    ,ZodiacSign { name = Pisces, zodiacLongitude = 330.0, zodiacElement = Water }
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

instance HasLongitude House where
  getLongitude = houseCusp

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

-- additions to SwissEphemeris

data PlanetPosition = PlanetPosition 
  { 
    planetName :: Planet
  , planetCoordinates :: Coordinates
  } deriving (Eq, Show)

instance HasLongitude PlanetPosition where
    getLongitude (PlanetPosition _ coords) = lng coords

data HoroscopeData = HoroscopeData
  {
    horoscopePlanetPositions :: [PlanetPosition]
  , horoscopeAngles :: Angles
  , horoscopeHouses :: [House]
  , horoscopeSystem :: HouseSystem
  , horoscopePlanetaryAspects :: [HoroscopeAspect PlanetPosition PlanetPosition]
  , horoscopeAngleAspects :: [HoroscopeAspect PlanetPosition House]
  } deriving (Eq, Show)
