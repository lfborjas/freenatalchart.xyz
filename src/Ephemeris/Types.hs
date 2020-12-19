{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Ephemeris.Types
  (
  -- re-exports from SwissEphemeris
    Angles(..)
  , Planet(
      Sun, Moon, Mercury, Venus, Mars,
      Jupiter, Saturn, Uranus, Neptune, Pluto,
      MeanNode, TrueNode, MeanApog, OscuApog,
      Chiron
    )
  , EclipticPosition(..)
  , HouseSystem(..)
  , HouseCusp
  , ZodiacSignName(..)
  , SplitDegreesOption(..)
  , JulianTime(..)
  , GeographicPosition(..)
  , EquatorialPosition(..)
  , ObliquityInformation(..)
  , CuspsCalculation(..)
  , LongitudeComponents(..)
  -- own types
  , HasLongitude(..)
  , HasLabel(..)
  , Element(..)
  , ZodiacSign(..)
  , HouseNumber(..)
  , House(..)
  , AspectName(..)
  , AspectTemperament(..)
  , AspectType(..)
  , Aspect(..)
  , HoroscopeAspect(..)
  , Latitude(..)
  , Longitude(..)
  , PlanetPosition(..)
  , HoroscopeData(..)
  , Transit(..)
  -- smart constructors
  , mkLatitude
  , mkLongitude
  ,AngleAspect,PlanetaryAspect)
where

import SwissEphemeris
    (HouseCusp,  Angles(..),
      CuspsCalculation(..),
      EclipticPosition(..),
      EquatorialPosition(..),
      GeographicPosition(..),
      HouseSystem(..),
      JulianTime(..),
      LongitudeComponents(..),
      ObliquityInformation(..),
      Planet(..),
      SplitDegreesOption(..),
      ZodiacSignName(..) ) 
import Utils ( maybeBetween )
import RIO.Time (UTCTime(..))


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

data HoroscopeAspect a b = HoroscopeAspect
    { aspect :: Aspect
    , bodies :: ( a, b )
    , aspectAngle :: Double
    , orb :: Double
    } deriving stock (Eq, Show)

type PlanetaryAspect = HoroscopeAspect PlanetPosition PlanetPosition
type AngleAspect = HoroscopeAspect PlanetPosition House

newtype Latitude = Latitude {unLatitude :: Double}
    deriving newtype (Eq, Show, Num, Ord)

newtype Longitude = Longitude {unLongitude :: Double}
    deriving newtype (Eq, Show, Num, Ord)

-- ranges from this wrong answer that turned out to be right for me:
-- https://stackoverflow.com/a/23914607
mkLatitude :: Double -> Maybe Latitude
mkLatitude l = 
   maybeBetween ((-90.0), 90.0) l >>= (Just . Latitude)

mkLongitude :: Double -> Maybe Longitude
mkLongitude l =
  maybeBetween ((-180.0), 180.0) l >>= (Just . Longitude)

instance HasLongitude Longitude where
  getLongitude = id
  getLongitudeRaw = unLongitude

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
  , horoscopePlanetaryAspects :: [PlanetaryAspect]
  , horoscopeAngleAspects :: [AngleAspect]
  , horoscopeUniversalTime :: UTCTime
  , horoscopeJulianTime :: JulianTime
  -- TODO: delta time?
  } deriving (Eq, Show)

data Transit a = Transit
  {
    transiting :: PlanetPosition
  , transited :: a
  , transitStarts :: Maybe UTCTime
  , transitEnds :: Maybe UTCTime
  , approximateTriggers :: [UTCTime]
  } deriving stock (Eq, Show)
