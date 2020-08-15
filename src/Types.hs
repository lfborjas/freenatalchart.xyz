{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

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


-- domain specific types
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
    ,ZodiacSign { name = Taurus, zodiacLongitude = 30.0, zodiacElement = Earth }
    ,ZodiacSign { name = Gemini, zodiacLongitude = 60.0, zodiacElement = Air }
    ,ZodiacSign { name = Cancer, zodiacLongitude = 90.0, zodiacElement = Water }
    ,ZodiacSign { name = Leo, zodiacLongitude = 120.0, zodiacElement = Fire }
    ,ZodiacSign { name = Virgo, zodiacLongitude = 150.0, zodiacElement = Earth }
    ,ZodiacSign { name = Libra, zodiacLongitude = 180.0, zodiacElement = Air }
    ,ZodiacSign { name = Scorpio, zodiacLongitude = 210.0, zodiacElement = Water }
    ,ZodiacSign { name = Sagittarius, zodiacLongitude = 240.0, zodiacElement = Fire }
    ,ZodiacSign { name = Capricorn, zodiacLongitude = 270.0, zodiacElement = Earth }
    ,ZodiacSign { name = Aquarius, zodiacLongitude = 300.0, zodiacElement = Air }
    ,ZodiacSign { name = Pisces, zodiacLongitude = 330.0, zodiacElement = Water }
    ]
