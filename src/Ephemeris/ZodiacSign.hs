{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ephemeris.ZodiacSign where

import Import
import Ephemeris.Types
import RIO.List (headMaybe)
import Ephemeris.Utils (splitDegreesZodiac)


westernZodiacSigns :: [ZodiacSign]
westernZodiacSigns =
    [ZodiacSign { name = Aries, zodiacLongitude = Longitude 0.0, zodiacElement = Fire }
    ,ZodiacSign { name = Taurus, zodiacLongitude = Longitude 30.0, zodiacElement = Earth }
    ,ZodiacSign { name = Gemini, zodiacLongitude = Longitude 60.0, zodiacElement = Air }
    ,ZodiacSign { name = Cancer, zodiacLongitude = Longitude 90.0, zodiacElement = Water }
    ,ZodiacSign { name = Leo, zodiacLongitude = Longitude 120.0, zodiacElement = Fire }
    ,ZodiacSign { name = Virgo, zodiacLongitude = Longitude 150.0, zodiacElement = Earth }
    ,ZodiacSign { name = Libra, zodiacLongitude = Longitude 180.0, zodiacElement = Air }
    ,ZodiacSign { name = Scorpio, zodiacLongitude = Longitude 210.0, zodiacElement = Water }
    ,ZodiacSign { name = Sagittarius, zodiacLongitude = Longitude 240.0, zodiacElement = Fire }
    ,ZodiacSign { name = Capricorn, zodiacLongitude = Longitude 270.0, zodiacElement = Earth }
    ,ZodiacSign { name = Aquarius, zodiacLongitude = Longitude 300.0, zodiacElement = Air }
    ,ZodiacSign { name = Pisces, zodiacLongitude = Longitude 330.0, zodiacElement = Water }
    ]

planetsBySign :: [PlanetPosition] -> [(ZodiacSignName, PlanetPosition)]
planetsBySign planets' =
  map bySign planets'
  & catMaybes  

planetsInSign :: [(ZodiacSignName, PlanetPosition)] -> ZodiacSignName -> [PlanetPosition]
planetsInSign = filterSign

housesBySign :: [House] -> [(ZodiacSignName, House)]
housesBySign houses' =
  map bySign houses'
  & catMaybes

housesInSign :: [(ZodiacSignName, House)] -> ZodiacSignName -> [House]
housesInSign = filterSign

filterSign :: [(ZodiacSignName, a)] -> ZodiacSignName -> [a]
filterSign mapped sgn = map snd . filter (\(s,_) -> s == sgn) $ mapped

bySign :: HasLongitude a => a -> Maybe (ZodiacSignName, a)
bySign p = maybe Nothing (\z -> Just (z, p)) (longitudeZodiacSign . splitDegreesZodiac . getLongitudeRaw $ p)

findSunSign :: [PlanetPosition] -> Maybe ZodiacSignName
findSunSign positions =
  positions
    & dropWhile (\PlanetPosition {..} -> planetName /= Sun)
    & headMaybe
    & fmap (longitudeZodiacSign . splitDegreesZodiac . getLongitudeRaw . planetLng)
    & maybe Nothing id

findAscendant :: [House] -> Maybe ZodiacSignName
findAscendant houses' =
  houses'
    & dropWhile (\House {..} -> houseNumber /= I)
    & headMaybe
    & fmap (longitudeZodiacSign . splitDegreesZodiac . getLongitudeRaw . houseCusp)
    & maybe Nothing id
