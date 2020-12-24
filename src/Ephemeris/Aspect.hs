{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Ephemeris.Aspect where

import Import
import Utils
import Ephemeris.Types
import Ephemeris.Utils
import RIO.List (headMaybe)

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

-- | Calculate aspects to use for transit insights.
-- Note that we use the default orbs.
-- However, to consider the aspect "active", we use a smaller orb, of 1 degree.
-- cf.: https://www.astro.com/astrowiki/en/Transit
aspectsForTransits :: [Aspect]
aspectsForTransits = defaultAspects-- map (\a -> a{maxOrb = 5.0}) majorAspects

-- TODO(luis) this may also suffer from the 0/360 false negative!
exactAspectAngle ::  (HasLongitude a) => HoroscopeAspect a b -> Longitude
exactAspectAngle (HoroscopeAspect aspect' (aspecting, _aspected) angle' orb') =
  if angle' >= (angle aspect') then
    Longitude $ aspectingLongitude - orb'
  else
    Longitude $ aspectingLongitude + orb'
  where
    aspectingLongitude = aspecting & getLongitudeRaw
  

aspects' :: (HasLongitude a, HasLongitude b) => [Aspect] -> [a] -> [b] -> [HoroscopeAspect a b]
aspects' possibleAspects bodiesA bodiesB =
  (concatMap aspectsBetween pairs) & catMaybes
  where
    pairs = [(x, y) | x <- bodiesA, y <- bodiesB]
    aspectsBetween bodyPair = map (haveAspect bodyPair) possibleAspects
    haveAspect (a, b) asp@Aspect {..} =
      let angleBefore = angularDifference (getLongitudeRaw a) (getLongitudeRaw b)
          orbBefore = (angle - (abs angleBefore)) & abs
          angleAfter = angularDifference (getLongitudeRaw b) (getLongitudeRaw a)
          orbAfter =  (angle - (abs angleAfter)) & abs
        in if orbAfter <= maxOrb
            then Just $ HoroscopeAspect {aspect = asp, bodies = (a, b), aspectAngle = angleAfter, orb = orbAfter}
            else if orbBefore <= maxOrb then
              Just $ HoroscopeAspect {aspect = asp, bodies = (a, b), aspectAngle = angleBefore, orb = orbBefore}
              else Nothing

aspects :: (HasLongitude a, HasLongitude b) => [a] -> [b] -> [HoroscopeAspect a b]
aspects = aspects' defaultAspects

-- | calculate aspects between the same set of planets. Unlike `transitingAspects`, don't
-- keep aspects of a planet with itself.
planetaryAspects :: [PlanetPosition] -> [HoroscopeAspect PlanetPosition PlanetPosition]
planetaryAspects ps = filter (\a -> (a & bodies & fst) /= (a & bodies & snd)) $ aspects ps $ rotateList 1 ps

celestialAspects :: [PlanetPosition] -> Angles -> [HoroscopeAspect PlanetPosition House]
celestialAspects ps as = aspects ps (aspectableAngles as)

aspectableAngles :: Angles -> [House]
aspectableAngles Angles {..} = [House I (Longitude ascendant) 0, House X (Longitude mc) 0]

-- TODO(luis) should this exist? Should we just use normal majorAspects, or even the default
-- aspects? The only benefit here is that many aspects with bigger orbs are discarded
-- outright and as such we don't need to go calculate their activity (which has an IO cost,
-- at the DB,) but it does mean fewer aspects are shown when using this than in
-- e.g. the chart of the moment. On the other hand, it's less sifting through "inactive" aspects.
transitingAspects :: (HasLongitude a, HasLongitude b) => [a] -> [b] -> [HoroscopeAspect a b]
transitingAspects = aspects' aspectsForTransits

-- | Given a list of aspects, keep only major aspects.
-- useful as a helper when plotting/showing tables.
selectMajorAspects :: [HoroscopeAspect a b] -> [HoroscopeAspect a b]
selectMajorAspects = filter ((== Major) . aspectType . aspect)

-- | Select aspects with an orb of at most 1 degree. Useful for plotting.
selectExactAspects :: [HoroscopeAspect a b] -> [HoroscopeAspect a b]
selectExactAspects = filter ((<= 1) . orb)

-- TODO(luis): have even fancier select*aspects heuristics? Maybe
-- something like "only select applying aspects with orb smaller than X,
-- or separating aspects with orb smaller than Y?"
selectSignificantAspects :: [HoroscopeAspect a b] -> [HoroscopeAspect a b]
selectSignificantAspects = selectExactAspects

-- TODO(luis): these find* functions are _so_ wasteful. We could clearly do it in one pass vs. traverse the whole
-- list for each planet. However, I always find myself updating this file at midnight when my neurons are
-- not ready for the magic.
findAspectBetweenPlanets :: [HoroscopeAspect PlanetPosition PlanetPosition] -> Planet -> Planet -> Maybe (HoroscopeAspect PlanetPosition PlanetPosition)
findAspectBetweenPlanets aspectList pa pb =
  aspectList
    & filter (\HoroscopeAspect {..} -> (planetName . fst $ bodies, planetName . snd $ bodies) `elem` [(pa, pb), (pb, pa)])
    & headMaybe

findAspectWithPlanet :: [PlanetaryAspect] -> Planet -> Planet -> Maybe PlanetaryAspect 
findAspectWithPlanet aspectList aspecting aspected =
  aspectList
    & filter (\HoroscopeAspect {..} -> (planetName . fst $ bodies, planetName . snd $ bodies) == (aspecting, aspected))
    & headMaybe

findAspectWithAngle :: [HoroscopeAspect PlanetPosition House] -> Planet -> HouseNumber -> Maybe (HoroscopeAspect PlanetPosition House)
findAspectWithAngle aspectList pa hb =
  aspectList
    & filter (\HoroscopeAspect {..} -> (planetName . fst $ bodies, houseNumber . snd $ bodies) == (pa, hb))
    & headMaybe

findAspectsByName :: [HoroscopeAspect a b] -> AspectName -> [HoroscopeAspect a b]
findAspectsByName aspectList name =
  aspectList
    & filter (\HoroscopeAspect {..} -> (aspect & aspectName) == name)

-- | Is the aspecting body approaching, or leaving, exactitude?
-- NOTE: we assume that the aspected body is static, which is a correct
-- assumption for transits, in which the aspected natal bodies are fixed,
-- but it's not necessarily correct for natal charts, in which both
-- bodies were in motion.
-- More on these:
-- https://www.astro.com/astrowiki/en/Applying_Aspect
-- https://www.astro.com/astrowiki/en/Separating_Aspect
aspectPhase :: HoroscopeAspect PlanetPosition a -> AspectPhase
aspectPhase asp@HoroscopeAspect {..} =
  if isMovingTowards then
    Applying 
  else
    Separating
  where
    isMovingTowards = isDirect && isApproaching
    isDirect = (== 1) . signum . planetLngSpeed $ aspectingPlanet
    isApproaching = (< 1) . signum $ eclipticDifference aspectingPlanet aspectedPoint
    aspectingPlanet = bodies & fst
    aspectedPoint   = exactAspectAngle asp

-- TODO(luis): maybe we can use this in the aspect calculation, and anywhere
-- where we need to account for "0/360 jumps"?
-- still not sure on how sound the math is.
eclipticDifference :: (HasLongitude a, HasLongitude b) => a -> b -> Double
eclipticDifference a b =
  if ((abs diff) >= biggerThanAnyAspect) then
    lB - lA
  else
    diff
  where
    biggerThanAnyAspect = 200
    diff = lA - lB
    lA = getLongitudeRaw a
    lB = getLongitudeRaw b
