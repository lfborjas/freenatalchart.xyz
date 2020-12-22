{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ephemeris.Transit where

import Numeric.MathFunctions.Constants (m_epsilon)
import Numeric.RootFinding
  ( RiddersParam (RiddersParam, riddersMaxIter, riddersTol),
    Root(..),
    Tolerance (RelTol),
    ridders,
  )
import SwissEphemeris (calculateEclipticPosition)
import System.IO.Unsafe (unsafePerformIO)
import Import
import Ephemeris.Types
import Control.Applicative
import Control.Monad (ap)
import Ephemeris.Internal.Approximations (maxSpeed)
import Database.SQLite.Simple (withConnection)
import Database.SQLite.Simple.Internal (Connection)
import Ephemeris.Aspect (exactAspectAngle)
import Ephemeris.Internal.Database (crossingCandidatesQuery, activityPeriodQuery)
import Ephemeris.Utils (julianToUTC)
import RIO.List (sortBy)
import RIO.Time (UTCTime)

-- | Given planetary aspects (in which it's always "transiting aspects transited",)
-- and a reference time, derive transit activity: when does it begin and end, and is it exact
-- within a day of the reference time?
transits :: EphemerisDatabase -> JulianTime -> [PlanetaryAspect] -> IO [PlanetaryTransit]
transits epheDB momentOfTransit planetaryAspects = 
  withConnection epheDB $ \conn -> do
    -- TODO: rank!
    allTransits <- mapM (transit conn momentOfTransit) planetaryAspects
    pure $ filter (isActiveTransit (julianToUTC $ momentOfTransit)) allTransits

-- TODO: actually filter    
--isActiveTransit :: Applicative f => p -> Transit a -> f Bool
isActiveTransit :: UTCTime -> Transit a -> Bool
isActiveTransit moment Transit {..} = 
  (maybe True (<= moment) transitStarts) &&
  (maybe True (>= moment) transitEnds)

transit :: Connection -> JulianTime -> PlanetaryAspect -> IO PlanetaryTransit 
transit conn momentOfTransit a@(HoroscopeAspect _aspect' (transiting', transited') _angle' _orb') = 
  do
    let transitAspectLongitude = exactAspectAngle a
        transitingPlanet = transiting' & planetName
    (activityStarts, activityEnds) <- activityPeriodQuery conn transitingPlanet transitAspectLongitude momentOfTransit
    crossingCandidates <- crossingCandidatesQuery conn transitingPlanet transitAspectLongitude momentOfTransit

    -- only consider crossing candidates that are at most day before or after the reference date.
    let immediateCrossings = filter ((<= 1) . abs . (subtract momentOfTransit)) crossingCandidates
    -- this is the only moment where we actually touch swiss ephemeris:
        exactImmediateCrossings =  [x | ExactAt x <- map (findExactTransitAround transitingPlanet transitAspectLongitude) immediateCrossings]
    pure $
      Transit {
        transiting = transiting'
      , transited  = transited'
      , transitStarts = julianToUTC <$> activityStarts 
      , transitEnds   = julianToUTC <$> activityEnds
      -- triggers within a day of the reference moment, sorted from latest to earliest.
      , immediateTriggers = map julianToUTC exactImmediateCrossings & (sortBy (comparing Down))
      }

data ExactTransit a
  = OutsideBounds
  | NoCrossing
  | ExactAt a
  deriving (Eq, Show)

instance Monad ExactTransit where
  OutsideBounds >>= _ = OutsideBounds
  NoCrossing >>= _ = NoCrossing
  ExactAt a >>= f = f a
  return = ExactAt

instance Functor ExactTransit where
  fmap _ OutsideBounds = OutsideBounds
  fmap _ NoCrossing = NoCrossing
  fmap f (ExactAt a) = ExactAt (f a)

instance Applicative ExactTransit where
  pure = return
  (<*>) = ap

instance Alternative ExactTransit where
  empty = OutsideBounds
  r@ExactAt {} <|> _ = r
  _ <|> r@ExactAt {} = r
  OutsideBounds <|> r = r
  r <|> OutsideBounds = r
  _ <|> r = r

-- | Merely for numerical convenience. This is an unsafe, partial function!!
unsafeCalculateEclipticLongitude :: Planet -> Double -> Double
unsafeCalculateEclipticLongitude
  planet
  time =
    case pos of
      --Right ep -> trace ("Found position: " <> show ep <> " for time: " <> show time) $ lng ep
      Right ep -> lng ep
      Left e -> error e
    where
      pos = unsafePerformIO $ calculateEclipticPosition (JulianTime time) planet

-- | Given a transiting `Planet`, a @Longitude@ (`Double`) we're interested in
-- and a @JulianTime@, return 0 if the given planet is found to cross
-- the given longitude at the given time. Note that, due to detection of
-- 0/360 jumps, this function only works for positions at most 10 days
-- apart (which means interpolation can't be done at a coarser level.)
-- The heuristic is: if the difference is
-- much greater than the max known speed of the planet,
-- it clearly means we're dealing with a jump over 0/360,
-- so we flip the operands to reflect that the position is actually
-- "on the other side" of the sought longitude.
longitudeIntersects :: Planet -> Double -> Double -> Double
longitudeIntersects p soughtLongitude t = 
  if ((abs difference) >= maxDayStep * (maxSpeed p)) then
    position - soughtLongitude
  else
    difference
  where
    difference = soughtLongitude - position
    position = unsafeCalculateEclipticLongitude p t
    maxDayStep = 10

-- | find a root, using the Ridder's method.
-- we're favoring the Ridder's method since it's built into the math-functions package.
-- But it isn't perfect: it'll only find a root if we can guarantee that `start` and `end`,
-- when passed to the function, will be on either side of the root. That is, we have to ensure
-- that `start` yields a longitude that's before the root, and that `end` is past.
-- I reckon this is why the swiss ephemeris people "normalize" all degrees to not be affected
-- by planets that make many revolutions in a given year.
-- for alternatives, especially non-bracketed functions like Tiruneh's, see:
-- https://math.stackexchange.com/questions/1596654/why-does-ridders-method-work-as-well-as-it-does/3296180
-- https://hackage.haskell.org/package/roots-0.1.1.2
-- The challenge likes in choosing a good pair of start/end to ensure bracketing
-- and fast convergence. Maybe initially it's sufficient to just look at minima and maxima,
-- but I get the sense that the Swiss Ephemeris fellas apply root finding to their precalculated
-- yearly ephemeris to find the day(s) when a transit may be exact, and _then_ do another
-- parabola fit for days?
root :: (Double -> Double) -> Double -> Double -> Root Double
root f start end =
  ridders RiddersParam {riddersMaxIter = 50, riddersTol = RelTol (4 * m_epsilon)} (start, end) f

findExactTransit :: Planet -> Longitude -> JulianTime -> JulianTime -> ExactTransit JulianTime
findExactTransit p (Longitude pos) (JulianTime start) (JulianTime end) =
  case root' of
    Root t -> ExactAt . JulianTime $ t
    NotBracketed -> OutsideBounds -- the given start/end won't converge
    SearchFailed -> NoCrossing -- we looked, but didn't find
  where
    root' = root (longitudeIntersects p pos) start end

findExactTransitAround :: Planet -> Longitude -> JulianTime -> ExactTransit JulianTime
findExactTransitAround p pos (JulianTime start) =
  transitBefore <|> transitAfter
  where
    transitBefore = findExactTransit p pos (JulianTime start) (JulianTime $ start + 1)
    transitAfter = findExactTransit p pos (JulianTime $ start -1) (JulianTime start)
