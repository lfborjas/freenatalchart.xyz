{-# OPTIONS -fno-warn-orphans #-}

module Arbitrary where

import Import ( liftM, liftM2 )
import Test.QuickCheck ( Arbitrary(..), choose, oneof )
import RIO.Time (DiffTime, timeToTimeOfDay, TimeOfDay(..), fromGregorian, toGregorian, Day(..), LocalTime(..))
import Data.Fixed (mod', Pico)
import Data.Ratio ((%))

-- from:
-- https://github.com/haskell/time/blob/866ed2501fe1acf31abeaaddefcdc730436d87fa/test/main/Test/Arbitrary.hs

-- notice that we override this instance to only encompass Julian
-- dates in the range fully supported by our current ephemeris
-- data, as per:
-- https://github.com/lfborjas/swiss-ephemeris/blob/06287a87d92d72bf3861802a29035f756e36cbf7/test/SwissEphemerisSpec.hs#L306-L307
instance Arbitrary Day where
    arbitrary = liftM ModifiedJulianDay $ choose (2378496, 2597641) -- 1800-Jan-01 AD to 2399-Dec-31
    shrink day = let
        (y, m, d) = toGregorian day
        dayShrink =
            if d > 1
                then [fromGregorian y m (d - 1)]
                else []
        monthShrink =
            if m > 1
                then [fromGregorian y (m - 1) d]
                else []
        yearShrink =
            if y > 2000
                then [fromGregorian (y - 1) m d]
                else if y < 2000
                         then [fromGregorian (y + 1) m d]
                         else []
        in dayShrink ++ monthShrink ++ yearShrink

instance Arbitrary TimeOfDay where
    arbitrary = liftM timeToTimeOfDay arbitrary
    shrink (TimeOfDay h m s) = let
        shrinkInt 0 = []
        shrinkInt 1 = [0]
        shrinkInt _ = [0, 1]
        shrinkPico 0 = []
        shrinkPico 1 = [0]
        shrinkPico p =
            case reduceDigits 12 p of
                Just p' -> [0, 1, p']
                Nothing -> [0, 1]
        in [TimeOfDay h' m s | h' <- shrinkInt h] ++
           [TimeOfDay h m' s | m' <- shrinkInt m] ++ [TimeOfDay h m s' | s' <- shrinkPico s]

reduceDigits :: Int -> Pico -> Maybe Pico
reduceDigits (-1) _ = Nothing
reduceDigits n x = let
    d :: Pico
    d = 10 ^^ (negate n)
    r = mod' x d
    in case r of
           0 -> reduceDigits (n - 1) x
           _ -> Just $ x - r        

instance Arbitrary DiffTime where
    arbitrary = oneof [intSecs, fracSecs] -- up to 1 leap second
      where
        intSecs = liftM secondsToDiffTime' $ choose (0, 86400)
        fracSecs = liftM picosecondsToDiffTime' $ choose (0, 86400 * 10 ^ (12 :: Int))
        secondsToDiffTime' :: Integer -> DiffTime
        secondsToDiffTime' = fromInteger
        picosecondsToDiffTime' :: Integer -> DiffTime
        picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance Arbitrary LocalTime where
    arbitrary = liftM2 LocalTime arbitrary arbitrary
    shrink (LocalTime d tod) = [LocalTime d' tod | d' <- shrink d] ++ [LocalTime d tod' | tod' <- shrink tod]  
