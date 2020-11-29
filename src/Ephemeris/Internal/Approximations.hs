{-# LANGUAGE NoImplicitPrelude #-}

module Ephemeris.Internal.Approximations where

import Import
import Ephemeris.Types

-- | In the precalculated ephemeris, what's the fastest
-- a given planet could be moving.
-- "memoized" version of running this query:
{-
planet	max(abs(speed))	min(abs(speed))
Chiron	0.06636793960389	0.00001234782277
Jupiter	0.24199499337397	0.00009114876237
Mars	0.79134232691456	0.00020755244253
MeanApog	0.11210634588862	0.11069957119221
MeanNode	0.05299705549493	0.05288724518955
Mercury	2.20233222428368	0.00172669123388
Moon	15.38267753571409	11.76634212613541
Neptune	0.03803362290451	0.00000617349252
Pluto	0.03490982565157	0.00001031003089
Saturn	0.13001194270687	0.00003681177736
Sun	1.01971714917181	0.95301396824946
Uranus	0.06043853015857	0.00000080886735
Venus	1.25886887876236	0.0003546286036
-}
maxSpeed :: Planet -> Double
maxSpeed Chiron = 0.06636793960389
maxSpeed Jupiter = 0.24199499337397
maxSpeed Mars = 0.79134232691456
maxSpeed MeanApog = 0.11210634588862
maxSpeed MeanNode = 0.05299705549493
maxSpeed Mercury = 2.20233222428368
maxSpeed Moon = 15.38267753571409
maxSpeed Neptune = 0.03803362290451
maxSpeed Pluto = 0.03490982565157
maxSpeed Saturn = 0.13001194270687
maxSpeed Sun = 1.01971714917181
maxSpeed Uranus = 0.06043853015857
maxSpeed Venus = 1.25886887876236
-- select avg(abs(speed)) from ecliptic_longitude_ephemeris;
-- earth, other apogees/nodes, planets not in the ephemeris.
maxSpeed _ = 1.34207046965558

-- | Given our known max speeds, the maximum number of days
-- it's "safe" to look back/ahead from a date without accidentally
-- crossing over a given longitude again; or put another way,
-- looking back this number of days is roughly equivalent
-- to looking back 180 degrees, plenty of time for the planet
-- to approach/leave a given longitude.
maxDayDelta :: Planet -> Double
maxDayDelta p = 360.0 / (maxSpeed p) / 2
