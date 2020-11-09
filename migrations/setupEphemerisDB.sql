drop table if exists ecliptic_ephemeris;

create table ecliptic_ephemeris (
  planet TEXT,
  julian_time REAL,
  longitude REAL,
  latitude REAL,
  distance REAL,
  lng_speed REAL,
  lat_speed REAL,
  dist_speed REAL
);

CREATE INDEX if not exists planet_idx ON ecliptic_ephemeris (planet);
CREATE INDEX if not exists time_idx ON ecliptic_ephemeris (julian_time);
CREATE INDEX if not exists planet_lng_idx ON ecliptic_ephemeris (planet, longitude);
CREATE INDEX if not exists planet_lng_at_idx ON ecliptic_ephemeris (planet, julian_time, longitude);
