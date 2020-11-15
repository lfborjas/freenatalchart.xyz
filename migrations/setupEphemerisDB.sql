drop table if exists ecliptic_longitude_ephemeris;

create table ecliptic_longitude_ephemeris (
  planet TEXT,
  julian_time REAL,
  longitude REAL,
  lng_speed_signum TINYINT
);

CREATE INDEX if not exists planet_idx ON ecliptic_longitude_ephemeris (planet);
CREATE INDEX if not exists time_idx ON ecliptic_longitude_ephemeris (julian_time);
CREATE INDEX if not exists planet_lng_idx ON ecliptic_longitude_ephemeris (planet, longitude);
CREATE INDEX if not exists planet_lng_at_idx ON ecliptic_longitude_ephemeris (planet, julian_time, longitude);
