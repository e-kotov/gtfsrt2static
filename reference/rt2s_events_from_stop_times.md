# Observed Stop Events from gps2gtfs Stop Times

Converts the `$stop_times` table produced by
[`gps2gtfs::g2g_extract_trips_and_stop_times()`](https://rdrr.io/pkg/gps2gtfs/man/g2g_extract_trips_and_stop_times.html)
into observed stop events (see
[observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).

## Usage

``` r
rt2s_events_from_stop_times(
  stop_times,
  tz = "UTC",
  route_ref = NA_character_,
  source = c("positions", "gps"),
  trip_ref_prefix = "g2g_",
  trip_id_col = NULL,
  shape_ref_prefix = NULL
)
```

## Arguments

- stop_times:

  The gps2gtfs stop times table: columns `trip_id`, `vehicle_id`,
  `direction` (1/2), `stop_id`, and `arrival_time`/`departure_time` as
  absolute `POSIXct` values (gps2gtfs \>= 0.2.0). Legacy "HH:MM:SS"
  strings are also accepted; they additionally require the `date` column
  they are interpreted within (and cannot represent a trip whose stop
  visits fall on the wrong side of midnight unambiguously - prefer
  POSIXct).

- tz:

  Timezone the legacy "HH:MM:SS" strings refer to (the timezone the GPS
  data was cleaned in). Ignored for POSIXct input, which carries its own
  timezone. Default "UTC".

- route_ref:

  Optional route identity to stamp on all events (gps2gtfs models one
  route per run). Default NA.

- source:

  Provenance source label: `"positions"` (GTFS-RT Vehicle Positions,
  default) or `"gps"` (raw AVL/logger data).

- trip_ref_prefix:

  Prefix for synthetic trip identities. When a trip has no official id
  (see `trip_id_col`), its trip ref is `<prefix><yyyymmdd>_<trip_id>` so
  synthetic refs stay unique across service dates. Default `"g2g_"`.

- trip_id_col:

  Optional name of a column carrying the official trip identity (e.g.
  `"provided_trip_id"`, as emitted by gps2gtfs's fast path from a
  GTFS-Realtime `trip_id`). Where present and non-missing, its value
  becomes the `trip_ref` verbatim, so baseline-mode assembly can match
  observed trips to the baseline `trips.txt`; trips lacking it fall back
  to a synthetic ref. `NULL` (default) auto-detects a `provided_trip_id`
  column and uses it when present.

- shape_ref_prefix:

  Optional character prefix for populating `shape_ref` from the internal
  `trip_id`, as `<prefix><trip_id>`. Set it to the `shape_id_prefix`
  used with
  [`gps2gtfs::g2g_shapes_from_trips()`](https://rdrr.io/pkg/gps2gtfs/man/g2g_shapes_from_trips.html)
  (default there `"SHP_"`) so the assembler can link `trips.shape_id` to
  those shapes. `NULL` (default) leaves `shape_ref` as NA (no shape
  linkage).

## Value

A validated observed stop events data.table.

## Details

Each trip is attributed to one service day: the day (in the times'
timezone) of its first observed stop. Post-midnight stops keep their
trip's service date, so an overnight trip yields a single `trip_ref` and
the assembler renders its late stops as `>24:00:00` clock times.
