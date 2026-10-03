# Observed Stop Events from GTFS-Realtime Trip Updates

Reduces an archive of Trip Updates (many successive predictions per trip
and stop across polls) to observed stop events (see
[observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)):
per (trip, service date, stop visit), the latest report wins - labeled
`"observed"` when it was issued at or after the vehicle's departure from
the stop, `"predicted-last"` otherwise. `SKIPPED` stops and
`CANCELED`/`DELETED` trips become explicit negative-information rows;
`NO_DATA` rows are dropped with a message.

## Usage

``` r
rt2s_events_from_trip_updates(updates, baseline = NULL, tz = "UTC")
```

## Arguments

- updates:

  A data.frame as returned by
  [`gtfsrealtime::read_gtfsrt_trip_updates()`](https://projects.indicatrix.org/gtfsrealtime-r/reference/read_gtfsrt_trip_updates.html):
  one row per stop-time update with `trip_id`, `stop_id` and/or
  `stop_sequence`, `arrival_time`/`arrival_delay`,
  `departure_time`/`departure_delay`, schedule relationships,
  `start_date`, `vehicle_id`, `file_timestamp`. When `trip_id` is absent
  (some producers, e.g. HSL, identify trips only by the GTFS-RT
  TripDescriptor), a stable identity is synthesized from `route_id`,
  `direction_id`, `start_date`, and `start_time` so predictions still
  group per operated trip.

- baseline:

  Optional baseline static GTFS feed (object or zip path). Required to
  resolve delay-only updates (a delay without an absolute time can only
  be interpreted against the scheduled time).

- tz:

  Timezone of the feed's service days (used to resolve delay-only
  updates against baseline scheduled times). Default "UTC".

## Value

A validated observed stop events data.table.

## Details

A trip that serves a stop more than once (a loop) keeps one event per
visit, told apart by `stop_sequence` as GTFS-Realtime requires for such
stops. A stop counts as looped when one poll lists it under two
`stop_sequence` values; elsewhere all reports of a stop form one visit
even if a producer renumbers stops between polls. At a looped stop a
report without `stop_sequence` is ambiguous and dropped with a warning.
Delay-only updates are resolved against the baseline row with the same
`stop_sequence` when that row is the same stop (a warning names
sequences that point at another stop), else against a stop the scheduled
trip serves once. A trip without `start_date` whose report falls in the
first hour of a daylight-saving fall-back day belongs to the previous
service day, as GTFS counts that hour past 24:00 of the day before.
