# Package index

## Collect

Archive ephemeral GTFS-Realtime endpoints as daily ZIPs of raw `.pb`
polls. The collector never parses protobuf.

- [`rt2s_collect()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_collect.md)
  : Continuously Archive GTFS-Realtime Feeds
- [`rt2s_archive_rotate()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_archive_rotate.md)
  : Roll Finished Archive Days into Daily ZIPs
- [`rt2s_archive_coverage()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_archive_coverage.md)
  : Archive Coverage and Gap Report
- [`rt2s_service_template()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_service_template.md)
  : Generate a Supervised-Service Template for the Collector

## Events

Reduce Trip Updates or gps2gtfs stop times to the canonical observed
stop events table: one row per trip, stop and service date, with
provenance.

- [`rt2s_events_from_trip_updates()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_trip_updates.md)
  : Observed Stop Events from GTFS-Realtime Trip Updates
- [`rt2s_events_from_stop_times()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_stop_times.md)
  : Observed Stop Events from gps2gtfs Stop Times
- [`rt2s_events_validate()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_validate.md)
  : Validate an Observed Stop Events Table

## Summarise

Reduce many observed runs to headway, travel-time and dwell quantiles
and to a cross-trip stop order.

- [`rt2s_obs_headways()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
  : Observed Headways per Route-Direction and Time Window
- [`rt2s_obs_travel_times()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_travel_times.md)
  : Observed Travel-Time and Dwell Quantiles per Stop
- [`rt2s_obs_stop_order()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_stop_order.md)
  : Cross-Trip Canonical Stop Order for Each Route-Direction
- [`rt2s_time_window()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md)
  : Classify Times of Day into Named Service Windows

## Baseline

Read a planned static feed: canonical stop patterns, scheduled headways,
and the dates a service runs.

- [`rt2s_baseline_patterns()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md)
  : Canonical Published Stop Pattern per Route-Direction
- [`rt2s_baseline_headways()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_headways.md)
  : Planned Headways per Route-Direction and Time Window
- [`rt2s_baseline_service_dates()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_service_dates.md)
  : Service Dates of a Named Baseline Service

## Assemble

Merge events into a planned feed, scaffold a compliant feed from
scratch, or collapse many runs into frequency-based feeds.

- [`rt2s_assemble()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_assemble.md)
  : Assemble a Realized GTFS Feed from Observed Stop Events
- [`rt2s_scaffold()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
  : Scaffold a Standard-Compliant GTFS Feed from Observed Stop Events
- [`rt2s_frequencies()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)
  : Assemble Frequency-Based Realized GTFS Feeds (One per Reliability
  Quantile)
- [`rt2s_resolved_grid()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_resolved_grid.md)
  : Resolved Headway-Group Grid Behind a Frequency Feed Set

## Concepts

- [`observed-stop-events`](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)
  : The Observed Stop Events Schema
- [`gtfsrt2static`](https://e-kotov.github.io/gtfsrt2static/reference/gtfsrt2static-package.md)
  [`gtfsrt2static-package`](https://e-kotov.github.io/gtfsrt2static/reference/gtfsrt2static-package.md)
  : gtfsrt2static: Realized Static GTFS Snapshots from GTFS-Realtime and
  GPS-Derived Observations

## Helpers

- [`rt2s_publishable()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_publishable.md)
  : Publish-Readiness of an Assembled Feed
- [`rt2s_monotone_offsets()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_monotone_offsets.md)
  : Make Stop-Time Offsets Monotone
