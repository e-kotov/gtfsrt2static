# Observed Headways per Route-Direction and Time Window

Reduces observed service to headway quantiles per
`(route_ref, direction_id, window)`. Two kinds of evidence can carry a
headway, and `method` picks between them; both drop non-positive gaps
and gaps longer than `max_headway_secs` as spurious (vehicles resuming
after a layover, data gaps), and neither applies weekday/weekend
filtering - restrict the `events` to the service dates you want
summarised before calling.

## Usage

``` r
rt2s_obs_headways(
  events,
  windows = NULL,
  quantiles = c(median = 0.5, p95 = 0.95),
  max_headway_secs = 3L * 3600L,
  method = c("trip_start", "passage"),
  reference_stops = NULL,
  min_revisit_gap_s = 600L,
  strict_within_window = FALSE,
  closed_last = FALSE
)
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).
  For `method = "passage"`, GPS observations should first be converted
  with
  [`rt2s_events_from_stop_times`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_stop_times.md),
  so they use the same provenance and source conventions as the rest of
  the package.
  [`rt2s_events_from_trip_updates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_trip_updates.md)
  keeps one row per trip, service date, and stop, so it exposes at most
  one passage per stop per trip and cannot recover repeated visits that
  were already reduced.

- windows:

  Time-window definition passed to
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md);
  `NULL` (default) treats each day as one `"all"` window. The window
  name `"other"` is reserved for unassigned service times. When
  `strict_within_window = TRUE`, configured windows must not overlap.

- quantiles:

  Named numeric vector of probabilities in `[0, 1]`; each becomes an
  integer-seconds column `headway_<name>`. Default
  `c(median = 0.5, p95 = 0.95)`.

- max_headway_secs:

  Upper cutoff; gaps above it are treated as between-service breaks and
  excluded. Default 10800 (3 h).

- method:

  Which evidence carries the headway: `"trip_start"` (default) or
  `"passage"`, as described above.

- reference_stops:

  Passage method only. Optional character vector of stop ids to consider
  as reference stops. If `NULL`, the best-observed direction-unique stop
  is selected for each route-direction. If multiple supplied stops match
  a route-direction, the best-observed one is used. Supplying
  `reference_stops` restricts output to route-directions that serve one
  of those stops. Explicit values are ignored with a warning under
  `method = "trip_start"`.

- min_revisit_gap_s:

  Passage method only. Minimum seconds between two passages of the same
  vehicle at the same reference stop. Detections closer together are
  treated as one passage. If `vehicle_ref` is missing, dwell revisits
  cannot be distinguished from following vehicles, so each
  unknown-vehicle detection is treated as a separate passage and this
  gap does not apply to it. Default 600 (10 min). Explicit values are
  ignored with a warning under `method = "trip_start"`.

- strict_within_window:

  Logical. When `FALSE` (default), intervals are calculated
  consecutively within each service date and attributed to the window of
  the later event, allowing intervals that cross window boundaries. When
  `TRUE`, events are assigned to windows before calculating intervals,
  intervals crossing a window boundary are discarded, and events in
  unassigned times are ignored. Configured windows must be pairwise
  non-overlapping under `strict_within_window = TRUE`.

- closed_last:

  Logical; when `TRUE` the last window in list order is closed on its
  end, so an event exactly on it is inside that window rather than
  unassigned. See
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md).

## Value

A data.table with columns `route_ref`, `direction_id`, `window`, one
`headway_<name>` column per quantile (integer seconds), and `n_headways`
(count of headways summarised). Groups with no usable headway (e.g. a
single run) produce no row.

`method = "passage"` additionally carries `reference_stop_ref`, the stop
the passages were measured at. The headway columns and `n_headways` are
identical across the two methods, so either result can feed the
frequency assembly path. If served events exist but no direction-unique
reference stop can be resolved, the passage method errors instead of
returning an empty table.

## Details

`method = "trip_start"` (default) measures the gaps between consecutive
**trip starts** *within one service date*. A trip's start is its first
**served** arrival: `skipped`/`canceled` rows are ignored, so a canceled
trip (which never operated) contributes no start and cannot shrink the
observed headway, even if its rows carry stale timestamps.

## Passage headways

`method = "passage"` instead measures the intervals between successive
vehicle passages at one reference stop per route-direction. This is what
to use when `trip_ref` cannot identify individual trips (for example, an
all-day block identifier): `"trip_start"` would see one trip start, but
a reference-stop passage sequence can still reveal the service headway.

The reference stop must identify a single direction within its route. A
stop observed in multiple known directions is rejected when supplied
explicitly, and is excluded from automatic selection. Rows with unknown
`direction_id` do not make a stop shared, but they are warned and
excluded from passage-headway output because they cannot be assigned to
a route-direction group. If every candidate reference-stop row lacks
`direction_id`, the function errors after this exclusion; a caller who
knows a stop is one-directional should stamp `direction_id` on their
events before calling. Automatic selection chooses the best-observed
direction-unique stop for each route-direction.

Consecutive detections of the same vehicle at the same reference stop
are collapsed into one passage until the vehicle has been absent for at
least `min_revisit_gap_s`. A chain of detections each spaced below
`min_revisit_gap_s` remains one passage even if the chain's total span
is longer. This keeps a dwell or repeated GPS fix from becoming many
artificial headways.
