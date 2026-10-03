# Planned Headways per Route-Direction and Time Window

Reduces a planned static feed's own trip start times to one headway per
`(route, direction, window)`: the gaps between consecutive scheduled
departures, summarised. This is the planned counterpart to
[`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md),
and it is what makes a "scheduled" scenario expressible - a feed that
keeps the published running times *and* the published frequency, to
contrast against observed ones.

## Usage

``` r
rt2s_baseline_headways(
  baseline,
  windows,
  route_key = c("route_id", "route_short_name"),
  statistic = c("median", "mean"),
  max_headway_secs = 3L * 3600L,
  closed_last = FALSE
)
```

## Arguments

- baseline:

  A planned static GTFS feed: a gtfsio/gtfstools-style object or a path
  to a GTFS zip. Requires `trips` and `stop_times`.

- windows:

  Named list of `c(start, end)` time strings, as in
  [`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md);
  passed to
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md),
  so overnight windows such as `c("22:00", "26:00")` work. The name
  `"other"` is reserved for unassigned times and is rejected. Trips
  whose first departure falls in no window are excluded.

- route_key:

  Which baseline column becomes `route_ref`; see
  [`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md).

- statistic:

  How to summarise the gaps: `"median"` (default) or `"mean"`. Both are
  rounded to whole seconds.

- max_headway_secs:

  Gaps above this are treated as between-service breaks and excluded.
  Default 10800 (3 h).

- closed_last:

  Logical; when `TRUE` the last window in list order is closed on its
  end, so a trip departing exactly on it is inside that window rather
  than excluded. See
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md).

## Value

A data.table with columns `route_ref`, `direction_id`, `window`,
`headway_secs` (integer) and `n_sched_trips` (the gaps summarised).
Groups with fewer than two departures in a window yield no row, since a
single departure defines no headway.

## Details

Feed the result to `rt2s_frequencies(headways=)` after tagging it with
the scenario it describes:

    sh <- rt2s_baseline_headways(static, windows)
    sh$scenario <- "scheduled"

## Difference from [`rt2s_obs_headways()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)

A static feed has no service dates, only a calendar, so gaps are *not*
grouped within a day the way observed trip starts are. Every trip the
feed defines for a route-direction contributes to one pool per window.
Restrict the `baseline` to the service pattern you mean (e.g. weekday
trips) before calling if that distinction matters.

## See also

[`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md),
[`rt2s_baseline_service_dates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_service_dates.md),
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)

## Examples

``` r
baseline <- list(
  trips = data.frame(
    trip_id = c("t1", "t2", "t3"),
    route_id = "B62",
    direction_id = 0L
  ),
  stop_times = data.frame(
    trip_id = rep(c("t1", "t2", "t3"), each = 2),
    stop_id = rep(c("S1", "S2"), 3),
    stop_sequence = rep(1:2, 3),
    arrival_time = c(
      "06:00:00", "06:05:00",
      "06:10:00", "06:15:00",
      "06:25:00", "06:30:00"
    ),
    departure_time = c(
      "06:00:00", "06:05:00",
      "06:10:00", "06:15:00",
      "06:25:00", "06:30:00"
    )
  )
)
# departures 06:00 / 06:10 / 06:25 -> gaps 600, 900 -> median 750
rt2s_baseline_headways(baseline, windows = list(am = c("06:00", "09:00")))
#>    route_ref direction_id window headway_secs n_sched_trips
#>       <char>        <int> <char>        <int>         <int>
#> 1:       B62            0     am          750             2
```
