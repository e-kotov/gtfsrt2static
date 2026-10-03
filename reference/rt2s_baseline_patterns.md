# Canonical Published Stop Pattern per Route-Direction

Reduces a planned static GTFS feed to **one representative stop pattern
per `(route, direction)`**: the pattern that the most trips actually
operate, with its stop-to-stop offsets rebased to a trip start. This is
the *anchoring* counterpart to
[`rt2s_obs_travel_times`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_travel_times.md),
which reconstructs a pattern from observations instead.

## Usage

``` r
rt2s_baseline_patterns(
  baseline,
  route_key = c("route_id", "route_short_name"),
  min_stops = 2L
)
```

## Arguments

- baseline:

  A planned static GTFS feed: a gtfsio/gtfstools-style object (named
  list of data.frames) or a path to a GTFS zip. Only `trips` and
  `stop_times` are required, plus `routes` when
  `route_key = "route_short_name"`.

- route_key:

  Which baseline column becomes `route_ref`, i.e. the identity that must
  match `events$route_ref` downstream. `"route_id"` (default) is the
  spec-canonical identity and what
  [`rt2s_events_from_trip_updates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_trip_updates.md)
  writes. `"route_short_name"` suits observations keyed on the public
  line number; when several `route_id`s share a short name they are
  collapsed with a warning.

- min_stops:

  Minimum stops for a trip to be a pattern candidate. Default 2 - a
  one-stop "pattern" cannot describe movement.

## Value

A data.table ordered by `route_ref`, `direction_id`, `stop_sequence`,
with columns `route_ref`, `direction_id`, `stop_ref`, `stop_sequence`
(dense, 1-based), `travel_base` (integer seconds from the template
trip's first departure; negative at the origin), `dwell_base` (integer
seconds), `template_trip_id`, `n_pattern_trips` (baseline trips carrying
the winning signature) and `n_stops`.

Trips are excluded from candidacy - with a warning giving the count -
when `direction_id` is `NA`, or when any `arrival_time` /
`departure_time` is missing (real feeds leave non-timepoint stops
blank). Excluding before counting means the modal rule simply picks
among fully timed trips; it is an error only if that empties a
route-direction.

## Details

Use it when an analysis compares scenarios that must share an identical
network. Because the stops, their order and their relative offsets all
come from the published feed rather than from observations, feeds built
for different scenarios differ only in the service levels applied to
them - so a scheduled-versus-observed contrast cannot be an artifact of
the scenarios having different stop sets. Feed it to
`rt2s_frequencies(pattern_source = "baseline")`.

## Pattern selection

Each trip is reduced to a *signature*: its `stop_id`s in `stop_sequence`
order. Per `(route, direction)` the winning signature is the one carried
by the most trips, tie-broken by more stops and then lexicographically,
so short turns and via-variants collapse to the one pattern that best
represents the route and the choice never depends on row order. The
*template trip* is the lowest `trip_id` carrying the winning signature;
it supplies the offsets and is returned for traceability.

## Offsets

Offsets are rebased on the template trip's **first departure**: the
layover before the vehicle starts moving is not travel time.
`travel_base` is therefore negative at the origin stop (its arrival
precedes that departure), which is intentional - the frequency assembler
clamps it through
[`rt2s_monotone_offsets`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_monotone_offsets.md).
`stop_sequence` is renumbered densely from 1, since the baseline's own
numbering may start anywhere and contain gaps (GTFS requires only that
it increase).

## See also

[`rt2s_baseline_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_headways.md)
for the planned feed's own headways,
[`rt2s_baseline_service_dates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_service_dates.md)
for its service days,
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)
to assemble feeds from the result.

## Examples

``` r
baseline <- list(
  trips = data.frame(
    trip_id = c("t1", "t2", "t3"),
    route_id = "B62",
    direction_id = 0L
  ),
  stop_times = data.frame(
    trip_id = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3"),
    stop_id = c("S1", "S2", "S3", "S1", "S2", "S3", "S1", "S2"),
    stop_sequence = c(1:3, 1:3, 1:2),
    arrival_time = c(
      "05:59:30", "06:02:00", "06:04:30",
      "06:09:30", "06:12:00", "06:14:30",
      "06:19:30", "06:22:00"
    ),
    departure_time = c(
      "06:00:00", "06:02:30", "06:05:00",
      "06:10:00", "06:12:30", "06:15:00",
      "06:20:00", "06:22:30"
    )
  )
)
# The 3-stop signature wins (2 trips vs 1); offsets rebase on 06:00:00.
rt2s_baseline_patterns(baseline)
#>    route_ref direction_id stop_ref stop_sequence travel_base dwell_base
#>       <char>        <int>   <char>         <int>       <int>      <int>
#> 1:       B62            0       S1             1         -30         30
#> 2:       B62            0       S2             2         120         30
#> 3:       B62            0       S3             3         270         30
#>    template_trip_id n_pattern_trips n_stops
#>              <char>           <int>   <int>
#> 1:               t1               2       3
#> 2:               t1               2       3
#> 3:               t1               2       3
```
