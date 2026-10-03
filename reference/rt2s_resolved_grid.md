# Resolved Headway-Group Grid Behind a Frequency Feed Set

Returns the resolved `(route, direction, window, scenario)` grid that
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)
built from: what was emitted, what was applied to it, and what was
dropped before it reached the feed. This is the programmatic counterpart
to the assembly warnings - a pipeline that reconciles its own
headway-group accounting against the feed can gate on it instead of
re-deriving the outcome from the written files, which is not always
possible.

## Usage

``` r
rt2s_resolved_grid(feeds)
```

## Arguments

- feeds:

  The list returned by
  [`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md).

## Value

A data.table with one row per candidate headway group and scenario:

- `route_ref`, `direction_id`, `window`, `scenario`:

  The headway group, keyed exactly as `scaling` and `headways` key
  theirs.

- `trip_id`:

  The generated representative trip id, matching `trips.txt` when the
  group was emitted.

- `ratio`:

  The running-time ratio applied, or `NA` under
  `pattern_source = "observed"`, where no ratio exists.

- `headway_secs`:

  The headway actually written to `frequencies.txt`, or `NA` when the
  group was dropped.

- `headway_source`:

  `"observed"` for a quantile-derived headway, `"override"` when
  `headways` supplied it, `NA` when the group was dropped.

- `emitted`:

  Whether the trip reached this scenario's `trips.txt`.

- `drop_reason`:

  `NA` when emitted, else, in pipeline order, `"no_stop_pattern"` (no
  served/baseline stop pattern), `"no_ratio"` (removed from every
  scenario under `scaling_missing = "drop"`), `"no_headway"` (no
  observed quantile and no `headways` override), or
  `"no_within_window_headway"` (strict observed mode found fewer than
  two starts in the configured window).

`"no_headway"` is a drop rather than a trip emitted without a
`frequencies.txt` row. Per GTFS a trip absent from `frequencies.txt` is
read as exact-time, so emitting one whose `stop_times` are offsets from
`00:00:00` would advertise a departure at midnight that never runs.
Service that cannot be written as a repeating headway belongs in
`rt2s_frequencies(extra_trips=)`. An emitted row therefore always
carries a positive `headway_secs`.

## Examples

``` r
if (FALSE) { # \dontrun{
feeds <- rt2s_frequencies(events, windows = win)
grid <- rt2s_resolved_grid(feeds)
# every candidate headway group is accounted for, in every scenario
table(grid$scenario, grid$drop_reason, useNA = "ifany")
} # }
```
