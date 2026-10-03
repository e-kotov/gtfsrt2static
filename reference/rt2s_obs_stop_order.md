# Cross-Trip Canonical Stop Order for Each Route-Direction

Collapses many observed passages of a route-direction into one stop
order. Each stop's canonical position is its **median offset from trip
start** across all passages, tie-broken by `stop_ref`. This is the
cross-trip complement to the per-trip chronological `stop_sequence` the
events converters already produce: when passages disagree on order (e.g.
several stops share an arrival second), per-trip chronology is ambiguous
but the median offset is not.

## Usage

``` r
rt2s_obs_stop_order(events)
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).

## Value

A data.table with one row per served
`(route_ref, direction_id, stop_ref)`: `canonical_offset` (median
seconds from trip start) and `stop_sequence` (1-based order within the
route-direction, by `canonical_offset` then `stop_ref`). Only served
passages count: `skipped`/`canceled` rows and rows without a timed
arrival are excluded, so a stop that was never actually served (e.g.
skipped on every run) produces no row rather than a placeholder
position. A stop visited more than once within a single trip
(loop/branch service) triggers a warning, since the median collapses the
repeated visits into one position.

## Reconstructed versus anchored stop patterns

This function **reconstructs** a stop order from the observations, which
is what you want when no usable published pattern exists. It is the
wrong tool when one does exist and an analysis depends on the network
being identical across the scenarios being compared: the order here is
derived from what was observed, so different observations can yield
different stop sets, and a scheduled-versus-observed contrast then
confounds "service changed" with "the network changed". For that design,
**anchor** on the published pattern with
[`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md)
and vary only service levels.

Worth knowing how easily the two are confused: the median-offset rule
below is the same rule a GPS-only pattern builder would use, so a
pipeline can slide from anchored to reconstructed without any visible
signal.
