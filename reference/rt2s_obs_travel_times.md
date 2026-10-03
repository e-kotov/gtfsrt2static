# Observed Travel-Time and Dwell Quantiles per Stop

Reduces many observed passages to one representative stop pattern: per
`(route_ref, direction_id, stop_ref)`, quantiles of travel time from
trip start and the median dwell. The stops are ordered by the cross-trip
canonical order
([`rt2s_obs_stop_order`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_stop_order.md)),
giving a monotone representative sequence suitable for a frequency
trip's `stop_times`.

## Usage

``` r
rt2s_obs_travel_times(events, quantiles = c(p05 = 0.05, p50 = 0.5, p95 = 0.95))
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).

- quantiles:

  Named numeric vector of probabilities in `[0, 1]`; each becomes an
  integer-seconds column `travel_<name>`. Default
  `c(p05 = 0.05, p50 = 0.5, p95 = 0.95)` - the free-flow / typical /
  reliable triple.

## Value

A data.table ordered by `stop_sequence`, with columns `route_ref`,
`direction_id`, `stop_ref`, `stop_sequence`, one `travel_<name>` column
per quantile (integer seconds from trip start), `dwell_median` (integer
seconds; `0` when no dwell was observed), and `n_obs` (served passages
at the stop). Only served passages are summarised - `skipped`/`canceled`
rows and rows without a timed arrival are excluded, so a stop served on
no run produces no row (rather than an all-NA one). Travel time is
**not** forced monotone here; the assembler applies the `cummax` guard
when it renders clock times.
