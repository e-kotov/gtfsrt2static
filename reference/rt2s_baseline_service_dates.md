# Service Dates of a Named Baseline Service

Expands one `service_id` of a planned static GTFS feed into the `Date`
vector it actually runs on, honouring `calendar.txt`'s weekday flags and
date range *and* `calendar_dates.txt`'s exceptions (type 1 adds a date,
type 2 removes one). Feeds that define a service through exceptions
alone, with no `calendar.txt` row, are supported.

## Usage

``` r
rt2s_baseline_service_dates(baseline, service_id)
```

## Arguments

- baseline:

  A planned static GTFS feed: a gtfsio/gtfstools-style object (named
  list of data.frames) or a path to a GTFS zip. Needs `calendar.txt`,
  `calendar_dates.txt`, or both.

- service_id:

  The single `service_id` to expand. A value the feed does not define is
  an error listing the ones it does.

## Value

A sorted `Date` vector with no duplicates. A service whose exceptions
remove every date it would otherwise run yields a zero-length vector
with a warning.

## Details

This is the opt-in way to give
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)
the baseline's own days through its `service_dates` argument. It is
deliberately a separate call rather than something the assembler does
for you: the assembler never silently inherits `baseline$calendar`,
because a planned calendar describes planned service while the emitted
feed describes the span the caller is asserting. Passing the result
explicitly keeps that choice visible at the call site.

## See also

[`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md),
[`rt2s_baseline_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_headways.md),
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md)

## Examples

``` r
baseline <- list(
  calendar = data.frame(
    service_id = "WD",
    monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L, friday = 1L,
    saturday = 0L, sunday = 0L,
    start_date = 20260302L, end_date = 20260313L
  ),
  calendar_dates = data.frame(
    service_id = "WD",
    date = c(20260307L, 20260311L),
    exception_type = c(1L, 2L)
  )
)
# ten weekdays, plus the added Saturday, minus the removed Wednesday
rt2s_baseline_service_dates(baseline, "WD")
#>  [1] "2026-03-02" "2026-03-03" "2026-03-04" "2026-03-05" "2026-03-06"
#>  [6] "2026-03-07" "2026-03-09" "2026-03-10" "2026-03-12" "2026-03-13"
```
