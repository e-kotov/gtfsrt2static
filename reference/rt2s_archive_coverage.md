# Archive Coverage and Gap Report

Summarizes each feed's manifest: polls per day, how many were stored vs
skipped as unchanged, error counts, and the longest gap between
consecutive polls. Long `skipped_unchanged` streaks with an advancing
poll clock usually mean a frozen upstream feed.

## Usage

``` r
rt2s_archive_coverage(dir)
```

## Arguments

- dir:

  Archive root directory (as used by
  [`rt2s_collect`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_collect.md)).

## Value

A data.table with one row per feed and day.
