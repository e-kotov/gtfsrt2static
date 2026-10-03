# Roll Finished Archive Days into Daily ZIPs

Zips every closed day directory (any date before today, UTC) of every
feed under the archive root into `<feed_id>/<YYYYMMDD>.zip` - exactly
the multi-file input
[`gtfsrealtime::read_gtfsrt_positions()`](https://projects.indicatrix.org/gtfsrealtime-r/reference/read_gtfsrt_positions.html)
and `read_gtfsrt_trip_updates()` ingest - and removes the day directory.

## Usage

``` r
rt2s_archive_rotate(dir)
```

## Arguments

- dir:

  Archive root directory (as used by
  [`rt2s_collect`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_collect.md)).

## Value

Invisibly, a character vector of the ZIP files written.
