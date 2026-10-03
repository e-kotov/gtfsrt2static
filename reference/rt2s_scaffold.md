# Scaffold a Standard-Compliant GTFS Feed from Observed Stop Events

Baseline-free assembly: synthesizes every spec-required GTFS file from
observed stop events plus user-supplied agency metadata, with
deterministic, properly linked identifiers. Call it directly when there
is no planned feed to inherit from at all;
[`rt2s_assemble`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_assemble.md)
also falls back to it when it is given no `baseline`.

## Usage

``` r
rt2s_scaffold(
  events,
  agency = NULL,
  stops = NULL,
  route_type = 3L,
  shapes = NULL,
  tz = NULL,
  feed_lang = "en",
  feed_contact_email = NULL,
  feed_contact_url = NULL,
  strict = FALSE
)
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).

- agency:

  Named list with `name`, `url`, `timezone`. Placeholders + a warning
  when omitted.

- stops:

  Optional table of stop locations: `stop_id` plus
  `latitude`/`longitude` (or `stop_lat`/`stop_lon`), optionally
  `stop_name`. Stops present in events but absent here get NA
  coordinates and a warning (the result will not validate until they are
  filled).

- route_type:

  GTFS route type for scaffolded routes. Default 3 (bus).

- shapes:

  Optional `shapes.txt`-shaped table (e.g. from
  [`gps2gtfs::g2g_shapes_from_trips()`](https://rdrr.io/pkg/gps2gtfs/man/g2g_shapes_from_trips.html)).
  Linked to trips via `trips.shape_id` when the events carry a matching
  `shape_ref` (see `rt2s_events_from_stop_times(shape_ref_prefix=)`);
  shapes not referenced by any trip are dropped, and references without
  matching geometry warn (or error under `strict`).

- tz:

  Timezone of the service days; used to derive GTFS clock strings (with
  \>24:00:00) from absolute event times. Defaults to `agency$timezone`,
  else "UTC".

- feed_lang:

  Primary language of the feed, written to `feed_info.feed_lang`
  (spec-required in feed_info.txt). Default `"en"`.

- feed_contact_email, feed_contact_url:

  Optional recommended `feed_info.txt` contact fields. Written only when
  supplied.

- strict:

  Logical. When `TRUE`, conditions that would yield a non-publishable
  feed - placeholder agency metadata or stops missing spec-required
  coordinates - raise an error instead of a warning. Use it as a publish
  gate: a scaffold that returns under `strict = TRUE` has no known
  spec-required gaps introduced by scaffolding (it is not a substitute
  for full GTFS validation). Default `FALSE`.

## Value

A gtfsio-convention feed object (class `gtfs`, named list of
data.tables) - write it with
[`gtfsio::export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.html).

## Details

What cannot come from GTFS-RT and must be supplied (or is filled with a
flagged placeholder): agency name/url/timezone (spec-required), stop
coordinates (`stops` argument, e.g. from
[`gps2gtfs::g2g_stops_from_positions()`](https://rdrr.io/pkg/gps2gtfs/man/g2g_stops_from_positions.html)),
and `route_type` (defaults to 3, bus, with a warning).
