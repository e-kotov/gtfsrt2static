# Get started with gtfsrt2static

`gtfsrt2static` turns observations of transit service (GTFS-Realtime
Trip Updates, or stop times inferred from GPS by
[`gps2gtfs`](https://e-kotov.github.io/gps2gtfs/)) into a static GTFS
feed of the service that actually ran. This article takes one bundled
snapshot of New York City bus Trip Updates through the whole path:

1.  read the protobuf with
    [`gtfsrealtime`](https://cran.r-project.org/package=gtfsrealtime);
2.  reduce it to *observed stop events*, the table every assembler
    consumes;
3.  check the events;
4.  scaffold a GTFS feed from them;
5.  write the feed and read it back with
    [`gtfstools`](https://ipea.github.io/gtfstools/).

``` r

library(gtfsrt2static)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

## Read the Trip Updates

`gtfsrealtime` parses GTFS-Realtime protobuf (single snapshots,
compressed files, or a daily ZIP of archived polls) into one row per
stop-time update. Pass the timezone the agency operates in. The reader
warns about duplicate trip descriptors in this particular snapshot;
those concern the source feed’s identifiers, not the conversion, so we
suppress them.

``` r

tu <- suppressWarnings(gtfsrealtime::read_gtfsrt_trip_updates(
  system.file("nyc-trip-updates.pb.bz2", package = "gtfsrealtime"),
  timezone = "America/New_York"
))
tu <- as.data.table(tu)
dim(tu)
#> [1] 90881    26

tu <- tu[route_id == "M15"] # one Manhattan bus route keeps the example small
tu[1:3, .(trip_id, route_id, stop_sequence, stop_id, arrival_time)]
#>                              trip_id route_id stop_sequence stop_id
#>                               <char>   <char>         <num>  <char>
#> 1: OH_A6-Weekday-SDon-092600_M15_230      M15            59  401802
#> 2: OH_A6-Weekday-SDon-092600_M15_230      M15            60  840005
#> 3: OH_A6-Weekday-SDon-092600_M15_230      M15            61  803002
#>           arrival_time
#>                 <POSc>
#> 1: 2026-01-28 17:13:49
#> 2: 2026-01-28 17:15:19
#> 3: 2026-01-28 17:15:45
```

## Reduce to observed stop events

[`rt2s_events_from_trip_updates()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_from_trip_updates.md)
keeps, for each trip, service date and stop visit, the latest report,
and labels it `observed` when it was issued after the vehicle left the
stop and `predicted-last` otherwise. Skipped stops and canceled trips
become explicit rows. Both input paths of the package, Trip Updates and
gps2gtfs stop times, end in this same table; see
[`?"observed-stop-events"`](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)
for its columns.

``` r

events <- rt2s_events_from_trip_updates(tu, tz = "America/New_York")
events[1:3, .(trip_ref, route_ref, service_date, stop_ref, stop_sequence,
              arrival_time, provenance)]
#> Key: <service_date, trip_ref, stop_sequence>
#>                             trip_ref route_ref service_date stop_ref
#>                               <char>    <char>       <Date>   <char>
#> 1: OH_A6-Weekday-SDon-092600_M15_230       M15   2026-01-28   405645
#> 2: OH_A6-Weekday-SDon-092600_M15_230       M15   2026-01-28   401802
#> 3: OH_A6-Weekday-SDon-092600_M15_230       M15   2026-01-28   840005
#>    stop_sequence        arrival_time     provenance
#>            <int>              <POSc>         <char>
#> 1:            58 2026-01-28 17:14:49 predicted-last
#> 2:            59 2026-01-28 17:15:37 predicted-last
#> 3:            60 2026-01-28 17:17:08 predicted-last
table(events$provenance)
#> 
#> predicted-last 
#>            272
```

A single poll is *prediction-dominated*: most stops have not been passed
yet, so they are labelled `predicted-last` rather than `observed`. An
archive collected over the day with
[`rt2s_collect()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_collect.md)
shifts that balance towards `observed`.

## Check the events

[`rt2s_events_validate()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_events_validate.md)
checks that a table conforms to the observed stop events schema and
returns it, so it fits in a pipe; an error names the first violation.
The converters and assemblers already call it, so run it yourself on
events you build or modify by hand.

``` r

events <- rt2s_events_validate(events)
```

## Scaffold a feed

With no planned feed to inherit from,
[`rt2s_scaffold()`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
synthesizes every spec-required GTFS file, with linked identifiers. Two
things cannot come from Trip Updates: agency metadata, which you supply,
and stop coordinates, which live in the operator’s static `stops.txt`.

``` r

feed <- rt2s_scaffold(
  events,
  agency = list(
    name = "MTA New York City Transit",
    url = "https://www.mta.info",
    timezone = "America/New_York"
  )
)
#> [INFO] route_type not given; scaffolding routes as 3 (bus).
#> Warning: 115 stop(s) have no coordinates (spec-required stop_lat/stop_lon are
#> NA). Supply 'stops' - e.g. estimated from Vehicle Positions with
#> gps2gtfs::g2g_stops_from_positions() - before publishing.
names(feed)
#> [1] "agency"         "stops"          "routes"         "trips"         
#> [5] "stop_times"     "calendar_dates" "feed_info"
head(feed$stop_times)
#>                              trip_id arrival_time departure_time stop_id
#>                               <char>       <char>         <char>  <char>
#> 1: OH_A6-Weekday-SDon-092600_M15_230     17:14:49       17:14:49  405645
#> 2: OH_A6-Weekday-SDon-092600_M15_230     17:15:37       17:15:37  401802
#> 3: OH_A6-Weekday-SDon-092600_M15_230     17:17:08       17:17:08  840005
#> 4: OH_A6-Weekday-SDon-092600_M15_230     17:17:33       17:17:33  803002
#> 5: OH_A6-Weekday-SDon-096600_M15_237     17:14:08       17:14:08  401761
#> 6: OH_A6-Weekday-SDon-096600_M15_237     17:16:46       17:16:46  401762
#>    stop_sequence
#>            <int>
#> 1:            58
#> 2:            59
#> 3:            60
#> 4:            61
#> 5:            22
#> 6:            23
```

The times are now GTFS clock strings on the service day’s clock. Each
feed carries its own publish gate. This one is not publishable yet, and
says why:

``` r

rt2s_publishable(feed)
#> $publishable
#> [1] FALSE
#> 
#> $blockers
#> [1] "115 stop(s) missing spec-required coordinates"
```

Pass a `stops` table (`stop_id`, `stop_lat`, `stop_lon`) taken from the
static feed, and set `strict = TRUE` to turn any remaining gap into an
error:

``` r

static_stops <- gtfsio::import_gtfs("path/to/mta_static.zip")$stops
feed <- rt2s_scaffold(
  events,
  agency = list(name = "MTA New York City Transit",
                url = "https://www.mta.info", timezone = "America/New_York"),
  stops = static_stops[, c("stop_id", "stop_lat", "stop_lon")],
  strict = TRUE
)
```

When you do have the operator’s planned feed, use
`rt2s_assemble(events, baseline = ...)` instead. The realized feed then
keeps the official route, trip, stop and service identifiers, so planned
and realized service join directly.

## Write it and read it back

The feed follows the [`gtfsio`](https://r-transit.github.io/gtfsio/)
convention, so
[`gtfsio::export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.html)
writes a standard GTFS zip, and any GTFS tool reads it. Here `gtfstools`
computes each trip’s duration from the file:

``` r

path <- tempfile(fileext = ".zip")
gtfsio::export_gtfs(feed, path)

gtfs <- gtfstools::read_gtfs(path)
head(gtfstools::get_trip_duration(gtfs, unit = "min"))
#> Key: <trip_id>
#>                              trip_id  duration
#>                               <char>     <num>
#> 1: OH_A6-Weekday-SDon-092600_M15_230  2.733333
#> 2: OH_A6-Weekday-SDon-096600_M15_237 74.533333
#> 3: OH_A6-Weekday-SDon-097600_M15_223 63.983333
#> 4: OH_A6-Weekday-SDon-097900_M15_235 34.466667
#> 5: OH_A6-Weekday-SDon-101600_M15_226 85.716667
#> 6: OH_A6-Weekday-SDon-101600_M15_227 89.583333
```

A trip that appears for a few minutes covers only the stops still ahead
of the bus when the snapshot was taken.

## Where next

- [From raw GPS to a realized GTFS
  feed](https://e-kotov.github.io/gtfsrt2static/articles/pipeline.html):
  the same assembly fed by `gps2gtfs` from vehicle trajectories, in both
  scaffold and baseline mode, with a planned-versus-realized comparison.
- [`vignette("frequency-feeds")`](https://e-kotov.github.io/gtfsrt2static/articles/frequency-feeds.md):
  collapsing many observed runs into frequency-based feeds at three
  reliability quantiles.
