# gtfsrt2static

> **Experimental** — APIs will change without deprecation.

Turn archived GTFS-Realtime feeds (and GPS-derived observations) into
**standard-compliant static GTFS snapshots of the service that actually ran**,
ready for comparison against the planned schedule.

## How it fits together

```
live endpoints ──rt2s_collect()──► daily .pb ZIPs
                                     │
      Trip Updates ──(gtfsrealtime)──┴──► rt2s_events_from_trip_updates() ──┐
                                                                         ├─► observed stop events ─► rt2s_assemble() ─► realized GTFS zip
      Vehicle Positions / raw GPS ──(gps2gtfs)─► rt2s_events_from_stop_times() ─┘        (baseline feed optional)
```

- [`gtfsrealtime`](https://cran.r-project.org/package=gtfsrealtime) parses
  GTFS-RT protobuf into data frames (not this package's job).
- [`gps2gtfs`](https://github.com/e-kotov/gps2gtfs) infers trips and stop
  times from raw coordinates (not this package's job either).
- **This package** does everything that is neither protobuf nor spatial:
  collecting feeds, converging both input paths on a canonical
  *observed stop events* table, and assembling compliant feeds.

## Modules

| Module | Functions | Job |
|---|---|---|
| collect | `rt2s_collect()`, `rt2s_archive_rotate()`, `rt2s_archive_coverage()`, `rt2s_service_template()` | archive ephemeral GTFS-RT endpoints as daily `.pb` ZIPs (never parses; raw bytes only) |
| events | `rt2s_events_from_trip_updates()`, `rt2s_events_from_stop_times()`, `rt2s_events_validate()` | reduce observations to one row per trip × stop × service date, with provenance |
| summarise | `rt2s_obs_headways()`, `rt2s_obs_travel_times()`, `rt2s_obs_stop_order()`, `rt2s_time_window()` | reduce many observed runs to headway / travel-time / dwell quantiles and a cross-trip canonical stop order |
| baseline | `rt2s_baseline_patterns()`, `rt2s_baseline_headways()` | reduce a *planned* static feed to one canonical stop pattern per route-direction, and to its own scheduled headways |
| assemble | `rt2s_assemble()`, `rt2s_scaffold()`, `rt2s_frequencies()`, `rt2s_resolved_grid()` | merge events into a baseline feed (official IDs preserved), scaffold a compliant feed from scratch, or collapse many runs into a frequency-based feed — and read back which cells were emitted, with what, and why the rest were dropped |

## Quick example

```r
library(gtfsrt2static)

# Trip Updates archive -> realized feed for one day
updates <- gtfsrealtime::read_gtfsrt_trip_updates("tu/20260714.zip", "America/New_York")
events  <- rt2s_events_from_trip_updates(updates)
feed    <- rt2s_assemble(events, baseline = "planned_gtfs.zip")
gtfsio::export_gtfs(feed, "realized_20260714.zip")

# Vehicle Positions (no Trip Updates, no baseline)
positions <- gtfsrealtime::read_gtfsrt_positions("vp/20260714.zip", "America/New_York")
result <- gps2gtfs::g2g_extract_trips_and_stop_times(positions, terminals, stops, ...)
events <- rt2s_events_from_stop_times(result$stop_times)
feed   <- rt2s_assemble(
  events,
  agency = list(name = "My Agency", url = "https://...", timezone = "America/New_York"),
  stops  = gps2gtfs::g2g_stops_from_positions(positions)
)

# Collapse many runs into a frequency-based feed at three reliability quantiles
feeds <- rt2s_frequencies(
  events,
  windows = list(am_peak = c("06:00", "09:00"), midday = c("09:00", "16:00")),
  stops   = stops_with_coords,
  agency  = list(name = "My Agency", url = "https://...", timezone = "America/New_York")
)
feeds$structural; feeds$median; feeds$reliable   # p05 / p50 / p95 feeds

# If trip_ref is not per-run, estimate headways from reference-stop passages
feeds <- rt2s_frequencies(
  events,
  windows = list(am_peak = c("06:00", "09:00"), midday = c("09:00", "16:00")),
  stops   = stops_with_coords,
  agency  = list(name = "My Agency", url = "https://...", timezone = "America/New_York"),
  headway_method = "passage"
)
```

Supplying `reference_stops` restricts passage-headway output to
route-directions that serve one of those stops. The same choice is available
one level down as `rt2s_obs_headways(method = "passage")`, which is where
`reference_stops` and `min_revisit_gap_s` are documented.

### Anchoring on a planned feed

The calls above *reconstruct* the stop pattern from the observations. When the
operator publishes a usable feed, anchor on it instead: the stops and their order
come from the timetable, and only a running-time ratio and a headway vary per
route and window. Every scenario then emits an identical trip set, so a
scheduled-versus-observed contrast measures service rather than network
differences.

```r
sched <- rt2s_baseline_headways(static, windows = windows)
sched$scenario <- "scheduled"

feeds <- rt2s_frequencies(
  events,
  windows = windows,
  quantiles = list(
    scheduled  = c(headway = 0.50),                 # travel side unused
    structural = c(travel = 0.05, headway = 0.50),  # free-flow at typical frequency
    median     = c(travel = 0.50, headway = 0.50),
    reliable   = c(travel = 0.95, headway = 0.95)
  ),
  baseline = static,
  pattern_source = "baseline",
  scaling  = ratios,   # ratio per route/direction/window/scenario, yours to estimate
  headways = sched
)
```

`quantiles` accepts a list so travel time and headway can differ per scenario;
a bare named numeric still applies one probability to both. See
`vignette("frequency-feeds")` for the identity contract between `events` and the
baseline.

### Mixing in individually-timed trips

Real service is not purely frequency-based. Per the GTFS specification only trips
listed in `frequencies.txt` are frequency-based, and the rest are read from
`stop_times` as exact scheduled times — so a cell that cannot be written as a
repeating headway is carried as an ordinary timed trip. `extra_trips=` takes
those, keyed by scenario, with absolute clock times and no `frequencies.txt` row:

```r
feeds <- rt2s_frequencies(
  events,
  windows = windows,
  stops   = stops_with_coords,
  extra_trips = list(median = list(trips = my_trips, stop_times = my_stop_times))
)
```

A scenario may supply more, fewer or no extra trips than another: exact-time
evidence legitimately differs by scenario, so no cross-scenario invariant is
imposed on them. They are not rows of `rt2s_resolved_grid()` — the grid is one row per
candidate cell — so reconcile `trips.txt` against the emitted cells plus the ids
you supplied.

See `vignette("frequency-feeds")` for the frequency workflow end to end. The
opt-in validator test exercises representative frequency feeds with the
MobilityData `gtfs-validator`.

## Installation

```r
# install.packages("pak")
pak::pak("e-kotov/gtfsrt2static")
```

## Design notes

- Both input paths converge on the **observed stop events** schema
  (`?"observed-stop-events"`): trip × stop × service date × actual times ×
  provenance (`observed` / `predicted-last` / `propagated` / `skipped` /
  `canceled`).
- Times are absolute `POSIXct` internally; `stop_times.txt` clock strings
  (including `>24:00:00` for post-midnight stops) are derived only at
  assembly.
- The collector never parses protobuf: archive fidelity must not depend on
  parser versions, and malformed responses are evidence worth keeping.
- Output follows the [gtfsio](https://r-transit.github.io/gtfsio/) convention,
  so [gtfstools](https://ipea.github.io/gtfstools/) and
  [tidytransit](https://r-transit.github.io/tidytransit/) work on it directly.

## License

MIT © Egor Kotov
