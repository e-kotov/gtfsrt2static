# Assemble a Realized GTFS Feed from Observed Stop Events

Turns observed stop events into one static GTFS feed describing the
service that actually operated. With a `baseline` (planned) feed, the
realized feed inherits agency, routes, stops, and shapes wholesale,
keeps official trip identifiers, and replaces `stop_times.txt` with the
observed times of the trips that actually ran on `service_date` - so
planned-vs-realized joins are direct. Without a baseline, a compliant
feed is scaffolded from scratch via
[`rt2s_scaffold`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md).

## Usage

``` r
rt2s_assemble(
  events,
  baseline = NULL,
  service_date = NULL,
  tz = NULL,
  feed_lang = "en",
  feed_contact_email = NULL,
  feed_contact_url = NULL,
  ...
)
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).

- baseline:

  Optional planned static GTFS feed: a gtfsio/gtfstools-style object or
  a path to a GTFS zip.

- service_date:

  The service day the snapshot describes (one realized feed per service
  day in baseline mode). Defaults to the single date in `events`; must
  be given when events span several dates. In both modes only events
  whose `service_date` equals this day are kept and their clock strings
  are rendered relative to its GTFS origin (noon minus 12h: midnight,
  except on a daylight-saving change day); it is an error when no event
  falls on it.

- tz:

  Timezone for GTFS clock strings. Defaults to the baseline's
  `agency_timezone` (baseline mode) or "UTC".

- feed_lang:

  Primary feed language written to `feed_info.feed_lang` in both modes.
  Default `"en"`.

- feed_contact_email, feed_contact_url:

  Optional recommended `feed_info.txt` contact fields, written only when
  supplied.

- ...:

  In scaffold mode (no baseline), passed to
  [`rt2s_scaffold`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
  (`agency`, `stops`, `route_type`, `shapes`, `strict`).

## Value

A gtfsio-convention feed object (class `gtfs`); write it with
[`gtfsio::export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.html).

## Details

In baseline mode each observed event is paired with at most one planned
`stop_times` row. Where the planned trip serves a stop more than once (a
loop):

- an observed visit that carries the `stop_sequence` of one of the
  planned visits takes that visit;

- any other visit may only take a planned visit numbered above every
  visit of the trip known to be observed before it and below every one
  known to be observed after it. Known are the visits whose own
  `stop_sequence` matches the plan and, for each stop the trip serves
  once, its first observed visit if that carries no `stop_sequence`;
  other visits bound nothing;

- within those limits observed and planned visits pair in order, as many
  as possible, with the least total distance in planned time, so a
  missed first pass does not shift the second pass onto the first one's
  `stop_sequence`. An observed visit without a time comes after the
  stop's timed visits in that order and is paired by the order alone.

Because the limits come from the observed visits, a spurious detection
of a stop the trip serves once bounds the visits after it too and can
leave a real loop visit without a planned partner, and an unnumbered
loop visit can still be numbered before a stop it was observed after.
Both put times out of order, which warns (below). A planned row without
times (a non-timepoint) is placed by linear interpolation over
`stop_sequence`; without planned times the pairs follow observation
order. Observed visits are ordered by arrival; visits with the same
arrival are ordered by their own `stop_sequence`, then by departure, and
only then by input row, which warns (duplicated events are the usual
cause). An observed visit without a planned partner is numbered
chronologically after the planned ones, which can put it out of time
order. Output that repeats a `(trip_id, stop_sequence)` warns with the
count, and so does a trip in which a row arrives before the previous row
with a time departs, which is the order GTFS validators check (rows
without times are skipped, not compared; a row with only one time uses
it for both, which validators reject under another rule). The output has
exactly one row per observed event.

This is the entry point to use when each observed run should stay its
own trip. To collapse many runs into one representative trip per time
window with a `frequencies.txt` headway instead, use
[`rt2s_frequencies`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_frequencies.md).
