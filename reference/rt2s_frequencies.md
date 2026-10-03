# Assemble Frequency-Based Realized GTFS Feeds (One per Reliability Quantile)

Collapses many observed runs into compact, frequency-based schedules:
one representative trip per `(route, direction, window)` with a
`frequencies.txt` headway and a representative stop pattern, emitted at
several reliability quantiles. The typical output is three feeds -
`structural` (free-flow, p05), `median` (p50), and `reliable` (p95). By
default each applies its quantile to **both** travel time and headway,
so the reliable feed is slower with longer headways than the structural
one; pass a list to `quantiles` to decouple the two (see below).

## Usage

``` r
rt2s_frequencies(
  events = NULL,
  windows,
  quantiles = c(structural = 0.05, median = 0.5, reliable = 0.95),
  agency = NULL,
  stops = NULL,
  route_type = 3L,
  service_id = "SVC1",
  service_dates = NULL,
  exact_times = 0L,
  feed_lang = "en",
  feed_contact_email = NULL,
  feed_contact_url = NULL,
  strict = FALSE,
  max_headway_secs = 3L * 3600L,
  headway_method = c("trip_start", "passage"),
  reference_stops = NULL,
  min_revisit_gap_s = 600L,
  baseline = NULL,
  pattern_source = c("observed", "baseline"),
  scaling = NULL,
  scaling_missing = c("error", "drop"),
  headways = NULL,
  headway_groups = NULL,
  route_key = c("route_id", "route_short_name"),
  extra_trips = NULL,
  strict_within_window = FALSE,
  closed_last = FALSE
)
```

## Arguments

- events:

  Observed stop events (see
  [observed-stop-events](https://e-kotov.github.io/gtfsrt2static/reference/observed-stop-events.md)).
  Restrict them to the service dates that form one service pattern
  before calling (e.g. weekdays only) - no day-type filtering is imposed
  here.

  May be `NULL` when `headway_groups` is supplied, in which case no
  headway analytics run at all and every headway must come from
  `headways`. `NULL` without `headway_groups` is an error: there would
  be no candidate headway group.

- windows:

  Named list of `c(start, end)` time strings defining the frequency
  windows, passed to
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md)
  (overnight windows such as `c("22:00", "26:00")` are supported). The
  window name `"other"` is reserved for unassigned service times. When
  `strict_within_window = TRUE`, configured windows must not overlap.
  Required: a frequency-based feed needs defined windows. Trips outside
  every window are not emitted.

- quantiles:

  Reliability quantiles in `[0, 1]`; one feed is produced per entry,
  named by its name. Two spellings are accepted:

  - a **named numeric vector** - one probability per scenario, applied
    to both travel time and headway. Default
    `c(structural = 0.05, median = 0.5, reliable = 0.95)`.

  - a **named list** whose elements are either a single probability
    (coupled, as above) or a numeric named `travel` and/or `headway`,
    which **decouples** the two. A side that is omitted inherits the
    side that is given. This is what expresses a free-flow scenario at
    typical frequency, e.g.
    `list(structural = c(travel = 0.05, headway = 0.50), median = 0.50, reliable = 0.95)` -
    a p05 running time at the *median* headway, not a p05 headway.

  `quantiles` is the single source of scenario identity in every mode:
  its names define which feeds are emitted, and `scaling`/`headways` may
  only refer to those names. Under `pattern_source = "baseline"` the
  `travel` side is inert (the pattern comes from `baseline` scaled by
  `scaling`), so a scenario may give only `c(headway = ...)`.

- agency, stops, route_type, feed_lang, feed_contact_email,
  feed_contact_url, strict:

  As in
  [`rt2s_scaffold`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md) -
  agency metadata, stop coordinates, route type, feed language/contacts,
  and the strict publish gate. Missing agency or stop coordinates are
  recorded as publish blockers on every returned feed (or error under
  `strict`).

- service_id:

  Identifier for the single synthesized service; its `calendar.txt` row
  is active on the weekdays present in the resolved service dates, over
  their range. Default `"SVC1"`.

- service_dates:

  Optional `Date` vector giving the days this feed describes. It
  replaces the dates that would otherwise be read from
  `events$service_date`, and is reduced by the same rule: weekday flags
  plus the minimum and maximum date. Required when `events` is `NULL`;
  [`rt2s_baseline_service_dates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_service_dates.md)
  is the way to inherit the baseline's own days.

  Dates inside `[min, max]` whose weekday *is* served but which are
  absent from this vector are written to `calendar_dates.txt` with
  `exception_type = 2`, so a span with holes in it is not overstated as
  uninterrupted service. A date set with no holes emits no
  `calendar_dates.txt` at all.

  Supplying `headway_groups` without `service_dates` warns when the
  supplied groups widen the feed past what `events` covers: the calendar
  then still describes only the observed span.

- exact_times:

  `frequencies.exact_times`: `0` (default, frequency-based) or `1`
  (schedule-based).

- max_headway_secs:

  Passed to
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md);
  gaps above it are treated as between-service breaks. Default 10800 (3
  h).

- headway_method:

  How to estimate headways, passed to
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
  as its `method`. `"trip_start"` (default) requires one usable
  `trip_ref` per run; `"passage"` measures intervals at one
  direction-unique reference stop per route-direction. This changes only
  the frequency headway; representative travel-time patterns still come
  from
  [`rt2s_obs_travel_times`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_travel_times.md)
  and need events whose `trip_ref` values make stop offsets meaningful.

  The argument is deliberately **not** named `method` here, even though
  that is what
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
  calls it. The two live at different altitudes: inside a 20-argument
  assembler that also chooses a `pattern_source` and a `scaling_missing`
  policy, a bare `method` does not say *method of what*. It is not an
  oversight to be tidied up.

- reference_stops, min_revisit_gap_s:

  Passed to
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
  when `headway_method = "passage"`. Explicit values are ignored with a
  warning when `headway_method = "trip_start"`.

- baseline:

  Optional planned static GTFS feed to anchor stop patterns on: a
  gtfsio/gtfstools-style object or a path to a GTFS zip. Required with
  `pattern_source = "baseline"` and rejected without it.

  Its `agency` and `stops` become the defaults for those arguments (an
  explicit value still wins) and its `routes` rows are inherited so the
  emitted `route_type` stays the operator's own rather than the scaffold
  default. `calendar` and `shapes` are deliberately *not* inherited: the
  calendar describes planned service while this feed describes the
  observed span, and the representative trips reference no baseline
  shape.

  **Identity contract:** the `route_ref` of `events` and of
  `headway_groups` must carry the same identifier as the baseline's
  `trips.route_id` (or `routes.route_short_name` under `route_key`), and
  `direction_id` must agree. A completely disjoint key set is an error;
  candidate keys with no baseline pattern are dropped with a warning,
  and baseline routes that are in no candidate headway group are
  reported and skipped rather than given an invented headway.

- pattern_source:

  Where the representative stop pattern comes from. `"observed"`
  (default) reconstructs it from `events` via
  [`rt2s_obs_travel_times`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_travel_times.md).
  `"baseline"` anchors on the published pattern from `baseline` (see
  [`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md))
  and scales it by `scaling`, so every scenario emits the same stops in
  the same order. See the section above for which regime fits.

- scaling:

  Running-time ratios, required with `pattern_source = "baseline"`. A
  data.frame with one row per `route_ref`, `direction_id`, `window`,
  `scenario` and a `ratio` column: the factor by which that scenario
  stretches the planned stop-to-stop offsets (1 keeps them unchanged,
  1.2 is 20\\ Ratios must be finite and strictly positive; they are not
  clamped, so any plausibility bounds belong to whatever estimated them.
  `scenario` values must be names of `quantiles`.

- scaling_missing:

  What to do when `scaling` has no ratio for an emitted headway
  group/scenario pair. `"error"` (default) reports the offending pairs.
  `"drop"` removes those trips from **every** scenario, with a warning -
  never from just the scenario that lacks a ratio, which would leave the
  feeds with different trip sets.

- headways:

  Optional per-group headway override: a data.frame keyed `route_ref`,
  `direction_id`, `window`, `scenario` with a positive `headway_secs`.
  It supersedes the quantile-derived headway for exactly the headway
  groups it lists; unlisted groups keep their observed value. This is
  how a scenario whose frequency does not come from observations at
  all - a planned "scheduled" feed, via
  [`rt2s_baseline_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_headways.md) -
  is expressed without a second way of naming scenarios. Rows naming a
  headway group that is not emitted are ignored with a warning.

  A group with **no** resolvable headway in some scenario - no observed
  quantile and no override - is dropped from **every** scenario with
  `drop_reason = "no_within_window_headway"` or `"no_headway"`, exactly
  as `scaling_missing = "drop"` behaves and for the same shared-trip-set
  reason. Use `extra_trips` to carry service that cannot be written as a
  repeating headway.

- headway_groups:

  Optional candidate headway groups, supplied rather than derived: a
  data.frame keyed `route_ref`, `direction_id`, `window` - the
  `scaling`/`headways` key minus `scenario`, because candidacy is a
  property of the group and not of the scenario. Valid only with
  `pattern_source = "baseline"`; passing it under `"observed"` is an
  error, since an observed pattern can only be reconstructed for a group
  that has events.

  Candidacy becomes the events-derived groups **union** these, so a
  group with no observed runs is still emitted when `scaling` gives it a
  ratio and `headways` gives it a headway. A supplied group with no
  baseline stop pattern is *not* an error: it warns and appears in
  [`rt2s_resolved_grid`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_resolved_grid.md)
  with `drop_reason = "no_stop_pattern"`, which is what makes a caller's
  drop funnel able to see it.

- route_key:

  Which baseline column supplies route identity, passed to
  [`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md).
  Baseline mode only.

- extra_trips:

  Optional individually-timed trips to add to the emitted feeds, as a
  **named list keyed by scenario name** - the same names as `quantiles`,
  which is the single source of scenario identity, so a misspelled
  scenario is an error rather than a silent no-op. Each element is a
  `list(trips=, stop_times=)`:

  - `trips`: required `trip_id` and `route_id`; optional `direction_id`
    and `service_id`. An absent `service_id` is stamped with this feed's
    single synthesized service; a different one is an error.

  - `stop_times`: required `trip_id`, `arrival_time`, `departure_time`,
    `stop_id`, `stop_sequence`. Times are **absolute clock strings**
    (`"HH:MM:SS"`, hours \>= 24 allowed for trips running past
    midnight), *not* offsets from `00:00:00` as the generated frequency
    trips use, and must be non-decreasing along the trip.

  Any other element of the list is ignored, so a builder that also
  returns provenance can be passed through unchanged.

  These trips get **no `frequencies.txt` row**. That is what makes them
  exact-time trips: per the GTFS specification only trips listed in
  `frequencies.txt` are frequency-based, and the rest are read from
  `stop_times` as scheduled times. A feed may mix the two, so this is a
  standard-compliant feed, not a workaround - it is how a headway group
  that cannot be expressed as a repeating headway is carried. Since a
  group with no resolvable headway is now a drop, this is the **only**
  way to carry such service.

  Their stops and routes are added to `stops.txt` and `routes.txt`, but
  every referenced `stop_id` must already be known (from `stops` or an
  emitted pattern) and every `route_id` must be an emitted or baseline
  route: a dangling reference is an invalid feed and is rejected rather
  than filled in. No extra `trip_id` may collide with a generated
  `route_direction_window` id.

  **No cross-scenario invariant is imposed.** A scenario may supply
  more, fewer or no extra trips than another, because the exact-time
  evidence for a headway group legitimately differs by scenario. The
  shared-trip-set guarantee below is scoped to the *generated frequency*
  trips, which is where `scaling_missing` enforces it.

  Extra trips are **not** rows of
  [`rt2s_resolved_grid`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_resolved_grid.md):
  the grid is one row per candidate `(route, direction, window)` headway
  group and extra trips are not groups. Reconciling a feed's `trips.txt`
  therefore means
  `c(grid[emitted == TRUE]$trip_id, <the ids you supplied>)`; the caller
  supplies the extra trips, so it already owns those ids.

- strict_within_window:

  Passed to
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md)
  when estimating headways from events. Logical; default `FALSE`. When
  `TRUE`, configured windows must be pairwise non-overlapping.

- closed_last:

  Logical; when `TRUE` the last window in list order is closed on its
  end, so an event exactly on it is inside that window rather than
  unassigned. Passed to
  [`rt2s_obs_headways`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_headways.md).
  See
  [`rt2s_time_window`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_time_window.md).

## Value

A named list of gtfsio-convention feed objects, one per quantile (e.g.
`$structural`, `$median`, `$reliable`); write each with
[`gtfsio::export_gtfs()`](https://r-transit.github.io/gtfsio/reference/export_gtfs.html).
Each carries `publishable` / `publish_blockers` attributes (see
[`rt2s_publishable`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_publishable.md)).

All scenario feeds share one *generated* trip set by construction:
`trip_id` is `route_direction_window` and is resolved once, before any
scenario is built, so a contrast between two feeds is a contrast in
service levels only. Trips added through `extra_trips` are outside that
guarantee by design and may differ per scenario.

The list carries a `resolved_grid` attribute: one row per candidate
`(route, direction, window)` headway group and scenario, recording the
ratio and headway actually applied and, for groups that never reached
the feed, why. Read it with
[`rt2s_resolved_grid`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_resolved_grid.md).
Groups dropped for want of a stop pattern, a ratio or a headway are
present and flagged rather than absent, so the grid reconciles against a
caller's own drop accounting. With `strict_within_window = TRUE`, an
observed group with fewer than two starts inside a window is retained
with `drop_reason = "no_within_window_headway"`.

## Details

The representative stop_times are offsets from a `00:00:00` trip start
(`exact_times = 0` frequency semantics: only relative offsets matter),
clamped non-decreasing. Spec-required surrounding files (agency, routes,
stops, calendar, feed_info) and the publish gate are built exactly as
[`rt2s_scaffold`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_scaffold.md)
builds them.

## Reconstructed versus anchored stop patterns

This function **reconstructs** a representative stop pattern from the
observations themselves, via
[`rt2s_obs_travel_times`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_travel_times.md)
and the canonical cross-trip order of
[`rt2s_obs_stop_order`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_obs_stop_order.md).
That is the right regime when no usable published pattern exists -
GPS-only data, an operator with no static feed, or a network whose
published patterns do not match what runs.

It is the *wrong* regime when a published pattern does exist and the
analysis rests on the network being identical across scenarios. Because
the reconstructed stop set is derived from what was observed, two feeds
built from different observations can differ in their stops and stop
order, and a scheduled-versus-observed contrast then confounds "service
got slower" with "the network changed". For that design, **anchor** on
the published pattern instead and vary only service levels: see
[`rt2s_baseline_patterns`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_patterns.md)
and `pattern_source = "baseline"`.

## Where the candidate headway groups come from

A **headway group** is one `(route_ref, direction_id, window)` - the
object `frequencies.txt` describes, and what EN 12896 (Transmodel) calls
a headway journey group. One representative trip is emitted per group.

By default the candidate groups are derived from `events`: a group with
no observed runs is not a candidate. Under `pattern_source = "baseline"`
that is the wrong gate, because the pattern comes from `baseline`, the
ratio from `scaling` and the headway from `headways`, so `events`
contributes nothing to such a group's output. `headway_groups` names
those groups directly; candidacy then becomes the events-derived groups
**union** the supplied ones, and `events` may be `NULL` entirely.

## See also

[`rt2s_resolved_grid`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_resolved_grid.md)
for the resolved grid,
[`rt2s_baseline_service_dates`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_baseline_service_dates.md)
for `service_dates`.
