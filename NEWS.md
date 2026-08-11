# gtfsrt2static 0.2.0

Frequency feeds can now be **anchored on a published static feed** instead of
reconstructed from observations, and travel-time and headway scenarios can be
specified independently. Together these let a scheduled-versus-observed contrast
hold the network fixed, so it measures service and not network differences.

* **assemble (frequencies)**: `snapshot_frequencies(quantiles=)` accepts a named
  *list* as well as a named numeric, so a scenario can carry separate `travel`
  and `headway` probabilities - e.g.
  `list(structural = c(travel = 0.05, headway = 0.50))`, a free-flow running time
  at a typical frequency. A bare named numeric still means "the same probability
  for both", so existing calls and the default are unchanged. An omitted side
  inherits the side that is given. `quantiles` remains the single source of
  scenario identity in every mode.

* **baseline**: `baseline_patterns()` reduces a planned static feed to one
  canonical stop pattern per `(route, direction)` - the signature carried by the
  most trips, tie-broken on more stops then lexicographically, with the lowest
  `trip_id` among the winners as the template. Offsets are rebased on the
  template's **first departure** (so `travel_base` is negative at the origin) and
  `stop_sequence` is renumbered densely from 1. Trips are excluded from candidacy
  when `direction_id` is `NA` or a time is missing, so the modal rule picks among
  fully timed trips rather than erroring on the one it would have chosen;
  `direction_id` is read from `trips.txt` and never inferred from a `trip_id`
  convention.

* **assemble (frequencies)**: `snapshot_frequencies(pattern_source = "baseline",
  baseline=, scaling=)` emits the published pattern scaled by a per-cell
  running-time ratio, as one representative trip plus a `frequencies.txt`
  headway. `scaling` is keyed
  `(route_ref, direction_id, window, scenario)`; ratios must be finite and
  strictly positive and are **not** clamped, since plausibility bounds belong to
  whatever estimated them. Ratios are resolved for the whole trip x scenario grid
  *before* any feed is built, so a cell missing a ratio leaves every scenario and
  all feeds keep one shared trip set - `scaling_missing` chooses between erroring
  (default) and dropping. Offsets are resolved once per distinct
  `(pattern, ratio)` pair rather than per trip, which keeps the work proportional
  to the patterns rather than to the emitted rows.

  In this mode the baseline supplies the defaults for `agency` and `stops` and its
  `routes` rows are inherited, so the emitted `route_type` stays the operator's
  own instead of the scaffolded 3 (bus); explicit arguments still win, and
  inherited values go through the same publish-blocker path. `calendar` and
  `shapes` are deliberately not inherited.

* **baseline**: `baseline_headways()` gives the planned feed's own headways per
  `(route, direction, window)` from its scheduled departure gaps, and
  `snapshot_frequencies(headways=)` injects them as a per-cell override. Together
  these express a "scheduled" scenario - published running times *and* published
  frequency - which no combination of observed quantiles could produce. Unlike
  `obs_headways()` there is no within-day grouping, since a static feed has no
  service dates.

* **frequency feeds**: the opt-in MobilityData validator test now covers a
  four-scenario baseline-anchored build (scheduled / structural / median /
  reliable over two windows), asserting the shared trip set across all four feeds
  and zero ERROR-severity notices from `gtfs-validator` v6.0.0 for each. It passes
  no `agency`/`stops`/`route_type`, so it exercises baseline inheritance under
  `strict = TRUE` rather than only the scaling arithmetic.

* **docs**: `snapshot_frequencies()` and `obs_stop_order()` now state when to
  **reconstruct** a pattern from observations and when to **anchor** on a
  published one, and note that `obs_stop_order()`'s median-offset rule is the
  same rule a GPS-only pattern builder would use - so a pipeline can slide from
  anchored to reconstructed with no visible signal. The `frequency-feeds`
  vignette gains an "Anchoring on a planned static feed" section.

* **known divergence**: at the origin stop, `monotone_offsets()` keeps the scaled
  origin dwell, so `departure_time` there is `dwell * ratio` rather than
  `00:00:00`. Every stop after the first is unaffected: in any valid baseline the
  second arrival is at or after the first departure, so the monotone forward pass
  never binds. Downstream code that reimplemented this by flattening the origin
  departure to zero will see that one field differ.

# gtfsrt2static 0.1.0

* **assemble (frequencies)**: `monotone_offsets()` is now exported. It rounds
  travel and dwell offsets to integer seconds, clamps negative inputs to zero,
  and ensures every arrival is no earlier than the preceding departure.

* **summarise**: `obs_headways_by_passage()` estimates headways from successive
  vehicle passages at one direction-unique reference stop per route-direction.
  It is useful when `trip_ref` is not a per-run identifier: consecutive
  detections of the same vehicle at a stop are collapsed with
  `min_revisit_gap_s`, missing `vehicle_ref` degrades to one passage per
  detection with a warning, shared bidirectional stops are rejected or skipped,
  unknown-direction rows are warned and excluded from output, and output
  headway columns match `obs_headways()`. A converter that has already reduced
  repeated visits cannot supply passage headways.
* **assemble (frequencies)**: `snapshot_frequencies(headway_method = "passage")`
  can opt into passage-derived frequency headways while leaving the default
  trip-start method unchanged. Supplying `reference_stops` restricts passage
  coverage to route-directions that serve those stops.
* **frequency feeds**: the opt-in validator test exercises representative
  frequency feeds with the MobilityData `gtfs-validator`; referential
  integrity, `stop_times` monotonicity, and the publish gate are also asserted
  programmatically.
* **vignette** `frequency-feeds` walks the frequency workflow from observed
  events to the three-quantile feeds, on both a self-contained synthetic
  example and a real bundled Trip Updates snapshot.
* **events**: `snapshot_from_trip_updates()` now drops stop-time updates that
  reference no stop at all (neither `stop_id` nor `stop_sequence`) with an
  `[INFO]` message, alongside the existing `NO_DATA` drop. Real feeds emit a few
  such empty rows; they previously became NA-`stop_ref` events that failed the
  schema validator.

# gtfsrt2static 0.0.0.9000

Initial development version.

* **summarise**: `time_window()`, `obs_headways()`, `obs_travel_times()`, and
  `obs_stop_order()` reduce the observed stop events table to the analytical
  inputs of a frequency-based feed. `obs_headways()` gives headway quantiles per
  route/direction/time-window (within-day gaps, spurious gaps dropped);
  `obs_travel_times()` gives per-stop travel-time and dwell quantiles as a
  representative pattern; `obs_stop_order()` derives the cross-trip canonical
  stop order from each stop's median offset from trip start (tie-broken by
  `stop_ref`), warning when a stop is revisited within a trip (loop/branch).
  No weekday filtering is imposed - the caller restricts service dates.
  All three reductions count only served passages: `skipped`/`canceled` stops
  and untimed rows are excluded. A stop served on no run yields no row rather
  than an all-NA one (its dwell defaults to 0), and a canceled trip contributes
  no `obs_headways()` start, so stale timestamps on unserved rows cannot shrink
  the observed headway.

* **events**: `snapshot_from_trip_updates()`, `snapshot_from_stop_times()`,
  and `validate_events()` reduce GTFS-Realtime Trip Updates or gps2gtfs stop
  times to the canonical observed-stop-events schema.
  - `snapshot_from_stop_times()` is POSIXct-native, attributes one service
    day per trip (day of first stop, so overnight trips stay whole), and
    repairs legacy `"HH:MM:SS"` input that wraps across midnight.
  - `trip_id_col` (auto-detecting a `provided_trip_id` column) promotes an
    official trip id to a verbatim `trip_ref`, so baseline-mode assembly can
    preserve official trip identifiers instead of synthesizing them.
* **assemble**: `snapshot_assemble()` (baseline mode: inherits files, keeps
  official IDs) and `snapshot_scaffold()` (baseline-free compliant feed;
  linked IDs; `>24:00:00` time encoding).
* **assemble (frequencies)**: `snapshot_frequencies()` collapses many observed
  runs into compact frequency-based feeds - one representative trip per
  route/direction/time-window plus a `frequencies.txt` headway - and emits one
  feed per reliability quantile (default `structural`/`median`/`reliable` =
  p05/p50/p95), applying the quantile to both travel time and headway.
  Representative `stop_times` are offsets from a `00:00:00` trip start
  (`exact_times = 0` semantics), clamped non-decreasing. Reuses the scaffold
  agency/stops/publish-gate machinery, so the same `strict` / `publishable`
  contract applies.
  - `strict = TRUE` turns placeholder-agency and missing-coordinate warnings
    into errors, as a publish gate. The default stays `FALSE` so exploratory
    and analysis use (where partial feeds are fine) is not blocked.
  - Assembled feeds carry `publishable` / `publish_blockers` attributes, read
    via `snapshot_publishable()` — a programmatic publish gate so a downstream
    step can check readiness instead of relying on a human reading warnings.
  - `shapes` are linked to `trips.shape_id` via the events' `shape_ref` (set
    by `snapshot_from_stop_times(shape_ref_prefix=)` to match
    `gps2gtfs::g2g_shapes_from_trips()`); unreferenced shapes are pruned and
    dangling references warn (or error under `strict`).
  - `feed_lang` (default `"en"`) and optional `feed_contact_email` /
    `feed_contact_url` populate `feed_info.txt`.
  - Errors instead of returning an empty feed when every headway group lacks a
    served stop pattern; GTFS clock rendering fails loudly on an NA/negative
    offset rather than emitting `"NA:NA:NA"`.
* **events**: the observed-stop-events schema gains a `shape_ref` column
  (NA when unknown).
  - `snapshot_from_trip_updates()` synthesizes a trip identity from the
    GTFS-RT TripDescriptor (`route_id`, `direction_id`, `start_date`,
    `start_time`) when `trip_id` is absent, so producers that identify trips
    only by descriptor (e.g. HSL) group correctly without manual preprocessing.
* **collect**: `rt_collect()`, `rt_rotate()`, `rt_coverage()`,
  `rt_service_template()` for raw GTFS-RT archiving.
* **validation**: opt-in MobilityData `gtfs-validator` harness
  (`tests/testthat/test-validator.R`, gated on
  `GTFSRT2STATIC_RUN_VALIDATOR=1`). A scaffolded overnight feed passes with
  zero ERROR notices (validator v6.0.0).
