# gtfsrt2static 0.0.0.9001 (development version)

* **frequency feeds validated on real data.** `snapshot_frequencies()` was run
  end to end on a real NYC MTA GTFS-Realtime Trip Updates snapshot (a
  coordinate-complete 25-route Manhattan slice, 537 trips). All three
  reliability feeds pass the MobilityData `gtfs-validator` v6.0.0 with zero
  ERROR notices; referential integrity, `stop_times` monotonicity, and the
  publish gate were asserted programmatically.
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
