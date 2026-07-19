# gtfsrt2static 0.0.0.9000

Initial development version.

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
