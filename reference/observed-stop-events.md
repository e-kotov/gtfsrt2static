# The Observed Stop Events Schema

Observed stop events are the package's canonical intermediate table -
the point where every input path converges before assembly and
comparison: one row per trip, stop, and service date, holding the
*actual* (or best observed) arrival and departure times plus a
provenance audit trail.

## Details

Columns:

- trip_ref:

  Trip identity. The GTFS-RT/baseline `trip_id` when known, a synthetic
  id for inferred trips. Never NA.

- route_ref:

  Route identity, NA when unknown.

- shape_ref:

  Shape identity linking the trip to `shapes.txt`, NA when unknown. Set
  by `rt2s_events_from_stop_times(shape_ref_prefix=)` to match the ids
  produced by
  [`gps2gtfs::g2g_shapes_from_trips()`](https://rdrr.io/pkg/gps2gtfs/man/g2g_shapes_from_trips.html);
  the assembler writes it to `trips.shape_id`.

- direction_id:

  GTFS direction (0/1), NA when unknown.

- service_date:

  `Date`. The service day the trip belongs to (attributed by trip start;
  post-midnight stops keep their trip's date).

- stop_ref:

  Stop identity; NA only on trip-level rows
  (`provenance == "canceled"`).

- stop_sequence:

  Integer order along the trip; optional (NA), the assembler guarantees
  a spec-compliant sequence.

- arrival_time, departure_time:

  Absolute `POSIXct` times; the assembler derives GTFS clock strings
  (with \>24:00:00) from them.

- provenance:

  One of `"observed"` (reported at/after passage), `"predicted-last"`
  (last prediction before passage), `"propagated"`, `"skipped"` (stop
  not served), `"canceled"` (whole trip did not run).

- vehicle_ref:

  Vehicle identity, NA when unknown.

- source:

  One of `"trip_updates"`, `"positions"`, `"gps"`.

- pattern_ref:

  *Optional, additive.* K-way branch/short-turn variant identity carried
  through from the gps2gtfs C5 contract; the handoff to the cross-trip
  stop-order stage. Reserved nullable - NA until a gps2gtfs pattern
  detector is enabled - and only present when the producer supplies it,
  so converters that do not emit it stay valid.
