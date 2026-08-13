# gtfsrt2static 0.6.0

`rt2s_frequencies()` no longer lets `events` silently bound what the feed
describes, and a group it cannot serve is now a visible drop rather than a trip
that advertises a departure it never makes.

## The object has a name now

A `(route_ref, direction_id, window)` is a **headway group** throughout the
documentation, the argument names and the user-facing messages. The term is
settled against EN 12896 (Transmodel), whose `HEADWAY JOURNEY GROUP` is "a group
of journeys defined in order to describe ... frequency-based services" and which
CEN's own DATA4PT mapping projects onto GTFS `frequencies.txt`. Previously the
docs said "cell" (~30 times) and the messages said "group" (~5 times) for the
same object. `window` is unchanged - it is the term the GTFS-analysis tooling
already uses - and `rt2s_resolved_grid()` keeps its name, since it returns the
resolved grid *of headway groups*.

## New: `headway_groups=`

* **`rt2s_frequencies(headway_groups=)`** takes the candidate headway groups
  directly, as a data.frame keyed `route_ref`, `direction_id`, `window` - the
  `scaling`/`headways` key minus `scenario`, because candidacy is a property of
  the group and not of the scenario. Candidacy becomes the events-derived groups
  **union** the supplied ones.

  Before this, a group with no rows in `events` was **never a candidate**, so it
  was absent from `rt2s_resolved_grid()` entirely rather than present with a
  `drop_reason` - even under `pattern_source = "baseline"`, where the pattern
  comes from `baseline`, the ratio from `scaling` and the headway from
  `headways`, so `events` contributes nothing to that group's output. A caller
  anchoring on a published network and estimating only per-group headways and
  ratios therefore emitted a valid but silently smaller feed that the resolved
  grid did not flag - the exact invariant the grid exists to protect.

  Valid only with `pattern_source = "baseline"`; passing it under `"observed"`
  is an error, mirroring the existing `scaling` guard. A supplied group with no
  baseline stop pattern is **not** an error: it warns and appears in the grid
  with `drop_reason = "no_stop_pattern"`.

* **`events` is now optional**, defaulting to `NULL`, when `headway_groups=` is
  supplied. No headway analytics run at all in that case, so every headway must
  come from `headways=`. `events = NULL` without `headway_groups=` is an error.

* **`rt2s_frequencies(service_dates=)`** supplies the `Date` vector the feed
  describes, replacing the dates otherwise read from `events$service_date` and
  reduced by the same rule. Required when `events` is `NULL`. Supplying
  `headway_groups=` without it warns when the supplied groups widen the feed past
  what `events` covers.

* **`rt2s_baseline_service_dates(baseline, service_id)`** is a new export, the
  third member of the `rt2s_baseline_*` family: it expands one named baseline
  service into a `Date` vector, honouring `calendar.txt` **and**
  `calendar_dates.txt` exceptions (types 1 and 2), including feeds that define a
  service through exceptions alone. This is the opt-in way to inherit the
  baseline's days - explicit, caller-chosen, and visible at the call site. It
  does **not** reverse the documented decision that the assembler never silently
  reads `baseline$calendar`.

## Behaviour changes

* **No resolvable headway is now a drop.** A headway group with no observed
  quantile and no `headways=` override is removed from **every** scenario with
  `drop_reason = "no_headway"`, exactly as `scaling_missing = "drop"` behaves and
  for the same shared-trip-set reason. This **reverses** the 0.3.0 documentation,
  which described a group emitted with `headway_secs = NA` as deliberately not a
  drop.

  It was a latent defect. Such a group wrote a trip with **no `frequencies.txt`
  row** whose `stop_times` are offsets from `00:00:00`; per GTFS - and per this
  package's own vignette - a trip absent from `frequencies.txt` is read as
  exact-time, so that trip advertised a **phantom midnight departure**. It was
  near-unreachable before (quantiles over non-empty gap pools are positive) and
  becomes the common path for any supplied group whose headway is missing, so it
  is fixed everywhere rather than worked around. `extra_trips=` is now the
  **only** way to carry service that cannot be expressed as a repeating headway,
  which is what it was built for; the two features stop overlapping.

  An emitted grid row therefore always carries a positive `headway_secs`.

* **`calendar_dates.txt` is emitted when the resolved dates have gaps.**
  `calendar.txt` reduces a span to weekday flags plus a first and last date, so a
  working set with two days missing claimed service on those two days. Each date
  inside `[min, max]` whose weekday is served but which is absent from the
  resolved set now gets an `exception_type = 2` row. **Only when gaps exist** - a
  contiguous date set emits no such file, so existing output is unchanged.

* Assembly messages naming the route/direction/window tuple were reworded for the
  new vocabulary and for `headway_groups=`; the identity error now says
  "candidate keys" rather than "observed keys", since with `events = NULL` there
  are none.

Verified byte-identical to 0.5.0 with `headway_groups = NULL`,
`service_dates = NULL` and no calendar gaps: six assembly configurations built
from a worktree at `v0.5.0` and from this version, all 120 exported files
identical.

# gtfsrt2static 0.5.0

**Every exported function was renamed.** The package had no prefix at all and
four competing sub-prefixes (`rt_`, `obs_`, `snapshot_`, `baseline_`) plus three
unprefixed orphans. The public API is now uniformly `rt2s_`, derived from the
package name the same way its sibling `gps2gtfs` derives `g2g_`, and reading as
"realtime to static". There are **no deprecated aliases**: the package has no
public users, and shims would outlive the reason they exist.

| 0.4.0 | 0.5.0 |
|---|---|
| `rt_collect()` | `rt2s_collect()` |
| `rt_coverage()` | `rt2s_archive_coverage()` |
| `rt_rotate()` | `rt2s_archive_rotate()` |
| `rt_service_template()` | `rt2s_service_template()` |
| `snapshot_from_trip_updates()` | `rt2s_events_from_trip_updates()` |
| `snapshot_from_stop_times()` | `rt2s_events_from_stop_times()` |
| `validate_events()` | `rt2s_events_validate()` |
| `obs_headways()` | `rt2s_obs_headways()` |
| `obs_headways_by_passage()` | `rt2s_obs_headways(method = "passage")` |
| `obs_travel_times()` | `rt2s_obs_travel_times()` |
| `obs_stop_order()` | `rt2s_obs_stop_order()` |
| `baseline_headways()` | `rt2s_baseline_headways()` |
| `baseline_patterns()` | `rt2s_baseline_patterns()` |
| `snapshot_assemble()` | `rt2s_assemble()` |
| `snapshot_scaffold()` | `rt2s_scaffold()` |
| `snapshot_frequencies()` | `rt2s_frequencies()` |
| `snapshot_publishable()` | `rt2s_publishable()` |
| `snapshot_grid()` | `rt2s_resolved_grid()` |
| `time_window()` | `rt2s_time_window()` |
| `monotone_offsets()` | `rt2s_monotone_offsets()` |

Two of these are more than cosmetic. `snapshot_from_stop_times()` and
`snapshot_from_trip_updates()` return **observed stop events**, not snapshots -
as their own titles already said - so the new names stop them misdescribing
their return type. `obs_`, `baseline_` and `events_` survive as *second* tokens
because they mark the stage or the evidence source: `rt2s_obs_headways()` and
`rt2s_baseline_headways()` are a deliberate pair, one measured and one planned.

* **`obs_headways_by_passage()` is removed**, folded into
  **`rt2s_obs_headways(method = c("trip_start", "passage"))`**. This is the only
  behaviour change in the release. The package already expressed this same choice
  one level up as `snapshot_frequencies(headway_method=)`, and saying it both as
  an argument and as a separate function was the defect. Both implementations are
  unchanged and dispatch is by `method`; `reference_stops` and
  `min_revisit_gap_s` move onto the merged signature, where passing either under
  `method = "trip_start"` warns rather than being silently dropped - the same
  shape the assembler already used. The passage return still carries its extra
  `reference_stop_ref` column and is otherwise column-compatible, so results feed
  the frequency path exactly as before.

  Note the argument order: `rt2s_obs_headways(events, windows, quantiles,
  max_headway_secs, method, reference_stops, min_revisit_gap_s)`. Positional
  calls written against `obs_headways()` still mean what they meant; positional
  calls written against `obs_headways_by_passage()`, whose second argument was
  `reference_stops`, do not.

* **`rt2s_frequencies()` keeps `headway_method=`**; it did *not* become
  `method=`. Inside a 20-argument assembler that also chooses a
  `pattern_source` and a `scaling_missing` policy, a bare `method` does not say
  *method of what*. The roxygen now records this so it is not later "fixed".

* **Documentation corrections in the same pass.** Two titles that were a
  question or a bare clause became noun phrases like their siblings
  (`rt2s_publishable()`, `rt2s_resolved_grid()`). Three descriptions that defined
  a public function by its internals - "the assembly module", "Used by
  `snapshot_assemble()`", "Used by `obs_headways()`" - now lead with what the
  caller gets and mention the internal relationship second.

No emitted feed byte changes in this release.

# gtfsrt2static 0.4.0

`snapshot_frequencies()` can now emit a **mixed feed**: frequency-based trips
alongside individually-timed ones. A feed built from this package no longer has
to be purely frequency-based, so cells that cannot honestly be written as a
repeating headway can be carried instead of dropped.

* **assemble (frequencies)**: new **`snapshot_frequencies(extra_trips=)`**, a
  named list keyed by scenario name - the same names as `quantiles`, which is the
  single source of scenario identity, so a misspelled scenario is an error rather
  than a silent no-op. Each element is a `list(trips=, stop_times=)` whose times
  are **absolute clock strings** (`"HH:MM:SS"`, hours >= 24 allowed), unlike the
  generated frequency trips whose `stop_times` are offsets from `00:00:00`. These
  trips reach `trips.txt` and `stop_times.txt` and get **no `frequencies.txt`
  row**, which is exactly what the GTFS specification means by an exact-time
  trip: only trips listed in `frequencies.txt` are frequency-based, and a feed
  may mix the two. An absent `service_id` is stamped with the feed's single
  synthesized service; a different one is an error.

  Their stops and routes widen `stops.txt` and `routes.txt`, but every referenced
  `stop_id` must already be known (from `stops` or an emitted pattern) and every
  `route_id` must be an emitted or baseline route. A dangling reference is an
  invalid feed and is rejected rather than filled in with placeholder
  coordinates: the lenient missing-coordinates path exists for observed data with
  genuinely unknown positions, not for a typo in a caller-supplied table.
  Collisions with a generated `route_direction_window` id, duplicate ids,
  unparsable times and times that go backwards along a trip are all hard errors
  naming the offenders.

  **No cross-scenario invariant is imposed on extra trips.** A scenario may
  supply more, fewer or none than another, because exact-time evidence
  legitimately differs by scenario - a scheduled scenario draws it from the
  timetable while the others draw it from observed passages. Requiring matching
  id sets would reject correct data. The shared-trip-set guarantee remains scoped
  to the **generated frequency trips**, where `scaling_missing` enforces it.

* **`snapshot_grid()` is unchanged and deliberately does not report extra
  trips.** The grid is one row per candidate `(route, direction, window)` cell;
  extra trips are not cells, and adding them would produce rows whose
  `route_ref`, `window`, `ratio`, `headway_secs` and `drop_reason` are all `NA`.
  No information is lost - the caller supplies the extra trips and so already
  owns their ids - but a drop-funnel check must now reconcile `trips.txt` against
  `c(grid[emitted == TRUE]$trip_id, <the ids you supplied>)`. The
  `vignette("frequency-feeds")` funnel example is corrected accordingly.

* Existing calls are unaffected: `extra_trips = NULL` (the default) reproduces
  0.3.0 output byte for byte.

# gtfsrt2static 0.3.0

`snapshot_frequencies()` now reports **what it built**, not only where it wrote
it, so a pipeline can reconcile its own cell accounting against the emitted feed
instead of re-deriving the outcome from the written files.

* **assemble (frequencies)**: `snapshot_frequencies()` now reports what it built,
  not only where it wrote it. The returned list carries a `resolved_grid`
  attribute, read with the new **`snapshot_grid()`**: one row per candidate
  `(route_ref, direction_id, window)` and scenario, with the `ratio` and
  `headway_secs` actually applied, whether the headway was `"observed"` or came
  from a `headways=` `"override"`, the generated `trip_id`, and whether the cell
  was `emitted`. Cells that never reached the feed are **present and flagged**
  rather than absent - `drop_reason` is `"no_stop_pattern"` (a headway with no
  served or baseline pattern) or `"no_ratio"` (removed from every scenario under
  `scaling_missing = "drop"`) - so a caller that reconciles its own cell
  accounting stage by stage can close the funnel against the feed instead of
  re-deriving the outcome from the written files. The grid's row count is
  invariant across drop stages, which is the property such a check asserts
  against. A cell can be emitted with `headway_secs = NA`: the trip and its
  stop_times are written but no `frequencies.txt` row is, because the group's
  headway quantile was missing or non-positive; that is not a drop and carries no
  `drop_reason`.

* **tests**: the opt-in MobilityData validator harness accepts
  `GTFSRT2STATIC_VALIDATOR_JAR`, a path to an already-downloaded validator jar,
  and reuses one download across scenarios otherwise. Batch nodes routinely have
  enough network to satisfy `skip_if_offline()` while still being unable to reach
  GitHub, which made the download the harness's least reliable step; a cached jar
  removes the network requirement entirely.

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
