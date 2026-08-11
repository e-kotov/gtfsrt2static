# Frequency-based realized feed assembly. Collapses many observed runs of a
# route-direction into one representative trip per time window plus a
# frequencies.txt headway, emitting one feed per reliability quantile
# (structural / median / reliable). Builds on the summarise module for the
# analytics and reuses the scaffold module's agency/stops/publish-gate helpers
# for the spec-required surrounding files.

#' Make Stop-Time Offsets Monotone
#'
#' Rounds travel and dwell offsets to integer seconds, clamps negative values
#' to zero, and makes a stop pattern monotone with a forward pass.
#'
#' @param travel Numeric vector of arrival offsets from trip start, in seconds.
#' @param dwell Numeric vector of dwell offsets, in seconds, with the same
#'   length as `travel`.
#' @return A list with integer vectors `arrival` and `departure`. Each
#'   departure is its arrival plus its dwell, and each arrival is at least the
#'   previous departure.
#' @details Inputs must be finite numeric vectors of equal length. Values are
#'   rounded with [base::round()] before negative values are clamped to zero.
#'   For each stop, the returned arrival is the larger of its clamped travel
#'   offset and the previous returned departure. This satisfies GTFS along-trip
#'   monotonicity even when independently estimated offsets are out of order.
#' @examples
#' monotone_offsets(
#'   travel = c(-2.4, 300.6, 298.2),
#'   dwell = c(0, 30.6, 15)
#' )
#' @export
monotone_offsets <- function(travel, dwell) {
  if (
    !is.numeric(travel) ||
      !is.numeric(dwell) ||
      length(travel) != length(dwell) ||
      any(!is.finite(travel)) ||
      any(!is.finite(dwell))
  ) {
    stop(
      "'travel' and 'dwell' must be finite numeric vectors of equal length.",
      call. = FALSE
    )
  }
  travel <- pmax(0L, as.integer(round(travel)))
  dwell <- pmax(0L, as.integer(round(dwell)))
  n <- length(travel)
  arr <- integer(n)
  dep <- integer(n)
  prev <- -1L
  for (i in seq_len(n)) {
    a <- max(travel[i], prev)
    d <- a + dwell[i]
    arr[i] <- a
    dep[i] <- d
    prev <- d
  }
  list(arrival = arr, departure = dep)
}

#' Assemble Frequency-Based Realized GTFS Feeds (One per Reliability Quantile)
#'
#' Collapses many observed runs into compact, frequency-based schedules: one
#' representative trip per \code{(route, direction, window)} with a
#' \code{frequencies.txt} headway and a representative stop pattern, emitted at
#' several reliability quantiles. The typical output is three feeds -
#' \code{structural} (free-flow, p05), \code{median} (p50), and \code{reliable}
#' (p95). By default each applies its quantile to \strong{both} travel time and
#' headway, so the reliable feed is slower with longer headways than the
#' structural one; pass a list to \code{quantiles} to decouple the two (see
#' below).
#'
#' The representative stop_times are offsets from a \code{00:00:00} trip start
#' (\code{exact_times = 0} frequency semantics: only relative offsets matter),
#' clamped non-decreasing. Spec-required surrounding files (agency, routes,
#' stops, calendar, feed_info) and the publish gate are built exactly as
#' \code{\link{snapshot_scaffold}} builds them.
#'
#' @section Reconstructed versus anchored stop patterns:
#' This function \strong{reconstructs} a representative stop pattern from the
#' observations themselves, via \code{\link{obs_travel_times}} and the canonical
#' cross-trip order of \code{\link{obs_stop_order}}. That is the right regime
#' when no usable published pattern exists - GPS-only data, an operator with no
#' static feed, or a network whose published patterns do not match what runs.
#'
#' It is the \emph{wrong} regime when a published pattern does exist and the
#' analysis rests on the network being identical across scenarios. Because the
#' reconstructed stop set is derived from what was observed, two feeds built
#' from different observations can differ in their stops and stop order, and a
#' scheduled-versus-observed contrast then confounds "service got slower" with
#' "the network changed". For that design, \strong{anchor} on the published
#' pattern instead and vary only service levels: see
#' \code{\link{baseline_patterns}} and \code{pattern_source = "baseline"}.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#'   Restrict them to the service dates that form one service pattern before
#'   calling (e.g. weekdays only) - no day-type filtering is imposed here.
#' @param windows Named list of \code{c(start, end)} time strings defining the
#'   frequency windows, passed to \code{\link{time_window}} (overnight windows
#'   such as \code{c("22:00", "26:00")} are supported). Required: a
#'   frequency-based feed needs defined windows. Trips outside every window are
#'   not emitted.
#' @param quantiles Reliability quantiles in \code{[0, 1]}; one feed is produced
#'   per entry, named by its name. Two spellings are accepted:
#'   \itemize{
#'     \item a \strong{named numeric vector} - one probability per scenario,
#'       applied to both travel time and headway. Default
#'       \code{c(structural = 0.05, median = 0.5, reliable = 0.95)}.
#'     \item a \strong{named list} whose elements are either a single
#'       probability (coupled, as above) or a numeric named \code{travel} and/or
#'       \code{headway}, which \strong{decouples} the two. A side that is
#'       omitted inherits the side that is given. This is what expresses a
#'       free-flow scenario at typical frequency, e.g.
#'       \code{list(structural = c(travel = 0.05, headway = 0.50),
#'       median = 0.50, reliable = 0.95)} - a p05 running time at the
#'       \emph{median} headway, not a p05 headway.
#'   }
#'   \code{quantiles} is the single source of scenario identity in every mode:
#'   its names define which feeds are emitted, and \code{scaling}/\code{headways}
#'   may only refer to those names. Under
#'   \code{pattern_source = "baseline"} the \code{travel} side is inert (the
#'   pattern comes from \code{baseline} scaled by \code{scaling}), so a scenario
#'   may give only \code{c(headway = ...)}.
#' @param agency,stops,route_type,feed_lang,feed_contact_email,feed_contact_url,strict
#'   As in \code{\link{snapshot_scaffold}} - agency metadata, stop coordinates,
#'   route type, feed language/contacts, and the strict publish gate. Missing
#'   agency or stop coordinates are recorded as publish blockers on every
#'   returned feed (or error under \code{strict}).
#' @param service_id Identifier for the single synthesized service; its
#'   \code{calendar.txt} row is active on the weekdays present in \code{events}
#'   over the observed date range. Default \code{"SVC1"}.
#' @param exact_times \code{frequencies.exact_times}: \code{0} (default,
#'   frequency-based) or \code{1} (schedule-based).
#' @param max_headway_secs Passed to \code{\link{obs_headways}} or
#'   \code{\link{obs_headways_by_passage}}; gaps above it are treated as
#'   between-service breaks. Default 10800 (3 h).
#' @param headway_method How to estimate headways. \code{"trip_start"}
#'   (default) uses \code{\link{obs_headways}}, requiring one usable
#'   \code{trip_ref} per run. \code{"passage"} uses
#'   \code{\link{obs_headways_by_passage}}, measuring intervals at one
#'   direction-unique reference stop per route-direction. This changes only
#'   the frequency headway; representative travel-time patterns still come
#'   from \code{\link{obs_travel_times}} and need events whose \code{trip_ref}
#'   values make stop offsets meaningful.
#' @param reference_stops,min_revisit_gap_s Passed to
#'   \code{\link{obs_headways_by_passage}} when
#'   \code{headway_method = "passage"}. Explicit values are ignored with a
#'   warning when \code{headway_method = "trip_start"}.
#' @param baseline Optional planned static GTFS feed to anchor stop patterns on:
#'   a gtfsio/gtfstools-style object or a path to a GTFS zip. Required with
#'   \code{pattern_source = "baseline"} and rejected without it.
#'
#'   Its \code{agency} and \code{stops} become the defaults for those arguments
#'   (an explicit value still wins) and its \code{routes} rows are inherited so
#'   the emitted \code{route_type} stays the operator's own rather than the
#'   scaffold default. \code{calendar} and \code{shapes} are deliberately
#'   \emph{not} inherited: the calendar describes planned service while this feed
#'   describes the observed span, and the representative trips reference no
#'   baseline shape.
#'
#'   \strong{Identity contract:} \code{events$route_ref} must carry the same
#'   identifier as the baseline's \code{trips.route_id} (or
#'   \code{routes.route_short_name} under \code{route_key}), and
#'   \code{direction_id} must agree. A completely disjoint key set is an error;
#'   observed keys with no baseline pattern are dropped with a warning, and
#'   baseline routes with no observed headway are reported and skipped rather
#'   than given an invented headway.
#' @param pattern_source Where the representative stop pattern comes from.
#'   \code{"observed"} (default) reconstructs it from \code{events} via
#'   \code{\link{obs_travel_times}}. \code{"baseline"} anchors on the published
#'   pattern from \code{baseline} (see \code{\link{baseline_patterns}}) and
#'   scales it by \code{scaling}, so every scenario emits the same stops in the
#'   same order. See the section above for which regime fits.
#' @param scaling Running-time ratios, required with
#'   \code{pattern_source = "baseline"}. A data.frame with one row per
#'   \code{route_ref}, \code{direction_id}, \code{window}, \code{scenario} and a
#'   \code{ratio} column: the factor by which that scenario stretches the
#'   planned stop-to-stop offsets (1 keeps them unchanged, 1.2 is 20\% slower).
#'   Ratios must be finite and strictly positive; they are not clamped, so any
#'   plausibility bounds belong to whatever estimated them. \code{scenario}
#'   values must be names of \code{quantiles}.
#' @param scaling_missing What to do when \code{scaling} has no ratio for an
#'   emitted \code{(route, direction, window, scenario)} cell. \code{"error"}
#'   (default) reports the offending cells. \code{"drop"} removes those trips
#'   from \strong{every} scenario, with a warning - never from just the scenario
#'   that lacks a ratio, which would leave the feeds with different trip sets.
#' @param headways Optional per-cell headway override: a data.frame keyed
#'   \code{route_ref}, \code{direction_id}, \code{window}, \code{scenario} with a
#'   positive \code{headway_secs}. It supersedes the quantile-derived headway for
#'   exactly the cells it lists; unlisted cells keep their observed value. This
#'   is how a scenario whose frequency does not come from observations at all -
#'   a planned "scheduled" feed, via \code{\link{baseline_headways}} - is
#'   expressed without a second way of naming scenarios. Rows naming a cell that
#'   is not emitted are ignored with a warning.
#' @param route_key Which baseline column supplies route identity, passed to
#'   \code{\link{baseline_patterns}}. Baseline mode only.
#' @return A named list of gtfsio-convention feed objects, one per quantile
#'   (e.g. \code{$structural}, \code{$median}, \code{$reliable}); write each
#'   with \code{gtfsio::export_gtfs()}. Each carries \code{publishable} /
#'   \code{publish_blockers} attributes (see \code{\link{snapshot_publishable}}).
#'
#'   All scenario feeds share one trip set by construction: \code{trip_id} is
#'   \code{route_direction_window} and is resolved once, before any scenario is
#'   built, so a contrast between two feeds is a contrast in service levels only.
#' @export
snapshot_frequencies <- function(
  events,
  windows,
  quantiles = c(structural = 0.05, median = 0.5, reliable = 0.95),
  agency = NULL,
  stops = NULL,
  route_type = 3L,
  service_id = "SVC1",
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
  route_key = c("route_id", "route_short_name")
) {
  dt <- validate_events(events)
  q <- resolve_quantiles(quantiles)
  headway_method <- match.arg(headway_method)
  pattern_source <- match.arg(pattern_source)
  scaling_missing <- match.arg(scaling_missing)
  route_key <- match.arg(route_key)
  anchored <- identical(pattern_source, "baseline")
  if (anchored && is.null(baseline)) {
    stop(
      "pattern_source = \"baseline\" needs a 'baseline' planned static feed to ",
      "anchor on.",
      call. = FALSE
    )
  }
  if (!anchored && !is.null(baseline)) {
    stop(
      "'baseline' was given but pattern_source is \"observed\", so it would be ",
      "ignored. Pass pattern_source = \"baseline\" to anchor on it.",
      call. = FALSE
    )
  }
  if (!anchored && !is.null(scaling)) {
    stop(
      "'scaling' only applies to pattern_source = \"baseline\"; observed ",
      "patterns already carry their own travel-time quantiles.",
      call. = FALSE
    )
  }
  if (anchored && is.null(scaling)) {
    stop(
      "pattern_source = \"baseline\" needs 'scaling': a running-time ratio per ",
      "(route_ref, direction_id, window, scenario). Use ratio = 1 for a ",
      "scenario that keeps the planned running times.",
      call. = FALSE
    )
  }
  ignored_headway_args <- character()
  if (identical(headway_method, "trip_start")) {
    if (!missing(reference_stops)) {
      ignored_headway_args <- c(ignored_headway_args, "reference_stops")
    }
    if (!missing(min_revisit_gap_s)) {
      ignored_headway_args <- c(ignored_headway_args, "min_revisit_gap_s")
    }
    if (length(ignored_headway_args) > 0L) {
      warning(
        "'",
        paste(ignored_headway_args, collapse = "', '"),
        "' ignored when headway_method = \"trip_start\".",
        call. = FALSE
      )
    }
  }
  if (
    missing(windows) ||
      !is.list(windows) ||
      length(windows) == 0L ||
      is.null(names(windows)) ||
      any(!nzchar(names(windows)))
  ) {
    stop(
      "'windows' must be a non-empty named list of c(start, end) time ",
      "strings; a frequency-based feed needs defined windows.",
      call. = FALSE
    )
  }
  if (!exact_times %in% c(0L, 1L)) {
    stop("'exact_times' must be 0 or 1.", call. = FALSE)
  }
  scen <- q$scenarios

  # --- analytics (computed once, all scenarios) -----------------------------
  # Travel and headway probabilities are resolved separately (see
  # resolve_quantiles()) but share their names, so the scenario-keyed column
  # lookups below are unaffected by which spelling the caller used.
  hw <- if (identical(headway_method, "trip_start")) {
    obs_headways(
      dt,
      windows = windows,
      quantiles = q$headway,
      max_headway_secs = max_headway_secs
    )
  } else {
    obs_headways_by_passage(
      dt,
      reference_stops = reference_stops,
      windows = windows,
      quantiles = q$headway,
      min_revisit_gap_s = min_revisit_gap_s,
      max_headway_secs = max_headway_secs
    )
  }
  hw <- hw[window != "other"]
  # The representative pattern comes from one of two sources. In baseline mode
  # obs_travel_times() is not called at all: the pattern is the operator's
  # published one and the travel side of 'quantiles' is inert (documented).
  pat <- if (anchored) {
    baseline_patterns(baseline, route_key = route_key)
  } else {
    obs_travel_times(dt, q$travel)
  }
  if (nrow(hw) == 0L) {
    if (identical(headway_method, "trip_start")) {
      stop(
        "No (route, direction, window) group has a usable trip-start ",
        "headway; check that 'events' cover multiple runs inside the given ",
        "windows.",
        call. = FALSE
      )
    }
    stop(
      "No (route, direction, window) group has a usable passage headway; ",
      "check that reference-stop events contain multiple passages inside the ",
      "given windows, that input events preserve repeated visits, and that ",
      "'min_revisit_gap_s' is below the true headway.",
      call. = FALSE
    )
  }

  # --- representative trips (shared across scenarios) -----------------------
  grp <- data.table::copy(hw)
  grp[, route_id := ifelse(is.na(route_ref), "R1", as.character(route_ref))]
  grp[, trip_id := paste(
    route_id,
    ifelse(is.na(direction_id), "NA", as.character(direction_id)),
    window,
    sep = "_"
  )]

  # window bounds as clock strings (normalise "HH:MM" -> "HH:MM:SS")
  win_start <- vapply(windows, function(w) secs_to_clock(hms_to_secs(w[1])), "")
  win_end <- vapply(windows, function(w) secs_to_clock(hms_to_secs(w[2])), "")

  # (route, direction) that have a stop pattern; drop groups lacking one.
  # In observed mode obs_headways and obs_travel_times share one "served"
  # definition, so a headway group is normally guaranteed a pattern - the
  # drop/guard below are a backstop against a broken invariant, never returning
  # an empty feed. In baseline mode the two sides come from different feeds, so
  # the same join is where a mismatched route identity surfaces.
  patt_key <- unique(pat[, list(route_ref, direction_id)])
  if (anchored) {
    check_baseline_identity(grp, patt_key, route_key)
  }
  grp <- merge(
    grp,
    patt_key[, list(route_ref, direction_id, has_pattern = TRUE)],
    by = c("route_ref", "direction_id"),
    all.x = TRUE
  )
  dropped <- grp[is.na(has_pattern)]
  if (nrow(dropped) > 0L) {
    warning(
      nrow(dropped),
      " (route, direction, window) group(s) had a headway but no ",
      if (anchored) "baseline" else "served",
      " stop pattern and were dropped",
      if (anchored) {
        paste0(
          ", e.g. ",
          baseline_key_examples(unique(dropped[, list(route_ref, direction_id)])),
          "; check that 'events' route_ref uses the same identifier as the ",
          "baseline (see route_key=)"
        )
      } else {
        ""
      },
      ".",
      call. = FALSE
    )
  }
  grp <- grp[!is.na(has_pattern)]
  if (nrow(grp) == 0L) {
    stop(
      "No (route, direction, window) group with a headway has a ",
      if (anchored) "baseline" else "served",
      " stop pattern, so no trip can be built and the feed would be empty.",
      if (anchored) {
        " Check the route/direction identity shared by 'events' and 'baseline'."
      } else {
        paste0(
          " Supply 'events' with served stops for the routes that have ",
          "headways."
        )
      },
      call. = FALSE
    )
  }
  # A baseline route that did not run in any window is expected, not an error;
  # it must never be given a fabricated headway.
  unobserved <- patt_key[
    !paste(route_ref, direction_id) %in%
      unique(grp[, paste(route_ref, direction_id)])
  ]
  if (anchored && nrow(unobserved) > 0L) {
    message(
      "[INFO] ",
      nrow(unobserved),
      " baseline route-direction(s) have no observed headway in any window ",
      "and are not emitted."
    )
  }

  # Ratios resolved once, over the whole (trip x scenario) grid, *before* the
  # scenario loop. That placement is what enforces the shared trip set: a cell
  # missing a ratio for one scenario has to leave every scenario, which cannot
  # be decided from inside build_scenario().
  ratios <- if (anchored) {
    resolve_trip_ratios(grp, scaling, scen, windows, scaling_missing)
  } else {
    NULL
  }
  if (anchored) {
    keep <- unique(ratios$trip_id)
    grp <- grp[trip_id %in% keep]
    if (nrow(grp) == 0L) {
      stop(
        "'scaling' covers none of the (route, direction, window) groups that ",
        "have an observed headway, so the feed would be empty.",
        call. = FALSE
      )
    }
  }
  headways <- check_headway_overrides(headways, grp, scen)

  # --- surrounding files + publish gate (emit warnings once) ----------------
  # In baseline mode the planned feed is the natural source for the files that
  # cannot be derived from observations. It supplies *defaults* only: an
  # explicit argument still wins, and the inherited values go through the same
  # resolve_agency()/build_stops_table() path, so a baseline that is itself
  # incomplete still raises the usual publish blockers.
  if (anchored) {
    if (is.null(agency)) {
      agency <- baseline_agency(baseline)
    }
    if (is.null(stops) && !is.null(baseline$stops)) {
      stops <- baseline$stops
    }
  }
  sink <- make_blocker_sink(strict)
  emit <- sink$emit
  ag <- resolve_agency(agency, emit, strict)
  agency_id <- if (anchored) baseline_agency_id(baseline) else "AG1"

  stop_ids <- sort(unique(pat[
    paste(route_ref, direction_id) %in%
      unique(grp[, paste(route_ref, direction_id)]),
    stop_ref
  ]))
  stops_out <- build_stops_table(stop_ids, stops, emit, strict)
  blockers <- sink$blockers()

  # Inherited routes keep the operator's real route_type; scaffolding them as 3
  # would silently mislabel a tram or metro line.
  routes <- baseline_routes_table(
    route_ids = unique(grp$route_id),
    baseline_routes = if (anchored) baseline$routes else NULL,
    feed_agency_id = agency_id,
    route_type = route_type,
    route_type_given = !missing(route_type)
  )

  trips_out <- unique(grp[, list(
    route_id,
    service_id = service_id,
    trip_id,
    direction_id = as.integer(direction_id)
  )])
  data.table::setorderv(trips_out, c("route_id", "service_id", "trip_id"))

  wd <- unique(as.POSIXlt(dt$service_date)$wday) # 0 = Sun .. 6 = Sat
  day_flag <- function(target) as.integer(target %in% wd)
  calendar <- data.table::data.table(
    service_id = service_id,
    monday = day_flag(1),
    tuesday = day_flag(2),
    wednesday = day_flag(3),
    thursday = day_flag(4),
    friday = day_flag(5),
    saturday = day_flag(6),
    sunday = day_flag(0),
    start_date = yyyymmdd(min(dt$service_date)),
    end_date = yyyymmdd(max(dt$service_date))
  )

  feed_info <- data.table::data.table(
    feed_publisher_name = ag$name,
    feed_publisher_url = ag$url,
    feed_lang = feed_lang,
    feed_start_date = yyyymmdd(min(dt$service_date)),
    feed_end_date = yyyymmdd(max(dt$service_date))
  )
  if (!is.null(feed_contact_email)) {
    feed_info[, feed_contact_email := as.character(feed_contact_email)]
  }
  if (!is.null(feed_contact_url)) {
    feed_info[, feed_contact_url := as.character(feed_contact_url)]
  }

  agency_tbl <- data.table::data.table(
    agency_id = agency_id,
    agency_name = ag$name,
    agency_url = ag$url,
    agency_timezone = ag$timezone
  )

  # --- one feed per scenario ------------------------------------------------
  build_scenario <- function(s) {
    headway_col <- paste0("headway_", s)

    # Two render branches, kept separate on purpose. The observed branch runs
    # the monotone pass once per (route, direction) and fans the result out to
    # that route-direction's trips, because every window shares one pattern.
    # The baseline branch cannot: its offsets depend on a per-window ratio.
    # Running the observed path through the baseline one with ratio == 1 would
    # do n_trips x n_stops work instead of n_stops and put the exact-value
    # observed assertions at risk of drift, for no gain.
    st <- if (anchored) {
      render_pattern_baseline(pat, grp, ratios, s)
    } else {
      render_pattern_observed(pat, grp, paste0("travel_", s))
    }
    stop_times <- st[, list(
      trip_id,
      arrival_time = secs_to_clock(arr),
      departure_time = secs_to_clock(dep),
      stop_id = stop_ref,
      stop_sequence = as.integer(stop_sequence)
    )]
    data.table::setorderv(stop_times, c("trip_id", "stop_sequence"))

    freq <- grp[, list(
      trip_id,
      start_time = win_start[window],
      end_time = win_end[window],
      headway_secs = as.integer(get(headway_col)),
      exact_times = as.integer(exact_times)
    )]
    freq <- apply_headway_overrides(freq, headways, s)
    freq <- freq[!is.na(headway_secs) & headway_secs > 0L]

    feed <- list(
      agency = agency_tbl,
      stops = stops_out,
      routes = routes,
      trips = trips_out[trip_id %in% unique(stop_times$trip_id)],
      stop_times = stop_times,
      frequencies = freq,
      calendar = calendar,
      feed_info = feed_info
    )
    stamp_publishable(as_gtfs_object(feed), blockers)
  }

  stats::setNames(lapply(scen, build_scenario), scen)
}

#' Render an observation-derived pattern for one scenario
#'
#' One monotone pass per (route, direction), fanned out to that route-direction's
#' trips: in observed mode every window shares the same representative pattern.
#' @return trip_id / stop_ref / stop_sequence / arr / dep.
#' @noRd
render_pattern_observed <- function(tt, grp, travel_col) {
  tt <- data.table::copy(tt)
  data.table::setorder(tt, route_ref, direction_id, stop_sequence)
  pattern <- tt[,
    {
      m <- monotone_offsets(get(travel_col), dwell_median)
      list(
        stop_ref = stop_ref,
        stop_sequence = stop_sequence,
        arr = m$arrival,
        dep = m$departure
      )
    },
    by = list(route_ref, direction_id)
  ]
  merge(
    grp[, list(route_ref, direction_id, trip_id)],
    pattern,
    by = c("route_ref", "direction_id"),
    allow.cartesian = TRUE
  )
}

#' Render a ratio-scaled baseline pattern for one scenario
#'
#' Scales the published stop-to-stop offsets by the scenario's running-time
#' ratio. Both travel and dwell are scaled, matching the intent that the whole
#' pattern stretches in time while the network stays fixed.
#'
#' The scaling is resolved **per distinct (route, direction, ratio)**, not per
#' trip: trips that share a pattern and a ratio have byte-identical offsets, and
#' real grids carry far more trip-scenario cells than distinct pattern-ratio
#' pairs. Deduplicating here keeps the monotone pass proportional to the
#' patterns rather than to the emitted rows, which matters for callers whose
#' upstream grids are large enough to be memory-bound.
#' @return trip_id / stop_ref / stop_sequence / arr / dep.
#' @noRd
render_pattern_baseline <- function(bp, grp, ratios, s) {
  trip_ratio <- merge(
    grp[, list(route_ref, direction_id, trip_id)],
    ratios[scenario == s, list(trip_id, ratio)],
    by = "trip_id"
  )
  keys <- unique(trip_ratio[, list(route_ref, direction_id, ratio)])
  keys[, pat_key := seq_len(.N)]

  scaled <- merge(
    keys,
    bp[, list(route_ref, direction_id, stop_ref, stop_sequence, travel_base, dwell_base)],
    by = c("route_ref", "direction_id"),
    allow.cartesian = TRUE
  )
  data.table::setorder(scaled, pat_key, stop_sequence)
  scaled <- scaled[,
    {
      r <- ratio[1L]
      m <- monotone_offsets(travel_base * r, dwell_base * r)
      list(
        stop_ref = stop_ref,
        stop_sequence = stop_sequence,
        arr = m$arrival,
        dep = m$departure
      )
    },
    by = pat_key
  ]

  trip_ratio <- merge(
    trip_ratio,
    keys,
    by = c("route_ref", "direction_id", "ratio")
  )
  out <- merge(
    trip_ratio[, list(trip_id, pat_key)],
    scaled,
    by = "pat_key",
    allow.cartesian = TRUE
  )
  out[, pat_key := NULL]
  out[]
}
