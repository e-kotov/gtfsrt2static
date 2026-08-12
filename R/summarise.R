# Observed-service summarisation on the canonical observed stop events table
# (see ?"observed-stop-events"). These helpers are the analytical layer between
# observed runs and a frequency-based realized feed: headways, travel-time and
# dwell quantiles, and the cross-trip canonical stop order used to build a
# single representative stop pattern from many passages. They are pure
# reductions of C6 and never infer coordinates or parse protobuf.

# --- internal time helpers --------------------------------------------------

#' Seconds since midnight for "HH:MM" / "HH:MM:SS" strings
#' @noRd
hms_to_secs <- function(x) {
  vapply(
    strsplit(as.character(x), ":", fixed = TRUE),
    function(p) {
      p <- suppressWarnings(as.integer(p))
      if (length(p) == 2L) p <- c(p, 0L)
      if (length(p) != 3L || anyNA(p)) {
        return(NA_integer_)
      }
      p[1] * 3600L + p[2] * 60L + p[3]
    },
    integer(1)
  )
}

#' Seconds since midnight for time-of-day bucketing.
#'
#' With \code{service_date}, returns seconds since the \emph{service day's}
#' midnight (in \code{tz}), so a post-midnight stop attributed to the previous
#' service date exceeds 86400 (e.g. 00:30 next day -> 88200) - matching the
#' GTFS >24:00:00 service-day convention and letting overnight windows such as
#' c("22:00", "26:00") work. Without it, POSIXct falls back to wall-clock
#' time-of-day (wraps at midnight). Integer/numeric input passes through.
#' @noRd
time_of_day_secs <- function(x, service_date = NULL, tz = NULL) {
  # Empty in, empty out: guard the POSIXct branch below, where
  # paste(character(0), "00:00:00") would recycle to " 00:00:00" and blow up
  # as.POSIXct(). Reachable when a group has no served trip start.
  if (length(x) == 0L) {
    return(numeric(0))
  }
  if (inherits(x, "POSIXct")) {
    if (!is.null(service_date)) {
      if (is.null(tz) || !nzchar(tz)) tz <- attr(x, "tzone")
      if (is.null(tz) || !nzchar(tz)) tz <- "UTC"
      midnight <- as.POSIXct(
        paste(as.character(as.Date(service_date)), "00:00:00"),
        tz = tz
      )
      return(as.numeric(difftime(x, midnight, units = "secs")))
    }
    lt <- as.POSIXlt(x)
    return(lt$hour * 3600 + lt$min * 60 + lt$sec)
  }
  as.numeric(x)
}

#' Validate a probs vector for the summary quantile arguments
#' @noRd
check_quantiles <- function(quantiles) {
  if (
    !is.numeric(quantiles) ||
      length(quantiles) == 0L ||
      is.null(names(quantiles)) ||
      any(!nzchar(names(quantiles)))
  ) {
    stop(
      "'quantiles' must be a non-empty *named* numeric vector, e.g. ",
      "c(median = 0.5, p95 = 0.95).",
      call. = FALSE
    )
  }
  if (anyNA(quantiles) || any(quantiles < 0 | quantiles > 1)) {
    stop("'quantiles' values must all lie in [0, 1].", call. = FALSE)
  }
  invisible(quantiles)
}

#' Split the Scenario Quantile Argument into Travel and Headway Probabilities
#'
#' The frequency assembler needs one probability per scenario for travel time
#' and, independently, one for headway: a "structural" free-flow scenario is
#' typically p05 running time at the \emph{median} headway, not a p05 headway.
#' This resolves both accepted spellings of that argument into two probability
#' vectors carrying the \strong{same names in the same order}, which is what
#' lets the assembler keep addressing scenarios by name alone
#' (\code{travel_<name>} / \code{headway_<name>}).
#'
#' Accepted forms:
#' \itemize{
#'   \item a named numeric - one probability per scenario, used for both sides
#'     (the historical form, so existing callers and the default are unchanged);
#'   \item a named list whose elements are either a single probability (coupled
#'     for that scenario) or a numeric named \code{travel} and/or
#'     \code{headway}. A side that is absent inherits the side that is present,
#'     so \code{c(headway = 0.5)} is a legal shorthand for a scenario whose
#'     travel times do not come from an observed quantile at all (baseline
#'     mode).
#' }
#'
#' Range validation is delegated to `check_quantiles()` on each resolved vector
#' so its error messages stay identical whichever form was supplied.
#' @return A list with `scenarios` (character), `travel` and `headway` (named
#'   numeric vectors, identically named and ordered).
#' @noRd
resolve_quantiles <- function(quantiles, arg = "quantiles") {
  # A data.frame is a list, so the list branch below would reach into its
  # columns and report a baffling element-shape error. Reject it up front.
  if (inherits(quantiles, "data.frame")) {
    stop(
      "'",
      arg,
      "' must be a named numeric vector or a named list, not a data.frame.",
      call. = FALSE
    )
  }

  if (is.numeric(quantiles)) {
    check_quantiles(quantiles)
    check_unique_scenarios(names(quantiles), arg)
    return(list(
      scenarios = names(quantiles),
      travel = quantiles,
      headway = quantiles
    ))
  }

  if (
    !is.list(quantiles) ||
      length(quantiles) == 0L ||
      is.null(names(quantiles)) ||
      any(!nzchar(names(quantiles)))
  ) {
    stop(
      "'",
      arg,
      "' must be a non-empty *named* numeric vector, e.g. ",
      "c(median = 0.5, p95 = 0.95), or a non-empty *named* list, e.g. ",
      "list(structural = c(travel = 0.05, headway = 0.5)).",
      call. = FALSE
    )
  }
  scen <- names(quantiles)
  check_unique_scenarios(scen, arg)

  # Per scenario, resolve the (travel, headway) pair. Anything that is not a
  # bare probability or a travel/headway-named numeric is rejected naming the
  # offending scenario - a mistyped side name is otherwise silent.
  pair <- function(x, s) {
    if (!is.numeric(x) && !is.list(x)) {
      stop(
        "'",
        arg,
        "[[\"",
        s,
        "\"]]' must be a single probability, or a numeric named 'travel' ",
        "and/or 'headway'.",
        call. = FALSE
      )
    }
    x <- unlist(x, use.names = TRUE)
    if (!is.numeric(x)) {
      stop(
        "'",
        arg,
        "[[\"",
        s,
        "\"]]' must be numeric.",
        call. = FALSE
      )
    }
    nm <- names(x)
    if (length(x) == 1L && (is.null(nm) || !nzchar(nm))) {
      return(c(travel = x[[1L]], headway = x[[1L]]))
    }
    if (is.null(nm) || any(!nzchar(nm)) || !all(nm %in% c("travel", "headway"))) {
      stop(
        "'",
        arg,
        "[[\"",
        s,
        "\"]]' must be a single probability, or a numeric named 'travel' ",
        "and/or 'headway'; got ",
        if (is.null(nm)) {
          paste0(length(x), " unnamed value(s)")
        } else {
          paste0("name(s) '", paste(nm, collapse = "', '"), "'")
        },
        ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(nm) > 0L) {
      stop(
        "'",
        arg,
        "[[\"",
        s,
        "\"]]' names 'travel'/'headway' more than once.",
        call. = FALSE
      )
    }
    # An absent side inherits the present one, so a scenario can name only the
    # side that is actually used (see the baseline-mode note above).
    tr <- if ("travel" %in% nm) x[["travel"]] else x[["headway"]]
    hw <- if ("headway" %in% nm) x[["headway"]] else x[["travel"]]
    c(travel = tr, headway = hw)
  }

  pairs <- lapply(scen, function(s) pair(quantiles[[s]], s))
  travel <- stats::setNames(
    vapply(pairs, function(p) p[["travel"]], numeric(1L)),
    scen
  )
  headway <- stats::setNames(
    vapply(pairs, function(p) p[["headway"]], numeric(1L)),
    scen
  )
  check_quantiles(travel)
  check_quantiles(headway)
  list(scenarios = scen, travel = travel, headway = headway)
}

#' Reject duplicate scenario names
#'
#' Scenario names address feed elements and analytics columns
#' (\code{travel_<name>}), so a duplicate would silently collide instead of
#' producing two feeds. Enforced only here, not in `check_quantiles()`, whose
#' leaf callers must keep their current contract.
#' @noRd
check_unique_scenarios <- function(scen, arg) {
  dup <- unique(scen[duplicated(scen)])
  if (length(dup) > 0L) {
    stop(
      "'",
      arg,
      "' has duplicate scenario name(s): '",
      paste(dup, collapse = "', '"),
      "'. Each name becomes one feed and one column suffix, so they must be ",
      "unique.",
      call. = FALSE
    )
  }
  invisible(scen)
}

#' Validate a positive scalar seconds argument
#' @noRd
check_positive_seconds <- function(x, arg) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      x <= 0
  ) {
    stop("'", arg, "' must be a single positive number.", call. = FALSE)
  }
  invisible(as.numeric(x))
}

#' Integer quantile matching as.integer(quantile(...)) (type 7,
#' truncated toward zero).
#' @noRd
int_quantile <- function(x, p) {
  as.integer(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
}

# --- internal offset / ordering primitives ----------------------------------

#' Provenance values that mark a stop the vehicle actually served (i.e. not
#' \code{skipped}/\code{canceled}). Unserved rows carry NA - or, from a
#' misbehaving producer, stale - times, so they are kept out of both headway and
#' pattern summaries. Shared by \code{trip_start_times} and
#' \code{served_offsets} so the two agree on what "served" means.
#' @noRd
is_served_provenance <- function(provenance) {
  !(as.character(provenance) %in% c("skipped", "canceled"))
}

#' One trip-start time per (service_date, trip_ref): the first served arrival.
#' Trips with no served arrival (e.g. wholly canceled) get no row, so a canceled
#' trip does not manufacture a spurious start that would shorten headways.
#' @noRd
trip_start_times <- function(dt) {
  dt[
    is_served_provenance(provenance) & !is.na(arrival_time),
    list(trip_start = min(arrival_time)),
    by = list(service_date, trip_ref)
  ]
}

#' Attach trip_start and per-event offset (arrival seconds from trip start).
#' @noRd
add_trip_offsets <- function(dt) {
  starts <- trip_start_times(dt)
  dt <- merge(dt, starts, by = c("service_date", "trip_ref"), all.x = TRUE)
  dt[, offset := as.numeric(arrival_time - trip_start, units = "secs")]
  dt
}

#' Served observations only: rows that actually contribute a timed passage.
#'
#' Drops unserved stops - \code{skipped}/\code{canceled} provenance (see
#' \code{is_served_provenance}) and any row whose \code{offset} is missing (no
#' timed arrival) or whose \code{stop_ref} is missing. Unserved rows carry NA -
#' or stale - times; letting them through collapses a stop's quantiles to NA,
#' which renders downstream as "NA:NA:NA" GTFS clock strings. This uses the same
#' served definition as \code{trip_start_times}, so any headway group is
#' guaranteed a served stop pattern.
#' @noRd
served_offsets <- function(dt_off) {
  dt_off[
    is_served_provenance(provenance) &
      !is.na(stop_ref) &
      !is.na(offset)
  ]
}

#' Canonical cross-trip stop order from a dt that already carries 'offset'.
#' @noRd
compute_stop_order <- function(dt_off) {
  served <- served_offsets(dt_off)
  dups <- served[
    ,
    list(n = .N),
    by = list(service_date, trip_ref, stop_ref)
  ][n > 1L]
  if (nrow(dups) > 0L) {
    warning(
      "A stop_ref is visited more than once within a trip in ",
      nrow(dups),
      " case(s) (loop/branch service). The median-offset ranking collapses ",
      "the repeated visits into one position; inspect these route-directions ",
      "before trusting the representative pattern.",
      call. = FALSE
    )
  }
  ord <- served[
    ,
    list(canonical_offset = as.numeric(stats::median(offset))),
    by = list(route_ref, direction_id, stop_ref)
  ]
  data.table::setorder(ord, route_ref, direction_id, canonical_offset, stop_ref)
  ord[, stop_sequence := seq_len(.N), by = list(route_ref, direction_id)]
  ord[]
}

# --- exported API -----------------------------------------------------------

#' Classify Times of Day into Named Service Windows
#'
#' Assigns each time to a named window (e.g. "am_peak"), returning one window
#' name per input time. Use it for any time-of-day bucketing of observed
#' service; it is also the function that gives \code{windows=} its meaning
#' throughout the package, so a window definition that behaves as you expect
#' here behaves the same way in \code{\link{rt2s_obs_headways}} and
#' \code{\link{rt2s_frequencies}}.
#'
#' @param x A \code{POSIXct} vector (its own timezone is respected) or integer
#'   seconds since midnight.
#' @param windows Named list of length-2 character vectors \code{c(start, end)}
#'   in "HH:MM" or "HH:MM:SS", e.g.
#'   \code{list(am_peak = c("06:00", "09:00"), pm_peak = c("16:00", "19:00"))}.
#'   End values may exceed 24:00 for overnight windows, e.g.
#'   \code{c("22:00", "26:00")} (requires \code{service_date}). Intervals are
#'   half-open \code{[start, end)} and the first matching window in list order
#'   wins, so overlaps resolve deterministically. \code{NULL} (default) places
#'   every non-missing time in a single \code{"all"} window.
#' @param service_date Optional \code{Date} vector (recycled to \code{x}). When
#'   supplied with POSIXct \code{x}, times are measured from the service day's
#'   midnight, so a post-midnight stop of a trip attributed to the previous
#'   service date classifies as e.g. 24:30 (88200 s) rather than 00:30 - the
#'   GTFS >24:00:00 convention. Without it, POSIXct uses wall-clock time-of-day.
#' @param tz Timezone of the service day (defaults to \code{x}'s own timezone,
#'   else "UTC"). Only used when \code{service_date} is supplied.
#' @return Character vector the length of \code{x}: the window name,
#'   \code{"other"} for a time matching no window (never produced when
#'   \code{windows} is \code{NULL}), and \code{NA} where \code{x} is \code{NA}.
#' @examples
#' rt2s_time_window(
#'   as.POSIXct(c("2026-07-14 07:30:00", "2026-07-14 12:00:00"), tz = "UTC"),
#'   list(am_peak = c("06:00", "09:00"))
#' )
#' # Overnight service attributed to the previous service date:
#' rt2s_time_window(
#'   as.POSIXct("2026-07-15 00:30:00", tz = "UTC"),
#'   list(overnight = c("22:00", "26:00")),
#'   service_date = as.Date("2026-07-14")
#' )
#' @export
rt2s_time_window <- function(x, windows = NULL, service_date = NULL, tz = NULL) {
  secs <- time_of_day_secs(x, service_date = service_date, tz = tz)
  if (is.null(windows)) {
    out <- rep("all", length(secs))
    out[is.na(secs)] <- NA_character_
    return(out)
  }
  if (!is.list(windows) || is.null(names(windows)) || any(!nzchar(names(windows)))) {
    stop(
      "'windows' must be a named list of c(start, end) time strings.",
      call. = FALSE
    )
  }
  out <- rep(NA_character_, length(secs))
  for (nm in names(windows)) {
    w <- windows[[nm]]
    if (length(w) != 2L) {
      stop("Window '", nm, "' must be a length-2 c(start, end).", call. = FALSE)
    }
    s <- hms_to_secs(w[1])
    e <- hms_to_secs(w[2])
    if (is.na(s) || is.na(e)) {
      stop("Window '", nm, "' has an unparseable time.", call. = FALSE)
    }
    hit <- !is.na(secs) & is.na(out) & secs >= s & secs < e
    out[hit] <- nm
  }
  out[is.na(out) & !is.na(secs)] <- "other"
  out
}

#' Cross-Trip Canonical Stop Order for Each Route-Direction
#'
#' Collapses many observed passages of a route-direction into one stop order.
#' Each stop's canonical position is its \strong{median offset from trip start}
#' across all passages, tie-broken by \code{stop_ref}. This is the cross-trip
#' complement to the per-trip chronological \code{stop_sequence} the events
#' converters already produce: when passages disagree on order (e.g. several
#' stops share an arrival second), per-trip chronology is ambiguous but the
#' median offset is not.
#'
#' @section Reconstructed versus anchored stop patterns:
#' This function \strong{reconstructs} a stop order from the observations, which
#' is what you want when no usable published pattern exists. It is the wrong
#' tool when one does exist and an analysis depends on the network being
#' identical across the scenarios being compared: the order here is derived from
#' what was observed, so different observations can yield different stop sets,
#' and a scheduled-versus-observed contrast then confounds "service changed"
#' with "the network changed". For that design, \strong{anchor} on the published
#' pattern with \code{\link{rt2s_baseline_patterns}} and vary only service levels.
#'
#' Worth knowing how easily the two are confused: the median-offset rule below
#' is the same rule a GPS-only pattern builder would use, so a pipeline can slide
#' from anchored to reconstructed without any visible signal.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @return A data.table with one row per served \code{(route_ref, direction_id,
#'   stop_ref)}: \code{canonical_offset} (median seconds from trip start) and
#'   \code{stop_sequence} (1-based order within the route-direction, by
#'   \code{canonical_offset} then \code{stop_ref}). Only served passages count:
#'   \code{skipped}/\code{canceled} rows and rows without a timed arrival are
#'   excluded, so a stop that was never actually served (e.g. skipped on every
#'   run) produces no row rather than a placeholder position. A stop visited
#'   more than once within a single trip (loop/branch service) triggers a
#'   warning, since the median collapses the repeated visits into one position.
#' @export
rt2s_obs_stop_order <- function(events) {
  dt <- add_trip_offsets(rt2s_events_validate(events))
  compute_stop_order(dt)
}

#' Observed Headways per Route-Direction and Time Window
#'
#' Reduces observed service to headway quantiles per \code{(route_ref,
#' direction_id, window)}. Two kinds of evidence can carry a headway, and
#' \code{method} picks between them; both drop non-positive gaps and gaps longer
#' than \code{max_headway_secs} as spurious (vehicles resuming after a layover,
#' data gaps), and neither applies weekday/weekend filtering - restrict the
#' \code{events} to the service dates you want summarised before calling.
#'
#' \code{method = "trip_start"} (default) measures the gaps between consecutive
#' \strong{trip starts} \emph{within one service date}. A trip's start is its
#' first \strong{served} arrival: \code{skipped}/\code{canceled} rows are
#' ignored, so a canceled trip (which never operated) contributes no start and
#' cannot shrink the observed headway, even if its rows carry stale timestamps.
#'
#' @section Passage headways:
#' \code{method = "passage"} instead measures the intervals between successive
#' vehicle passages at one reference stop per route-direction. This is what to
#' use when \code{trip_ref} cannot identify individual trips (for example, an
#' all-day block identifier): \code{"trip_start"} would see one trip start,
#' but a reference-stop passage sequence can still reveal the service headway.
#'
#' The reference stop must identify a single direction within its route. A stop
#' observed in multiple known directions is rejected when supplied explicitly,
#' and is excluded from automatic selection. Rows with unknown
#' \code{direction_id} do not make a stop shared, but they are warned and
#' excluded from passage-headway output because they cannot be assigned to a
#' route-direction group. If every candidate reference-stop row lacks
#' \code{direction_id}, the function errors after this exclusion; a caller who
#' knows a stop is one-directional should stamp \code{direction_id} on their
#' events before calling. Automatic selection chooses the best-observed
#' direction-unique stop for each route-direction.
#'
#' Consecutive detections of the same vehicle at the same reference stop are
#' collapsed into one passage until the vehicle has been absent for at least
#' \code{min_revisit_gap_s}. A chain of detections each spaced below
#' \code{min_revisit_gap_s} remains one passage even if the chain's total span
#' is longer. This keeps a dwell or repeated GPS fix from becoming many
#' artificial headways.
#'
#' @param events Observed stop events (see \link{observed-stop-events}). For
#'   \code{method = "passage"}, GPS observations should first be converted with
#'   \code{\link{rt2s_events_from_stop_times}}, so they use the same provenance
#'   and source conventions as the rest of the package.
#'   \code{\link{rt2s_events_from_trip_updates}} keeps one row per trip, service
#'   date, and stop, so it exposes at most one passage per stop per trip and
#'   cannot recover repeated visits that were already reduced.
#' @param windows Time-window definition passed to \code{\link{rt2s_time_window}};
#'   \code{NULL} (default) treats each day as one \code{"all"} window.
#' @param quantiles Named numeric vector of probabilities in \code{[0, 1]};
#'   each becomes an integer-seconds column \code{headway_<name>}. Default
#'   \code{c(median = 0.5, p95 = 0.95)}.
#' @param max_headway_secs Upper cutoff; gaps above it are treated as
#'   between-service breaks and excluded. Default 10800 (3 h).
#' @param method Which evidence carries the headway: \code{"trip_start"}
#'   (default) or \code{"passage"}, as described above.
#' @param reference_stops Passage method only. Optional character vector of stop
#'   ids to consider as reference stops. If \code{NULL}, the best-observed
#'   direction-unique stop is selected for each route-direction. If multiple
#'   supplied stops match a route-direction, the best-observed one is used.
#'   Supplying \code{reference_stops} restricts output to route-directions that
#'   serve one of those stops. Explicit values are ignored with a warning under
#'   \code{method = "trip_start"}.
#' @param min_revisit_gap_s Passage method only. Minimum seconds between two
#'   passages of the same vehicle at the same reference stop. Detections closer
#'   together are treated as one passage. If \code{vehicle_ref} is missing, dwell
#'   revisits cannot be distinguished from following vehicles, so each
#'   unknown-vehicle detection is treated as a separate passage and this gap does
#'   not apply to it. Default 600 (10 min). Explicit values are ignored with a
#'   warning under \code{method = "trip_start"}.
#' @return A data.table with columns \code{route_ref}, \code{direction_id},
#'   \code{window}, one \code{headway_<name>} column per quantile (integer
#'   seconds), and \code{n_headways} (count of headways summarised). Groups with
#'   no usable headway (e.g. a single run) produce no row.
#'
#'   \code{method = "passage"} additionally carries
#'   \code{reference_stop_ref}, the stop the passages were measured at. The
#'   headway columns and \code{n_headways} are identical across the two methods,
#'   so either result can feed the frequency assembly path. If served events
#'   exist but no direction-unique reference stop can be resolved, the passage
#'   method errors instead of returning an empty table.
#' @export
rt2s_obs_headways <- function(
  events,
  windows = NULL,
  quantiles = c(median = 0.5, p95 = 0.95),
  max_headway_secs = 3L * 3600L,
  method = c("trip_start", "passage"),
  reference_stops = NULL,
  min_revisit_gap_s = 600L
) {
  method <- match.arg(method)
  if (identical(method, "trip_start")) {
    # An argument that cannot apply warns rather than being silently dropped;
    # missing() is what distinguishes "explicitly passed" from "left at default".
    ignored <- character()
    if (!missing(reference_stops)) {
      ignored <- c(ignored, "reference_stops")
    }
    if (!missing(min_revisit_gap_s)) {
      ignored <- c(ignored, "min_revisit_gap_s")
    }
    if (length(ignored) > 0L) {
      warning(
        "'",
        paste(ignored, collapse = "', '"),
        "' ignored when method = \"trip_start\".",
        call. = FALSE
      )
    }
    return(headways_by_trip_start(
      events,
      windows = windows,
      quantiles = quantiles,
      max_headway_secs = max_headway_secs
    ))
  }
  headways_by_passage(
    events,
    reference_stops = reference_stops,
    windows = windows,
    quantiles = quantiles,
    min_revisit_gap_s = min_revisit_gap_s,
    max_headway_secs = max_headway_secs
  )
}

#' Headways between consecutive trip starts (method = "trip_start")
#' @noRd
headways_by_trip_start <- function(
  events,
  windows = NULL,
  quantiles = c(median = 0.5, p95 = 0.95),
  max_headway_secs = 3L * 3600L
) {
  dt <- rt2s_events_validate(events)
  check_quantiles(quantiles)
  max_headway_secs <- check_positive_seconds(
    max_headway_secs,
    "max_headway_secs"
  )
  tz <- attr(dt$arrival_time, "tzone")
  if (is.null(tz) || !nzchar(tz)) tz <- "UTC"

  starts <- trip_start_times(dt)
  trip_meta <- unique(dt[, list(service_date, trip_ref, route_ref, direction_id)])
  trips <- merge(starts, trip_meta, by = c("service_date", "trip_ref"))
  data.table::setorder(
    trips,
    route_ref,
    direction_id,
    service_date,
    trip_start,
    trip_ref
  )
  trips[,
    headway_secs := c(NA_real_, diff(as.numeric(trip_start))),
    by = list(route_ref, direction_id, service_date)
  ]
  trips[
    headway_secs <= 0 | headway_secs > max_headway_secs,
    headway_secs := NA_real_
  ]
  trips[,
    window := rt2s_time_window(trip_start, windows, service_date = service_date, tz = tz)
  ]

  qn <- names(quantiles)
  summ <- trips[
    !is.na(headway_secs),
    c(
      stats::setNames(
        lapply(quantiles, function(p) int_quantile(headway_secs, p)),
        paste0("headway_", qn)
      ),
      list(n_headways = .N)
    ),
    by = list(route_ref, direction_id, window)
  ]
  data.table::setorder(summ, route_ref, direction_id, window)
  summ[]
}

#' Headways between successive reference-stop passages (method = "passage")
#'
#' The reference stop must identify a single direction within its route, and
#' consecutive detections of the same vehicle are collapsed until it has been
#' absent for `min_revisit_gap_s`. The public contract lives on
#' [rt2s_obs_headways()].
#' @noRd
headways_by_passage <- function(
  events,
  reference_stops = NULL,
  windows = NULL,
  quantiles = c(median = 0.5, p95 = 0.95),
  min_revisit_gap_s = 600L,
  max_headway_secs = 3L * 3600L
) {
  dt <- rt2s_events_validate(events)
  check_quantiles(quantiles)
  min_revisit_gap_s <- check_positive_seconds(
    min_revisit_gap_s,
    "min_revisit_gap_s"
  )
  max_headway_secs <- check_positive_seconds(
    max_headway_secs,
    "max_headway_secs"
  )
  tz <- attr(dt$arrival_time, "tzone")
  if (is.null(tz) || !nzchar(tz)) tz <- "UTC"

  served <- data.table::copy(dt[
    is_served_provenance(provenance) &
      !is.na(stop_ref) &
      !is.na(arrival_time)
  ])
  if (nrow(served) == 0L) {
    return(empty_passage_headways(quantiles))
  }

  refs <- resolve_reference_stops(served, reference_stops)
  warn_missing_reference_groups(served, refs)

  ref_events <- merge(
    served,
    refs[, list(route_ref, direction_id, stop_ref)],
    by = c("route_ref", "direction_id", "stop_ref")
  )
  if (nrow(ref_events) == 0L) {
    # Reference-stop resolution normally reports why no eligible rows remain.
    # Keep a typed shell here as a last-resort fallback for empty resolved sets.
    return(empty_passage_headways(quantiles))
  }

  ref_events[, vehicle_key := as.character(vehicle_ref)]
  n_missing_vehicle <- sum(is.na(ref_events$vehicle_key))
  if (n_missing_vehicle > 0L) {
    ref_events[
      is.na(vehicle_key),
      vehicle_key := paste0("<unknown_vehicle_", seq_len(.N), ">")
    ]
    warning(
      n_missing_vehicle,
      " reference-stop detection(s) have no 'vehicle_ref'; each is treated ",
      "as a separate passage because a dwell revisit cannot be distinguished ",
      "from a following vehicle without vehicle identity. ",
      "'min_revisit_gap_s' does not apply to them.",
      call. = FALSE
    )
  }
  data.table::setorder(
    ref_events,
    route_ref,
    direction_id,
    service_date,
    stop_ref,
    vehicle_key,
    arrival_time,
    departure_time,
    trip_ref
  )
  ref_events[,
    prev_arrival := data.table::shift(arrival_time),
    by = list(route_ref, direction_id, service_date, stop_ref, vehicle_key)
  ]
  ref_events[,
    new_passage := is.na(prev_arrival) |
      as.numeric(arrival_time - prev_arrival, units = "secs") >=
        min_revisit_gap_s
  ]
  ref_events[,
    passage_id := cumsum(new_passage),
    by = list(route_ref, direction_id, service_date, stop_ref, vehicle_key)
  ]

  passages <- ref_events[
    ,
    list(passage_time = min(arrival_time)),
    by = list(
      route_ref,
      direction_id,
      service_date,
      reference_stop_ref = stop_ref,
      vehicle_key,
      passage_id
    )
  ]
  data.table::setorder(
    passages,
    route_ref,
    direction_id,
    service_date,
    reference_stop_ref,
    passage_time,
    vehicle_key,
    passage_id
  )
  passages[,
    headway_secs := c(NA_real_, diff(as.numeric(passage_time))),
    by = list(route_ref, direction_id, service_date, reference_stop_ref)
  ]
  passages[
    headway_secs <= 0 | headway_secs > max_headway_secs,
    headway_secs := NA_real_
  ]
  passages[,
    window := rt2s_time_window(
      passage_time,
      windows,
      service_date = service_date,
      tz = tz
    )
  ]

  qn <- names(quantiles)
  summ <- passages[
    !is.na(headway_secs),
    c(
      stats::setNames(
        lapply(quantiles, function(p) int_quantile(headway_secs, p)),
        paste0("headway_", qn)
      ),
      list(n_headways = .N)
    ),
    by = list(route_ref, direction_id, window, reference_stop_ref)
  ]
  if (nrow(summ) == 0L) {
    passage_counts <- passages[
      ,
      list(n_passages = .N),
      by = list(route_ref, direction_id, service_date, reference_stop_ref)
    ]
    if (nrow(passage_counts) > 0L && all(passage_counts$n_passages == 1L)) {
      warning(
        "Every route-direction/date/reference-stop group produced only one ",
        "passage, so no passage headways can be computed. This often means ",
        "the input events came from a converter that already reduced repeated ",
        "visits, or that 'min_revisit_gap_s' is larger than the true headway.",
        call. = FALSE
      )
    } else {
      warning(
        "No route-direction/date/reference-stop group produced a usable ",
        "passage headway after applying passage filters; check ",
        "'min_revisit_gap_s', non-positive passage gaps, and ",
        "'max_headway_secs'.",
        call. = FALSE
      )
    }
  }
  data.table::setorder(summ, route_ref, direction_id, window)
  summ[]
}

#' Empty output shell for passage headways
#' @noRd
empty_passage_headways <- function(quantiles) {
  cols <- stats::setNames(
    replicate(length(quantiles), integer(), simplify = FALSE),
    paste0("headway_", names(quantiles))
  )
  out <- data.table::data.table(
    route_ref = character(),
    direction_id = integer(),
    window = character(),
    reference_stop_ref = character()
  )
  for (nm in names(cols)) {
    out[, (nm) := cols[[nm]]]
  }
  out[, n_headways := integer()]
  out[]
}

#' Format route-direction groups for compact warning messages
#' @noRd
format_route_direction_groups <- function(groups, max_n = 5L) {
  groups <- utils::head(groups, max_n)
  fmt <- function(x) ifelse(is.na(x), "<NA>", as.character(x))
  paste0(
    "route_ref=", fmt(groups$route_ref),
    ", direction_id=", fmt(groups$direction_id)
  )
}

#' Format route-stop groups for compact warning messages
#' @noRd
format_route_stop_groups <- function(groups, max_n = 5L) {
  groups <- utils::head(groups, max_n)
  fmt <- function(x) ifelse(is.na(x), "<NA>", as.character(x))
  paste0(
    "route_ref=", fmt(groups$route_ref),
    ", stop_ref=", fmt(groups$stop_ref)
  )
}

#' Warn about route-directions with no resolved reference stop
#' @noRd
warn_missing_reference_groups <- function(served, refs) {
  all_groups <- unique(served[
    !is.na(direction_id),
    list(route_ref, direction_id)
  ])
  ref_groups <- unique(refs[, list(route_ref, direction_id)])
  missing_groups <- all_groups[
    !ref_groups,
    on = c("route_ref", "direction_id")
  ]
  if (nrow(missing_groups) == 0L) {
    return(invisible(NULL))
  }
  shown <- format_route_direction_groups(missing_groups)
  n_more <- nrow(missing_groups) - length(shown)
  suffix <- if (n_more > 0L) paste0("; ", n_more, " more") else ""
  warning(
    "No reference stop resolved for ",
    nrow(missing_groups),
    " route-direction group(s); they will not produce passage headways: ",
    paste(shown, collapse = "; "),
    suffix,
    ".",
    call. = FALSE
  )
  invisible(NULL)
}

#' Pick one direction-unique reference stop for each route-direction
#' @noRd
resolve_reference_stops <- function(served, reference_stops = NULL) {
  stop_dirs <- unique(served[, list(route_ref, stop_ref, direction_id)])
  known_stop_dirs <- stop_dirs[!is.na(direction_id)]
  shared <- known_stop_dirs[
    ,
    list(n_directions = data.table::uniqueN(direction_id)),
    by = list(route_ref, stop_ref)
  ][n_directions > 1L]

  if (!is.null(reference_stops)) {
    if (
      !is.character(reference_stops) ||
        length(reference_stops) == 0L ||
        anyNA(reference_stops) ||
        any(!nzchar(trimws(reference_stops)))
    ) {
      stop(
        "'reference_stops' must be NULL or a non-empty character vector ",
        "without missing or empty values.",
        call. = FALSE
      )
    }
    reference_stops <- unique(as.character(reference_stops))
    shared_ref <- shared[stop_ref %in% reference_stops]
    if (nrow(shared_ref) > 0L) {
      stop(
        "Reference stop(s) must be direction-unique within each route; ",
        "stop(s) observed in more than one known direction: ",
        paste(unique(shared_ref$stop_ref), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    candidates <- served[stop_ref %in% reference_stops]
    if (nrow(candidates) == 0L) {
      stop(
        "None of 'reference_stops' appears in served observed events.",
        call. = FALSE
      )
    }
  } else {
    candidates <- merge(
      served,
      shared[, list(route_ref, stop_ref, shared_stop = TRUE)],
      by = c("route_ref", "stop_ref"),
      all.x = TRUE,
      sort = FALSE
    )
    candidates <- candidates[is.na(shared_stop)]
    candidates[, shared_stop := NULL]
    if (nrow(candidates) == 0L) {
      stop(
        "No direction-unique reference stop is available; supply events with ",
        "direction-specific stops or choose a direction-unique ",
        "'reference_stops' value.",
        call. = FALSE
      )
    }
  }

  unknown_direction <- unique(candidates[
    is.na(direction_id),
    list(route_ref, stop_ref)
  ])
  if (nrow(unknown_direction) > 0L) {
    shown <- format_route_stop_groups(unknown_direction)
    n_more <- nrow(unknown_direction) - length(shown)
    suffix <- if (n_more > 0L) paste0("; ", n_more, " more") else ""
    warning(
      nrow(unknown_direction),
      " candidate reference stop group(s) include rows with unknown ",
      "'direction_id'; those rows are excluded from passage-headway output ",
      "and do not make the stop shared: ",
      paste(shown, collapse = "; "),
      suffix,
      ".",
      call. = FALSE
    )
    candidates <- candidates[!is.na(direction_id)]
    if (nrow(candidates) == 0L) {
      stop(
        "No direction-unique reference stop is available after excluding ",
        "rows with unknown 'direction_id'; supply events with known ",
        "'direction_id' values or choose a direction-unique ",
        "'reference_stops' value with known direction.",
        call. = FALSE
      )
    }
  }

  # This ranks raw detections. Dwell collapse happens after reference-stop
  # selection, so duplicate fixes can influence this cheap tie-break heuristic.
  refs <- candidates[
    ,
    list(n_events = .N),
    by = list(route_ref, direction_id, stop_ref)
  ]
  data.table::setorder(refs, route_ref, direction_id, -n_events, stop_ref)
  refs <- refs[, .SD[1L], by = list(route_ref, direction_id)]
  refs[, n_events := NULL]
  refs[]
}

#' Observed Travel-Time and Dwell Quantiles per Stop
#'
#' Reduces many observed passages to one representative stop pattern: per
#' \code{(route_ref, direction_id, stop_ref)}, quantiles of travel time from
#' trip start and the median dwell. The stops are ordered by the cross-trip
#' canonical order (\code{\link{rt2s_obs_stop_order}}), giving a monotone
#' representative sequence suitable for a frequency trip's \code{stop_times}.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @param quantiles Named numeric vector of probabilities in \code{[0, 1]};
#'   each becomes an integer-seconds column \code{travel_<name>}. Default
#'   \code{c(p05 = 0.05, p50 = 0.5, p95 = 0.95)} - the free-flow / typical /
#'   reliable triple.
#' @return A data.table ordered by \code{stop_sequence}, with columns
#'   \code{route_ref}, \code{direction_id}, \code{stop_ref}, \code{stop_sequence},
#'   one \code{travel_<name>} column per quantile (integer seconds from trip
#'   start), \code{dwell_median} (integer seconds; \code{0} when no dwell was
#'   observed), and \code{n_obs} (served passages at the stop). Only served
#'   passages are summarised - \code{skipped}/\code{canceled} rows and rows
#'   without a timed arrival are excluded, so a stop served on no run produces
#'   no row (rather than an all-NA one). Travel time is \strong{not} forced
#'   monotone here; the assembler applies the \code{cummax} guard when it
#'   renders clock times.
#' @export
rt2s_obs_travel_times <- function(
  events,
  quantiles = c(p05 = 0.05, p50 = 0.5, p95 = 0.95)
) {
  dt <- add_trip_offsets(rt2s_events_validate(events))
  check_quantiles(quantiles)
  dt[, dwell := as.numeric(departure_time - arrival_time, units = "secs")]

  served <- served_offsets(dt)
  qn <- names(quantiles)
  agg <- served[
    ,
    c(
      stats::setNames(
        lapply(quantiles, function(p) int_quantile(offset, p)),
        paste0("travel_", qn)
      ),
      list(
        dwell_median = {
          d <- dwell[!is.na(dwell)]
          if (length(d) > 0L) as.integer(stats::median(d)) else 0L
        },
        n_obs = .N
      )
    ),
    by = list(route_ref, direction_id, stop_ref)
  ]

  ord <- compute_stop_order(dt)
  agg <- merge(
    agg,
    ord[, list(route_ref, direction_id, stop_ref, stop_sequence)],
    by = c("route_ref", "direction_id", "stop_ref"),
    all.x = TRUE
  )
  data.table::setorder(agg, route_ref, direction_id, stop_sequence)
  agg[]
}
