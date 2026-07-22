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
#' Assigns each time to a named window (e.g. "am_peak"). Used by
#' \code{\link{obs_headways}} to group trips, and stand-alone for any
#' time-of-day bucketing of observed service.
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
#' time_window(
#'   as.POSIXct(c("2026-07-14 07:30:00", "2026-07-14 12:00:00"), tz = "UTC"),
#'   list(am_peak = c("06:00", "09:00"))
#' )
#' # Overnight service attributed to the previous service date:
#' time_window(
#'   as.POSIXct("2026-07-15 00:30:00", tz = "UTC"),
#'   list(overnight = c("22:00", "26:00")),
#'   service_date = as.Date("2026-07-14")
#' )
#' @export
time_window <- function(x, windows = NULL, service_date = NULL, tz = NULL) {
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
obs_stop_order <- function(events) {
  dt <- add_trip_offsets(validate_events(events))
  compute_stop_order(dt)
}

#' Observed Headways per Route-Direction and Time Window
#'
#' Reduces observed trip start times to headway quantiles per
#' \code{(route_ref, direction_id, window)}. A trip's start is its first
#' \strong{served} arrival: \code{skipped}/\code{canceled} rows are ignored, so
#' a canceled trip (which never operated) contributes no start and cannot shrink
#' the observed headway, even if its rows carry stale timestamps. Headways are
#' the gaps between consecutive trip starts \emph{within one service date}.
#' Non-positive gaps and gaps longer than \code{max_headway_secs} are dropped as
#' spurious (vehicles resuming after a layover, data gaps). No weekday/weekend
#' filtering is applied - restrict the \code{events} to the service dates you
#' want summarised before calling.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @param windows Time-window definition passed to \code{\link{time_window}};
#'   \code{NULL} (default) treats each day as one \code{"all"} window.
#' @param quantiles Named numeric vector of probabilities in \code{[0, 1]};
#'   each becomes an integer-seconds column \code{headway_<name>}. Default
#'   \code{c(median = 0.5, p95 = 0.95)}.
#' @param max_headway_secs Upper cutoff; gaps above it are treated as
#'   between-service breaks and excluded. Default 10800 (3 h).
#' @return A data.table with columns \code{route_ref}, \code{direction_id},
#'   \code{window}, one \code{headway_<name>} column per quantile (integer
#'   seconds), and \code{n_headways} (count of headways summarised). Groups with
#'   no usable headway (e.g. a single run) produce no row.
#' @export
obs_headways <- function(
  events,
  windows = NULL,
  quantiles = c(median = 0.5, p95 = 0.95),
  max_headway_secs = 3L * 3600L
) {
  dt <- validate_events(events)
  check_quantiles(quantiles)
  if (
    !is.numeric(max_headway_secs) ||
      length(max_headway_secs) != 1L ||
      is.na(max_headway_secs) ||
      max_headway_secs <= 0
  ) {
    stop("'max_headway_secs' must be a single positive number.", call. = FALSE)
  }
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
    window := time_window(trip_start, windows, service_date = service_date, tz = tz)
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

#' Observed Travel-Time and Dwell Quantiles per Stop
#'
#' Reduces many observed passages to one representative stop pattern: per
#' \code{(route_ref, direction_id, stop_ref)}, quantiles of travel time from
#' trip start and the median dwell. The stops are ordered by the cross-trip
#' canonical order (\code{\link{obs_stop_order}}), giving a monotone
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
obs_travel_times <- function(
  events,
  quantiles = c(p05 = 0.05, p50 = 0.5, p95 = 0.95)
) {
  dt <- add_trip_offsets(validate_events(events))
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
