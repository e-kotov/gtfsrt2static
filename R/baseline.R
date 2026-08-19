# Reductions of a *planned* static feed. Where the summarise module derives a
# representative stop pattern from observations, this module takes the pattern
# the operator published and leaves its shape alone, so a scenario contrast can
# vary service levels without also varying the network. See the
# "Reconstructed versus anchored stop patterns" section of
# ?rt2s_frequencies for when each regime is the right one.

#' Seconds from a GTFS time column, tolerating already-numeric input
#'
#' `gtfsio::import_gtfs()` yields character "HH:MM:SS" (possibly >= 24 h); some
#' in-memory feeds already carry seconds. Unparseable values become NA and are
#' handled by the caller, which excludes those trips.
#' @noRd
gtfs_time_secs <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  as.numeric(hms_to_secs(x))
}

#' Canonical Published Stop Pattern per Route-Direction
#'
#' Reduces a planned static GTFS feed to \strong{one representative stop pattern
#' per \code{(route, direction)}}: the pattern that the most trips actually
#' operate, with its stop-to-stop offsets rebased to a trip start. This is the
#' \emph{anchoring} counterpart to \code{\link{rt2s_obs_travel_times}}, which
#' reconstructs a pattern from observations instead.
#'
#' Use it when an analysis compares scenarios that must share an identical
#' network. Because the stops, their order and their relative offsets all come
#' from the published feed rather than from observations, feeds built for
#' different scenarios differ only in the service levels applied to them - so a
#' scheduled-versus-observed contrast cannot be an artifact of the scenarios
#' having different stop sets. Feed it to
#' \code{rt2s_frequencies(pattern_source = "baseline")}.
#'
#' @section Pattern selection:
#' Each trip is reduced to a \emph{signature}: its \code{stop_id}s in
#' \code{stop_sequence} order. Per \code{(route, direction)} the winning
#' signature is the one carried by the most trips, tie-broken by more stops and
#' then lexicographically, so short turns and via-variants collapse to the one
#' pattern that best represents the route and the choice never depends on row
#' order. The \emph{template trip} is the lowest \code{trip_id} carrying the
#' winning signature; it supplies the offsets and is returned for traceability.
#'
#' @section Offsets:
#' Offsets are rebased on the template trip's \strong{first departure}: the
#' layover before the vehicle starts moving is not travel time.
#' \code{travel_base} is therefore negative at the origin stop (its arrival
#' precedes that departure), which is intentional - the frequency assembler
#' clamps it through \code{\link{rt2s_monotone_offsets}}. \code{stop_sequence} is
#' renumbered densely from 1, since the baseline's own numbering may start
#' anywhere and contain gaps (GTFS requires only that it increase).
#'
#' @param baseline A planned static GTFS feed: a gtfsio/gtfstools-style object
#'   (named list of data.frames) or a path to a GTFS zip. Only \code{trips} and
#'   \code{stop_times} are required, plus \code{routes} when
#'   \code{route_key = "route_short_name"}.
#' @param route_key Which baseline column becomes \code{route_ref}, i.e. the
#'   identity that must match \code{events$route_ref} downstream.
#'   \code{"route_id"} (default) is the spec-canonical identity and what
#'   \code{\link{rt2s_events_from_trip_updates}} writes. \code{"route_short_name"}
#'   suits observations keyed on the public line number; when several
#'   \code{route_id}s share a short name they are collapsed with a warning.
#' @param min_stops Minimum stops for a trip to be a pattern candidate. Default
#'   2 - a one-stop "pattern" cannot describe movement.
#' @return A data.table ordered by \code{route_ref}, \code{direction_id},
#'   \code{stop_sequence}, with columns \code{route_ref}, \code{direction_id},
#'   \code{stop_ref}, \code{stop_sequence} (dense, 1-based),
#'   \code{travel_base} (integer seconds from the template trip's first
#'   departure; negative at the origin), \code{dwell_base} (integer seconds),
#'   \code{template_trip_id}, \code{n_pattern_trips} (baseline trips carrying
#'   the winning signature) and \code{n_stops}.
#'
#'   Trips are excluded from candidacy - with a warning giving the count - when
#'   \code{direction_id} is \code{NA}, or when any \code{arrival_time} /
#'   \code{departure_time} is missing (real feeds leave non-timepoint stops
#'   blank). Excluding before counting means the modal rule simply picks among
#'   fully timed trips; it is an error only if that empties a route-direction.
#' @seealso \code{\link{rt2s_baseline_headways}} for the planned feed's own headways,
#'   \code{\link{rt2s_baseline_service_dates}} for its service days,
#'   \code{\link{rt2s_frequencies}} to assemble feeds from the result.
#' @examples
#' baseline <- list(
#'   trips = data.frame(
#'     trip_id = c("t1", "t2", "t3"),
#'     route_id = "B62",
#'     direction_id = 0L
#'   ),
#'   stop_times = data.frame(
#'     trip_id = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3"),
#'     stop_id = c("S1", "S2", "S3", "S1", "S2", "S3", "S1", "S2"),
#'     stop_sequence = c(1:3, 1:3, 1:2),
#'     arrival_time = c(
#'       "05:59:30", "06:02:00", "06:04:30",
#'       "06:09:30", "06:12:00", "06:14:30",
#'       "06:19:30", "06:22:00"
#'     ),
#'     departure_time = c(
#'       "06:00:00", "06:02:30", "06:05:00",
#'       "06:10:00", "06:12:30", "06:15:00",
#'       "06:20:00", "06:22:30"
#'     )
#'   )
#' )
#' # The 3-stop signature wins (2 trips vs 1); offsets rebase on 06:00:00.
#' rt2s_baseline_patterns(baseline)
#' @export
rt2s_baseline_patterns <- function(
  baseline,
  route_key = c("route_id", "route_short_name"),
  min_stops = 2L
) {
  route_key <- match.arg(route_key)
  min_stops <- check_min_stops(min_stops)
  baseline <- read_gtfs_input(baseline)
  trips <- baseline_trips(baseline, route_key)
  st <- baseline_stop_times(baseline, trips$trip_id)

  # Which route-directions the feed claims to serve, before any exclusion. Used
  # only to tell "this feed has no such route" from "we dropped all its trips".
  claimed <- unique(trips[
    !is.na(route_ref) & !is.na(direction_id),
    list(route_ref, direction_id)
  ])

  na_trips <- unique(st[is.na(arr_s) | is.na(dep_s), trip_id])
  if (length(na_trips) > 0L) {
    warning(
      length(na_trips),
      " baseline trip(s) have a missing arrival or departure time and were ",
      "excluded from pattern candidacy (non-timepoint stops are commonly ",
      "blank).",
      call. = FALSE
    )
    st <- st[!trip_id %in% na_trips]
  }

  sig <- st[,
    list(signature = paste(stop_id, collapse = ">"), n_stops = .N),
    by = trip_id
  ]
  sig <- merge(
    sig,
    trips[, list(trip_id, route_ref, direction_id)],
    by = "trip_id"
  )
  sig <- sig[!is.na(route_ref) & !is.na(direction_id) & n_stops >= min_stops]
  if (nrow(sig) == 0L) {
    stop(
      "No baseline trip is a usable pattern candidate (needs a non-NA ",
      "route/direction, at least ",
      min_stops,
      " stops, and complete arrival/departure times). If your feed leaves ",
      "non-timepoint times blank, interpolate them first (e.g. ",
      "gtfstools::interpolate_stop_times()).",
      call. = FALSE
    )
  }

  # Modal signature: most trips, then more stops, then lexicographic. Every
  # level is a total order over the candidates, so the pick is deterministic
  # and independent of input row order.
  counts <- sig[,
    list(n_pattern_trips = .N, n_stops = n_stops[1L]),
    by = list(route_ref, direction_id, signature)
  ]
  data.table::setorder(
    counts,
    route_ref,
    direction_id,
    -n_pattern_trips,
    -n_stops,
    signature
  )
  chosen <- counts[, .SD[1L], by = list(route_ref, direction_id)]

  lost <- baseline_lost_groups(claimed, chosen)
  if (nrow(lost) > 0L) {
    stop(
      nrow(lost),
      " baseline route-direction(s) have trips but no usable pattern ",
      "candidate, e.g. ",
      baseline_key_examples(lost),
      ". This usually means every one of their trips has a missing ",
      "arrival/departure time; interpolate them first (e.g. ",
      "gtfstools::interpolate_stop_times()).",
      call. = FALSE
    )
  }

  tmpl <- merge(
    sig,
    chosen[, list(route_ref, direction_id, signature, n_pattern_trips)],
    by = c("route_ref", "direction_id", "signature")
  )
  data.table::setorder(tmpl, route_ref, direction_id, trip_id)
  tmpl <- tmpl[, .SD[1L], by = list(route_ref, direction_id)]

  pat <- merge(
    st,
    tmpl[, list(trip_id, route_ref, direction_id, n_pattern_trips, n_stops)],
    by = "trip_id"
  )
  data.table::setorder(pat, route_ref, direction_id, stop_sequence)
  out <- pat[,
    {
      base <- min(dep_s)
      list(
        stop_ref = stop_id,
        stop_sequence = seq_len(.N),
        travel_base = as.integer(round(arr_s - base)),
        dwell_base = as.integer(round(dep_s - arr_s)),
        template_trip_id = trip_id,
        n_pattern_trips = n_pattern_trips,
        n_stops = n_stops
      )
    },
    by = list(route_ref, direction_id)
  ]
  data.table::setorder(out, route_ref, direction_id, stop_sequence)
  out[]
}

#' Planned Headways per Route-Direction and Time Window
#'
#' Reduces a planned static feed's own trip start times to one headway per
#' \code{(route, direction, window)}: the gaps between consecutive scheduled
#' departures, summarised. This is the planned counterpart to
#' \code{\link{rt2s_obs_headways}}, and it is what makes a "scheduled" scenario
#' expressible - a feed that keeps the published running times \emph{and} the
#' published frequency, to contrast against observed ones.
#'
#' Feed the result to \code{rt2s_frequencies(headways=)} after tagging it
#' with the scenario it describes:
#' \preformatted{sh <- rt2s_baseline_headways(static, windows)
#' sh$scenario <- "scheduled"}
#'
#' @section Difference from \code{rt2s_obs_headways()}:
#' A static feed has no service dates, only a calendar, so gaps are \emph{not}
#' grouped within a day the way observed trip starts are. Every trip the feed
#' defines for a route-direction contributes to one pool per window. Restrict
#' the \code{baseline} to the service pattern you mean (e.g. weekday trips)
#' before calling if that distinction matters.
#'
#' @param baseline A planned static GTFS feed: a gtfsio/gtfstools-style object or
#'   a path to a GTFS zip. Requires \code{trips} and \code{stop_times}.
#' @param windows Named list of \code{c(start, end)} time strings, as in
#'   \code{\link{rt2s_frequencies}}; passed to \code{\link{rt2s_time_window}}, so
#'   overnight windows such as \code{c("22:00", "26:00")} work. The name
#'   \code{"other"} is reserved for unassigned times and is rejected. Trips whose
#'   first departure falls in no window are excluded.
#' @param route_key Which baseline column becomes \code{route_ref}; see
#'   \code{\link{rt2s_baseline_patterns}}.
#' @param statistic How to summarise the gaps: \code{"median"} (default) or
#'   \code{"mean"}. Both are rounded to whole seconds.
#' @param max_headway_secs Gaps above this are treated as between-service breaks
#'   and excluded. Default 10800 (3 h).
#' @return A data.table with columns \code{route_ref}, \code{direction_id},
#'   \code{window}, \code{headway_secs} (integer) and \code{n_sched_trips} (the
#'   gaps summarised). Groups with fewer than two departures in a window yield no
#'   row, since a single departure defines no headway.
#' @seealso \code{\link{rt2s_baseline_patterns}},
#'   \code{\link{rt2s_baseline_service_dates}}, \code{\link{rt2s_frequencies}}
#' @examples
#' baseline <- list(
#'   trips = data.frame(
#'     trip_id = c("t1", "t2", "t3"),
#'     route_id = "B62",
#'     direction_id = 0L
#'   ),
#'   stop_times = data.frame(
#'     trip_id = rep(c("t1", "t2", "t3"), each = 2),
#'     stop_id = rep(c("S1", "S2"), 3),
#'     stop_sequence = rep(1:2, 3),
#'     arrival_time = c(
#'       "06:00:00", "06:05:00",
#'       "06:10:00", "06:15:00",
#'       "06:25:00", "06:30:00"
#'     ),
#'     departure_time = c(
#'       "06:00:00", "06:05:00",
#'       "06:10:00", "06:15:00",
#'       "06:25:00", "06:30:00"
#'     )
#'   )
#' )
#' # departures 06:00 / 06:10 / 06:25 -> gaps 600, 900 -> median 750
#' rt2s_baseline_headways(baseline, windows = list(am = c("06:00", "09:00")))
#' @export
rt2s_baseline_headways <- function(
  baseline,
  windows,
  route_key = c("route_id", "route_short_name"),
  statistic = c("median", "mean"),
  max_headway_secs = 3L * 3600L
) {
  route_key <- match.arg(route_key)
  statistic <- match.arg(statistic)
  max_headway_secs <- check_positive_seconds(
    max_headway_secs,
    "max_headway_secs"
  )
  if (
    missing(windows) ||
      !is.list(windows) ||
      length(windows) == 0L ||
      is.null(names(windows)) ||
      any(!nzchar(names(windows)))
  ) {
    stop(
      "'windows' must be a non-empty named list of c(start, end) time strings.",
      call. = FALSE
    )
  }
  check_reserved_window_name(windows)
  for (nm in names(windows)) {
    w <- windows[[nm]]
    if (length(w) == 2L) {
      s <- hms_to_secs(w[1]); e <- hms_to_secs(w[2])
      if (!is.na(s) && !is.na(e) && e <= s) {
        stop("Window '", nm, "' end time (", w[2], ") must be strictly after start time (", w[1], ").", call. = FALSE)
      }
    }
  }
  baseline <- read_gtfs_input(baseline)
  trips <- baseline_trips(baseline, route_key)
  st <- baseline_stop_times(baseline, trips$trip_id)

  # A trip starts when it first departs, which is also what rt2s_obs_headways() uses
  # for observed runs, so the two are comparable.
  starts <- st[!is.na(dep_s), list(start_s = min(dep_s)), by = trip_id]
  starts <- merge(
    starts,
    trips[, list(trip_id, route_ref, direction_id)],
    by = "trip_id"
  )
  starts <- starts[!is.na(route_ref) & !is.na(direction_id)]
  if (nrow(starts) == 0L) {
    stop(
      "No baseline trip has a usable first departure and route/direction.",
      call. = FALSE
    )
  }
  # Numeric seconds go through rt2s_time_window()'s pass-through branch, so a
  # planned departure at "25:10:00" lands in an overnight window correctly.
  starts[, window := rt2s_time_window(start_s, windows = windows)]
  starts <- starts[window != "other"]
  if (nrow(starts) == 0L) {
    stop(
      "No baseline trip departs inside any of the given 'windows'.",
      call. = FALSE
    )
  }

  data.table::setorder(starts, route_ref, direction_id, window, start_s)
  gaps <- starts[,
    {
      d <- diff(start_s)
      d <- d[d > 0 & d <= max_headway_secs]
      if (length(d) == 0L) {
        list(headway_secs = NA_integer_, n_sched_trips = 0L)
      } else {
        v <- if (identical(statistic, "median")) {
          stats::median(d)
        } else {
          mean(d)
        }
        list(
          headway_secs = as.integer(round(v)),
          n_sched_trips = length(d)
        )
      }
    },
    by = list(route_ref, direction_id, window)
  ]
  gaps <- gaps[!is.na(headway_secs)]
  data.table::setorder(gaps, route_ref, direction_id, window)
  gaps[]
}

# --- scaling, headway overrides, and inherited feed tables -------------------

#' Validate the per-headway-group running-time ratio table
#'
#' Ratios must be strictly positive: a ratio of 0 collapses an entire trip into
#' one instant, which is structurally valid GTFS and completely wrong, so it is
#' rejected rather than clamped. Bounds policy (e.g. refusing implausible
#' ratios) belongs to the caller that estimated them.
#' @noRd
check_scaling <- function(scaling, scen, windows) {
  if (!is.data.frame(scaling)) {
    stop(
      "'scaling' must be a data.frame with columns route_ref, direction_id, ",
      "window, scenario, ratio.",
      call. = FALSE
    )
  }
  scaling <- data.table::as.data.table(scaling)
  validate_required_columns(
    scaling,
    c("route_ref", "direction_id", "window", "scenario", "ratio"),
    "'scaling'"
  )
  if (nrow(scaling) == 0L) {
    stop(
      "'scaling' has no rows, so no scenario has a running-time ratio.",
      call. = FALSE
    )
  }
  out <- scaling[, list(
    route_ref = as.character(route_ref),
    direction_id = suppressWarnings(as.integer(direction_id)),
    window = as.character(window),
    scenario = as.character(scenario),
    ratio = suppressWarnings(as.numeric(ratio))
  )]
  bad_scen <- setdiff(unique(out$scenario), scen)
  if (length(bad_scen) > 0L) {
    stop(
      "'scaling' names scenario(s) that 'quantiles' does not define: '",
      paste(bad_scen, collapse = "', '"),
      "'. 'quantiles' is the single source of scenario identity.",
      call. = FALSE
    )
  }
  bad_win <- setdiff(unique(out$window), names(windows))
  if (length(bad_win) > 0L) {
    stop(
      "'scaling' names window(s) that 'windows' does not define: '",
      paste(bad_win, collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  if (anyNA(out$ratio) || any(!is.finite(out$ratio)) || any(out$ratio <= 0)) {
    stop(
      "'scaling$ratio' must be finite and strictly greater than 0.",
      call. = FALSE
    )
  }
  key <- c("route_ref", "direction_id", "window", "scenario")
  if (anyDuplicated(out, by = key) > 0L) {
    stop(
      "'scaling' has duplicate (route_ref, direction_id, window, scenario) ",
      "rows, so the ratio for those headway groups is ambiguous.",
      call. = FALSE
    )
  }
  out
}

#' Resolve one ratio per (trip, scenario) over the whole grid
#'
#' Called before the scenario loop so an incomplete headway group can be removed from
#' *every* scenario. Dropping it from only the scenario that lacks a ratio would
#' give the emitted feeds different trip sets, reacquiring exactly the
#' different-networks confound that anchoring exists to remove.
#'
#' @return A list with \code{ratios} (trip_id / scenario / ratio, kept pairs
#'   only) and \code{dropped} (trip_ids removed under
#'   \code{scaling_missing = "drop"}). The drops are returned rather than only
#'   warned about because the resolved grid has to account for them: a caller
#'   reconciling a drop funnel needs the drops as much as the keeps.
#' @noRd
resolve_trip_ratios <- function(grp, scaling, scen, windows, on_missing) {
  scaling <- check_scaling(scaling, scen, windows)
  want <- data.table::CJ(trip_id = unique(grp$trip_id), scenario = scen, unique = TRUE)
  want <- merge(
    want,
    grp[, list(trip_id, route_ref, direction_id, window)],
    by = "trip_id"
  )
  got <- merge(
    want,
    scaling,
    by = c("route_ref", "direction_id", "window", "scenario"),
    all.x = TRUE
  )
  drop_trips <- character()
  missing_pairs <- got[is.na(ratio)]
  if (nrow(missing_pairs) > 0L) {
    examples <- utils::head(missing_pairs, 5L)
    detail <- paste(
      sprintf(
        "(%s, %s, %s, %s)",
        examples$route_ref,
        examples$direction_id,
        examples$window,
        examples$scenario
      ),
      collapse = ", "
    )
    if (identical(on_missing, "error")) {
      stop(
        "'scaling' has no ratio for ",
        nrow(missing_pairs),
        " headway group/scenario pair(s), e.g. ",
        detail,
        ". Every scenario needs a ratio for every emitted trip so all feeds ",
        "share one trip set; pass scaling_missing = \"drop\" to drop those ",
        "trips from all scenarios instead.",
        call. = FALSE
      )
    }
    drop_trips <- as.character(unique(missing_pairs$trip_id))
    warning(
      nrow(missing_pairs),
      " headway group/scenario pair(s) have no ratio, e.g. ",
      detail,
      "; their ",
      length(drop_trips),
      " trip(s) were dropped from every scenario to keep one shared trip set.",
      call. = FALSE
    )
    got <- got[!trip_id %in% drop_trips]
  }
  list(ratios = got[, list(trip_id, scenario, ratio)], dropped = drop_trips)
}

#' Validate the optional per-headway-group headway override table
#'
#' An override supersedes the quantile-derived headway for exactly the headway
#' groups it lists; unlisted groups keep their observed value. This is what expresses a
#' scenario whose service level does not come from observations at all - a
#' planned "scheduled" feed - without a second way to name scenarios.
#' @noRd
check_headway_overrides <- function(headways, grp, scen) {
  if (is.null(headways)) {
    return(NULL)
  }
  if (!is.data.frame(headways)) {
    stop(
      "'headways' must be a data.frame with columns route_ref, direction_id, ",
      "window, scenario, headway_secs.",
      call. = FALSE
    )
  }
  headways <- data.table::as.data.table(headways)
  validate_required_columns(
    headways,
    c("route_ref", "direction_id", "window", "scenario", "headway_secs"),
    "'headways'"
  )
  out <- headways[, list(
    route_ref = as.character(route_ref),
    direction_id = suppressWarnings(as.integer(direction_id)),
    window = as.character(window),
    scenario = as.character(scenario),
    headway_secs = suppressWarnings(as.integer(headway_secs))
  )]
  bad_scen <- setdiff(unique(out$scenario), scen)
  if (length(bad_scen) > 0L) {
    stop(
      "'headways' names scenario(s) that 'quantiles' does not define: '",
      paste(bad_scen, collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  if (anyNA(out$headway_secs) || any(out$headway_secs <= 0L)) {
    stop(
      "'headways$headway_secs' must be a positive whole number of seconds.",
      call. = FALSE
    )
  }
  key <- c("route_ref", "direction_id", "window", "scenario")
  if (anyDuplicated(out, by = key) > 0L) {
    stop(
      "'headways' has duplicate (route_ref, direction_id, window, scenario) ",
      "rows.",
      call. = FALSE
    )
  }
  # Rows for headway groups that are not emitted are ignored rather than honoured:
  # inventing a trip for them would break the shared trip set.
  known <- grp[, list(route_ref, direction_id, window, trip_id)]
  matched <- merge(
    out,
    known,
    by = c("route_ref", "direction_id", "window"),
    all.x = TRUE
  )
  unknown <- matched[is.na(trip_id)]
  if (nrow(unknown) > 0L) {
    warning(
      nrow(unknown),
      " 'headways' row(s) name a headway group that is not emitted and were ",
      "ignored.",
      call. = FALSE
    )
  }
  matched[!is.na(trip_id), list(trip_id, scenario, headway_secs)]
}

#' Validate the caller-supplied candidate headway-group table
#'
#' Candidacy is a property of the group, not of the scenario, so this table is
#' keyed on the three-key `scaling`/`headways` share minus `scenario`. Mirrors
#' check_scaling(): coerce, check the windows, reject zero rows and duplicates.
#' @noRd
check_headway_groups <- function(headway_groups, windows) {
  if (!is.data.frame(headway_groups)) {
    stop(
      "'headway_groups' must be a data.frame with columns route_ref, ",
      "direction_id, window.",
      call. = FALSE
    )
  }
  headway_groups <- data.table::as.data.table(headway_groups)
  validate_required_columns(
    headway_groups,
    c("route_ref", "direction_id", "window"),
    "'headway_groups'"
  )
  if (nrow(headway_groups) == 0L) {
    stop(
      "'headway_groups' has no rows, so it names no candidate headway group.",
      call. = FALSE
    )
  }
  out <- headway_groups[, list(
    route_ref = as.character(route_ref),
    direction_id = suppressWarnings(as.integer(direction_id)),
    window = as.character(window)
  )]
  bad_win <- setdiff(unique(out$window), names(windows))
  if (length(bad_win) > 0L) {
    stop(
      "'headway_groups' names window(s) that 'windows' does not define: '",
      paste(bad_win, collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  key <- c("route_ref", "direction_id", "window")
  if (anyDuplicated(out, by = key) > 0L) {
    stop(
      "'headway_groups' has duplicate (route_ref, direction_id, window) rows.",
      call. = FALSE
    )
  }
  out
}

#' Resolve one headway per (trip, scenario) over the whole grid
#'
#' Called before the scenario loop for the same reason resolve_trip_ratios() is:
#' a headway group that cannot be served in one scenario has to leave *every*
#' scenario, which cannot be decided from inside build_scenario().
#'
#' A group with no resolvable headway is a drop, not a trip written without a
#' `frequencies.txt` row. Per GTFS a trip absent from `frequencies.txt` is read
#' as exact-time, so emitting one whose `stop_times` are offsets from
#' `00:00:00` would advertise a phantom midnight departure. Service that cannot
#' be expressed as a repeating headway belongs in `extra_trips`.
#'
#' @param grp The surviving headway-group grid, carrying one `headway_<scenario>`
#'   column per scenario.
#' @param headways The checked override table (trip_id / scenario /
#'   headway_secs) from check_headway_overrides(), or NULL.
#' @return A list with \code{headways} (trip_id / scenario / headway_secs /
#'   headway_source, kept pairs only) and \code{dropped} (trip_ids removed from
#'   every scenario). The drops are returned rather than only warned about
#'   because the resolved grid has to account for them.
#' @noRd
resolve_trip_headways <- function(grp, headways, scen) {
  base <- data.table::rbindlist(lapply(scen, function(s) {
    col <- paste0("headway_", s)
    grp[, list(
      trip_id = as.character(trip_id),
      route_ref,
      direction_id,
      window,
      scenario = s,
      headway_secs = suppressWarnings(as.integer(get(col))),
      headway_source = "observed"
    )]
  }))
  base <- unique(base, by = c("trip_id", "scenario"))
  if (!is.null(headways) && nrow(headways) > 0L) {
    ov <- data.table::as.data.table(headways)[, list(
      trip_id = as.character(trip_id),
      scenario = as.character(scenario),
      override = suppressWarnings(as.integer(headway_secs))
    )]
    base[
      ov,
      c("headway_secs", "headway_source") := list(i.override, "override"),
      on = c("trip_id", "scenario")
    ]
  }
  # A non-positive headway is no headway: frequencies.txt requires a positive
  # value, so it fails the same way a missing quantile does.
  base[
    is.na(headway_secs) | headway_secs <= 0L,
    c("headway_secs", "headway_source") := list(NA_integer_, NA_character_)
  ]

  drop_trips <- character()
  missing_pairs <- base[is.na(headway_secs)]
  if (nrow(missing_pairs) > 0L) {
    examples <- utils::head(missing_pairs, 5L)
    detail <- paste(
      sprintf(
        "(%s, %s, %s, %s)",
        examples$route_ref,
        examples$direction_id,
        examples$window,
        examples$scenario
      ),
      collapse = ", "
    )
    drop_trips <- as.character(unique(missing_pairs$trip_id))
    warning(
      nrow(missing_pairs),
      " headway group/scenario pair(s) have no resolvable ",
      "headway, e.g. ",
      detail,
      "; their ",
      length(drop_trips),
      " headway group(s) were dropped from every scenario. Give them a ",
      "headway through 'headways', or carry service that is not a repeating ",
      "headway through 'extra_trips'.",
      call. = FALSE
    )
    base <- base[!trip_id %in% drop_trips]
  }
  list(
    headways = base[, list(trip_id, scenario, headway_secs, headway_source)],
    dropped = drop_trips
  )
}

#' Service Dates of a Named Baseline Service
#'
#' Expands one \code{service_id} of a planned static GTFS feed into the
#' \code{Date} vector it actually runs on, honouring \code{calendar.txt}'s
#' weekday flags and date range \emph{and} \code{calendar_dates.txt}'s
#' exceptions (type 1 adds a date, type 2 removes one). Feeds that define a
#' service through exceptions alone, with no \code{calendar.txt} row, are
#' supported.
#'
#' This is the opt-in way to give \code{\link{rt2s_frequencies}} the baseline's
#' own days through its \code{service_dates} argument. It is deliberately a
#' separate call rather than something the assembler does for you: the assembler
#' never silently inherits \code{baseline$calendar}, because a planned calendar
#' describes planned service while the emitted feed describes the span the
#' caller is asserting. Passing the result explicitly keeps that choice visible
#' at the call site.
#'
#' @param baseline A planned static GTFS feed: a gtfsio/gtfstools-style object
#'   (named list of data.frames) or a path to a GTFS zip. Needs
#'   \code{calendar.txt}, \code{calendar_dates.txt}, or both.
#' @param service_id The single \code{service_id} to expand. A value the feed
#'   does not define is an error listing the ones it does.
#' @return A sorted \code{Date} vector with no duplicates. A service whose
#'   exceptions remove every date it would otherwise run yields a zero-length
#'   vector with a warning.
#' @seealso \code{\link{rt2s_baseline_patterns}},
#'   \code{\link{rt2s_baseline_headways}}, \code{\link{rt2s_frequencies}}
#' @examples
#' baseline <- list(
#'   calendar = data.frame(
#'     service_id = "WD",
#'     monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L, friday = 1L,
#'     saturday = 0L, sunday = 0L,
#'     start_date = 20260302L, end_date = 20260313L
#'   ),
#'   calendar_dates = data.frame(
#'     service_id = "WD",
#'     date = c(20260307L, 20260311L),
#'     exception_type = c(1L, 2L)
#'   )
#' )
#' # ten weekdays, plus the added Saturday, minus the removed Wednesday
#' rt2s_baseline_service_dates(baseline, "WD")
#' @export
rt2s_baseline_service_dates <- function(baseline, service_id) {
  if (
    missing(service_id) ||
      length(service_id) != 1L ||
      is.na(service_id) ||
      !nzchar(as.character(service_id))
  ) {
    stop(
      "'service_id' must be a single non-empty service identifier.",
      call. = FALSE
    )
  }
  # Held in a differently-named local because 'service_id' is also a column of
  # both tables filtered below, where the column would win.
  want_service <- as.character(service_id)
  baseline <- read_gtfs_input(baseline)
  # [[ ]] rather than $: on a plain list `$calendar` partially matches
  # `calendar_dates`, so an exception-only feed would read as a malformed
  # calendar.txt.
  cal <- baseline[["calendar"]]
  cd <- baseline[["calendar_dates"]]
  if (is.null(cal) && is.null(cd)) {
    stop(
      "Baseline feed has neither 'calendar.txt' nor 'calendar_dates.txt', so ",
      "no service can be expanded into dates.",
      call. = FALSE
    )
  }
  if (!is.null(cal)) {
    cal <- data.table::as.data.table(cal)
    validate_required_columns(
      cal,
      c(
        "service_id", "monday", "tuesday", "wednesday", "thursday", "friday",
        "saturday", "sunday", "start_date", "end_date"
      ),
      "baseline calendar"
    )
    cal <- cal[as.character(service_id) == want_service]
  }
  if (!is.null(cd)) {
    cd <- data.table::as.data.table(cd)
    validate_required_columns(
      cd,
      c("service_id", "date", "exception_type"),
      "baseline calendar_dates"
    )
    cd <- cd[as.character(service_id) == want_service]
  }
  known <- unique(c(
    if (!is.null(baseline[["calendar"]])) {
      as.character(data.table::as.data.table(baseline[["calendar"]])$service_id)
    },
    if (!is.null(baseline[["calendar_dates"]])) {
      as.character(
        data.table::as.data.table(baseline[["calendar_dates"]])$service_id
      )
    }
  ))
  if (!want_service %in% known) {
    stop(
      "Baseline feed defines no service_id '",
      want_service,
      "'. Available: ",
      extra_examples(sort(known), 10L),
      ".",
      call. = FALSE
    )
  }
  if (!is.null(cal) && nrow(cal) > 1L) {
    stop(
      "Baseline 'calendar.txt' has ",
      nrow(cal),
      " rows for service_id '",
      want_service,
      "'; it must have at most one.",
      call. = FALSE
    )
  }

  dates <- as.Date(character())
  if (!is.null(cal) && nrow(cal) == 1L) {
    from <- parse_start_date(cal$start_date[[1L]])
    to <- parse_start_date(cal$end_date[[1L]])
    if (is.na(from) || is.na(to)) {
      stop(
        "Baseline 'calendar.txt' row for service_id '",
        want_service,
        "' has an unparseable start_date or end_date (expected YYYYMMDD).",
        call. = FALSE
      )
    }
    if (to >= from) {
      span <- seq(from, to, by = "day")
      flags <- c(
        sunday = 0L, monday = 1L, tuesday = 2L, wednesday = 3L,
        thursday = 4L, friday = 5L, saturday = 6L
      )
      served <- unname(flags[vapply(
        names(flags),
        function(nm) isTRUE(as.integer(cal[[nm]][[1L]]) == 1L),
        logical(1L)
      )])
      dates <- span[as.POSIXlt(span)$wday %in% served]
    }
  }
  if (!is.null(cd) && nrow(cd) > 0L) {
    ex_date <- parse_start_date(cd$date)
    ex_type <- suppressWarnings(as.integer(cd$exception_type))
    if (anyNA(ex_date)) {
      stop(
        sum(is.na(ex_date)),
        " baseline 'calendar_dates.txt' row(s) for service_id '",
        want_service,
        "' have an unparseable 'date' (expected YYYYMMDD).",
        call. = FALSE
      )
    }
    bad_type <- unique(ex_type[!ex_type %in% c(1L, 2L)])
    if (length(bad_type) > 0L) {
      stop(
        "Baseline 'calendar_dates.txt' has exception_type value(s) other than ",
        "1 (added) or 2 (removed): ",
        extra_examples(bad_type),
        ".",
        call. = FALSE
      )
    }
    dates <- c(dates, ex_date[ex_type == 1L])
    dates <- dates[!dates %in% ex_date[ex_type == 2L]]
  }
  # Normalised to double storage: seq.Date() and as.Date() disagree about it, so
  # without this the return type would depend on which branch built the vector.
  dates <- as.Date(sort(unique(as.numeric(dates))), origin = "1970-01-01")
  if (length(dates) == 0L) {
    warning(
      "Baseline service_id '",
      want_service,
      "' resolves to no dates.",
      call. = FALSE
    )
  }
  dates
}

#' Agency metadata inherited from a baseline feed, or NULL
#' @noRd
baseline_agency <- function(baseline) {
  ag <- baseline$agency
  if (is.null(ag) || nrow(ag) == 0L) {
    return(NULL)
  }
  ag <- data.table::as.data.table(ag)
  pick <- function(col) {
    if (!col %in% names(ag)) {
      return(NULL)
    }
    v <- as.character(ag[[col]][1L])
    if (is.na(v) || !nzchar(v)) NULL else v
  }
  out <- list(
    name = pick("agency_name"),
    url = pick("agency_url"),
    timezone = pick("agency_timezone")
  )
  # An all-empty agency table is no better than none: let the usual placeholder
  # and publish-blocker path handle it.
  if (all(vapply(out, is.null, logical(1L)))) NULL else out
}

#' agency_id to use for the emitted feed
#' @noRd
baseline_agency_id <- function(baseline) {
  ag <- baseline$agency
  if (is.null(ag) || nrow(ag) == 0L || !"agency_id" %in% names(ag)) {
    return("AG1")
  }
  v <- as.character(ag$agency_id[1L])
  if (is.na(v) || !nzchar(v)) "AG1" else v
}

#' routes.txt for the emitted feed, inheriting baseline rows where available
#'
#' Inherited rows keep the operator's real `route_type`; only routes with no
#' baseline counterpart are scaffolded. `agency_id` is rewritten to the single
#' emitted agency so referential integrity holds even when the caller overrode
#' the agency metadata.
#' @noRd
baseline_routes_table <- function(
  route_ids,
  baseline_routes,
  feed_agency_id,
  route_type,
  route_type_given
) {
  cols <- c(
    "route_id",
    "agency_id",
    "route_short_name",
    "route_long_name",
    "route_type"
  )
  scaffold <- function(ids) {
    data.table::data.table(
      route_id = ids,
      agency_id = feed_agency_id,
      route_short_name = ids,
      route_long_name = "",
      route_type = as.integer(route_type)
    )
  }
  if (is.null(baseline_routes) || nrow(baseline_routes) == 0L) {
    if (!route_type_given) {
      message("[INFO] route_type not given; scaffolding routes as 3 (bus).")
    }
    return(scaffold(route_ids))
  }
  br <- data.table::as.data.table(baseline_routes)
  validate_required_columns(br, c("route_id", "route_type"), "baseline routes")
  # Build the columns outside the data.table frame: two of them are optional in
  # the baseline, so guarding them inside j would rely on evaluation order.
  br_id <- as.character(br$route_id)
  br <- data.table::data.table(
    route_id = br_id,
    agency_id = feed_agency_id,
    route_short_name = if ("route_short_name" %in% names(br)) {
      as.character(br$route_short_name)
    } else {
      br_id
    },
    route_long_name = if ("route_long_name" %in% names(br)) {
      as.character(br$route_long_name)
    } else {
      ""
    },
    route_type = suppressWarnings(as.integer(br$route_type))
  )
  br <- unique(br, by = "route_id")
  br <- br[route_id %in% route_ids]
  br[is.na(route_short_name), route_short_name := route_id]
  br[is.na(route_long_name), route_long_name := ""]
  missing_ids <- setdiff(route_ids, br$route_id)
  if (length(missing_ids) > 0L) {
    warning(
      length(missing_ids),
      " emitted route(s) have no baseline routes.txt row and were scaffolded ",
      "as route_type ",
      as.integer(route_type),
      ", e.g. '",
      paste(utils::head(missing_ids, 3L), collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  out <- data.table::rbindlist(
    list(br[, cols, with = FALSE], scaffold(missing_ids)),
    use.names = TRUE
  )
  data.table::setorderv(out, "route_id")
  out[]
}

#' Route-direction keys present in the headway grid but not in the pattern
#' @noRd
check_baseline_identity <- function(grp, patt_key, route_key) {
  obs <- unique(grp[, list(route_ref, direction_id)])
  shared <- intersect(
    paste(obs$route_ref, obs$direction_id),
    paste(patt_key$route_ref, patt_key$direction_id)
  )
  if (length(shared) > 0L) {
    return(invisible(NULL))
  }
  stop(
    "No (route, direction) key is shared by the candidate headway groups and ",
    "'baseline', so no trip could be built. Candidate keys include ",
    baseline_key_examples(obs),
    "; baseline keys include ",
    baseline_key_examples(patt_key),
    ". The route_ref of 'events'/'headway_groups' must carry the same ",
    "identifier as the baseline's ",
    if (identical(route_key, "route_id")) {
      "trips.route_id"
    } else {
      "routes.route_short_name"
    },
    " (see route_key=), and direction_id must agree.",
    call. = FALSE
  )
}

#' Validate 'min_stops'
#' @noRd
check_min_stops <- function(min_stops) {
  if (
    !is.numeric(min_stops) ||
      length(min_stops) != 1L ||
      is.na(min_stops) ||
      min_stops < 2L
  ) {
    stop("'min_stops' must be a single number >= 2.", call. = FALSE)
  }
  as.integer(min_stops)
}

#' Route-direction keys claimed by baseline trips but left without a pattern
#' @noRd
baseline_lost_groups <- function(claimed, chosen) {
  have <- paste(chosen$route_ref, chosen$direction_id)
  claimed[!paste(route_ref, direction_id) %in% have]
}

#' First few (route, direction) keys as a readable string
#' @noRd
baseline_key_examples <- function(keys, n = 3L) {
  n <- min(n, nrow(keys))
  paste(
    sprintf(
      "(%s, %s)",
      keys$route_ref[seq_len(n)],
      keys$direction_id[seq_len(n)]
    ),
    collapse = ", "
  )
}

#' Baseline trips reduced to trip_id / route_ref / direction_id
#'
#' `direction_id` is read, never inferred: a frequency feed emits one trip per
#' route-direction-window, so merging the two directions of a route would be a
#' silent modelling error. Parsing it out of a trip-id suffix is an
#' operator-specific convention and stays with the caller.
#' @noRd
baseline_trips <- function(baseline, route_key) {
  for (tbl in c("trips", "stop_times")) {
    if (is.null(baseline[[tbl]])) {
      stop(
        "Baseline feed is missing required file '",
        tbl,
        ".txt'.",
        call. = FALSE
      )
    }
  }
  trips <- data.table::as.data.table(baseline$trips)
  validate_required_columns(trips, c("trip_id", "route_id"), "baseline trips")
  if (!"direction_id" %in% names(trips)) {
    stop(
      "Baseline 'trips.txt' has no 'direction_id' column; a frequency feed ",
      "emits one representative trip per (route, direction, window) and ",
      "cannot merge the two directions of a route. Add 'direction_id' to the ",
      "baseline trips table before calling.",
      call. = FALSE
    )
  }
  dir_in <- trips$direction_id
  trips <- trips[, list(
    trip_id = as.character(trip_id),
    route_id = as.character(route_id),
    direction_id = suppressWarnings(as.integer(direction_id))
  )]
  coerced <- sum(is.na(trips$direction_id) & !is.na(dir_in))
  if (coerced > 0L) {
    warning(
      coerced,
      " baseline trip(s) have a 'direction_id' that is not an integer and ",
      "became NA.",
      call. = FALSE
    )
  }
  n_na_dir <- sum(is.na(trips$direction_id))
  if (n_na_dir > 0L) {
    warning(
      n_na_dir,
      " baseline trip(s) have an NA 'direction_id' and were excluded from ",
      "pattern candidacy.",
      call. = FALSE
    )
  }
  if (anyDuplicated(trips$trip_id) > 0L) {
    stop(
      "Baseline 'trips.txt' has duplicate 'trip_id' values.",
      call. = FALSE
    )
  }
  trips[, route_ref := baseline_route_ref(baseline, route_id, route_key)]
  trips[]
}

#' Resolve route_ref from the chosen baseline route key
#' @noRd
baseline_route_ref <- function(baseline, route_id, route_key) {
  if (identical(route_key, "route_id")) {
    return(route_id)
  }
  if (is.null(baseline$routes)) {
    stop(
      "route_key = \"route_short_name\" needs the baseline 'routes.txt', ",
      "which this feed does not have.",
      call. = FALSE
    )
  }
  routes <- data.table::as.data.table(baseline$routes)
  validate_required_columns(
    routes,
    c("route_id", "route_short_name"),
    "baseline routes"
  )
  map <- unique(routes[, list(
    route_id = as.character(route_id),
    route_ref = as.character(route_short_name)
  )])
  collapsed <- map[, list(n = .N), by = route_ref][n > 1L]
  if (nrow(collapsed) > 0L) {
    warning(
      nrow(collapsed),
      " route_short_name(s) map to several route_id(s) and were collapsed ",
      "into one pattern each, e.g. '",
      paste(utils::head(collapsed$route_ref, 3L), collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  map$route_ref[match(route_id, map$route_id)]
}

#' Baseline stop_times reduced to the trips of interest, with second offsets
#' @noRd
baseline_stop_times <- function(baseline, keep_trip_ids) {
  st <- data.table::as.data.table(baseline$stop_times)
  validate_required_columns(
    st,
    c("trip_id", "stop_id", "stop_sequence", "arrival_time", "departure_time"),
    "baseline stop_times"
  )
  st <- st[, list(
    trip_id = as.character(trip_id),
    stop_id = as.character(stop_id),
    stop_sequence = suppressWarnings(as.integer(stop_sequence)),
    arr_s = gtfs_time_secs(arrival_time),
    dep_s = gtfs_time_secs(departure_time)
  )]
  st <- st[trip_id %in% keep_trip_ids]
  if (nrow(st) == 0L) {
    stop(
      "Baseline 'stop_times.txt' has no rows for any baseline trip.",
      call. = FALSE
    )
  }
  if (anyNA(st$stop_sequence)) {
    stop(
      sum(is.na(st$stop_sequence)),
      " baseline stop_times row(s) have a missing or non-integer ",
      "'stop_sequence'; it is spec-required and orders the pattern.",
      call. = FALSE
    )
  }
  data.table::setorder(st, trip_id, stop_sequence)
  dup <- st[,
    list(bad = anyDuplicated(stop_sequence) > 0L),
    by = trip_id
  ][bad == TRUE]
  if (nrow(dup) > 0L) {
    stop(
      nrow(dup),
      " baseline trip(s) have a repeated 'stop_sequence', e.g. '",
      paste(utils::head(dup$trip_id, 3L), collapse = "', '"),
      "'. GTFS requires it to increase along a trip.",
      call. = FALSE
    )
  }
  neg <- st[!is.na(arr_s) & !is.na(dep_s) & dep_s < arr_s]
  if (nrow(neg) > 0L) {
    stop(
      nrow(neg),
      " baseline stop_times row(s) depart before they arrive, e.g. trip '",
      neg$trip_id[1L],
      "' at stop '",
      neg$stop_id[1L],
      "'.",
      call. = FALSE
    )
  }
  st[]
}
