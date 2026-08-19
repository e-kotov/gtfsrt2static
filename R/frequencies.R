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
#' rt2s_monotone_offsets(
#'   travel = c(-2.4, 300.6, 298.2),
#'   dwell = c(0, 30.6, 15)
#' )
#' @export
rt2s_monotone_offsets <- function(travel, dwell) {
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
#' \code{\link{rt2s_scaffold}} builds them.
#'
#' @section Reconstructed versus anchored stop patterns:
#' This function \strong{reconstructs} a representative stop pattern from the
#' observations themselves, via \code{\link{rt2s_obs_travel_times}} and the canonical
#' cross-trip order of \code{\link{rt2s_obs_stop_order}}. That is the right regime
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
#' \code{\link{rt2s_baseline_patterns}} and \code{pattern_source = "baseline"}.
#'
#' @section Where the candidate headway groups come from:
#' A \strong{headway group} is one \code{(route_ref, direction_id, window)} - the
#' object \code{frequencies.txt} describes, and what EN 12896 (Transmodel) calls
#' a headway journey group. One representative trip is emitted per group.
#'
#' By default the candidate groups are derived from \code{events}: a group with
#' no observed runs is not a candidate. Under
#' \code{pattern_source = "baseline"} that is the wrong gate, because the pattern
#' comes from \code{baseline}, the ratio from \code{scaling} and the headway from
#' \code{headways}, so \code{events} contributes nothing to such a group's output.
#' \code{headway_groups} names those groups directly; candidacy then becomes the
#' events-derived groups \strong{union} the supplied ones, and \code{events} may
#' be \code{NULL} entirely.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#'   Restrict them to the service dates that form one service pattern before
#'   calling (e.g. weekdays only) - no day-type filtering is imposed here.
#'
#'   May be \code{NULL} when \code{headway_groups} is supplied, in which case no
#'   headway analytics run at all and every headway must come from
#'   \code{headways}. \code{NULL} without \code{headway_groups} is an error:
#'   there would be no candidate headway group.
#' @param windows Named list of \code{c(start, end)} time strings defining the
#'   frequency windows, passed to \code{\link{rt2s_time_window}} (overnight windows
#'   such as \code{c("22:00", "26:00")} are supported). The window name
#'   \code{"other"} is reserved for unassigned service times. When
#'   \code{strict_within_window = TRUE}, configured windows must not overlap.
#'   Required: a frequency-based feed needs defined windows. Trips outside every
#'   window are not emitted.
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
#'   As in \code{\link{rt2s_scaffold}} - agency metadata, stop coordinates,
#'   route type, feed language/contacts, and the strict publish gate. Missing
#'   agency or stop coordinates are recorded as publish blockers on every
#'   returned feed (or error under \code{strict}).
#' @param service_id Identifier for the single synthesized service; its
#'   \code{calendar.txt} row is active on the weekdays present in the resolved
#'   service dates, over their range. Default \code{"SVC1"}.
#' @param service_dates Optional \code{Date} vector giving the days this feed
#'   describes. It replaces the dates that would otherwise be read from
#'   \code{events$service_date}, and is reduced by the same rule: weekday flags
#'   plus the minimum and maximum date. Required when \code{events} is
#'   \code{NULL}; \code{\link{rt2s_baseline_service_dates}} is the way to inherit
#'   the baseline's own days.
#'
#'   Dates inside \code{[min, max]} whose weekday \emph{is} served but which are
#'   absent from this vector are written to \code{calendar_dates.txt} with
#'   \code{exception_type = 2}, so a span with holes in it is not overstated as
#'   uninterrupted service. A date set with no holes emits no
#'   \code{calendar_dates.txt} at all.
#'
#'   Supplying \code{headway_groups} without \code{service_dates} warns when the
#'   supplied groups widen the feed past what \code{events} covers: the calendar
#'   then still describes only the observed span.
#' @param exact_times \code{frequencies.exact_times}: \code{0} (default,
#'   frequency-based) or \code{1} (schedule-based).
#' @param max_headway_secs Passed to \code{\link{rt2s_obs_headways}}; gaps above
#'   it are treated as between-service breaks. Default 10800 (3 h).
#' @param headway_method How to estimate headways, passed to
#'   \code{\link{rt2s_obs_headways}} as its \code{method}. \code{"trip_start"}
#'   (default) requires one usable \code{trip_ref} per run; \code{"passage"}
#'   measures intervals at one direction-unique reference stop per
#'   route-direction. This changes only the frequency headway; representative
#'   travel-time patterns still come from \code{\link{rt2s_obs_travel_times}} and
#'   need events whose \code{trip_ref} values make stop offsets meaningful.
#'
#'   The argument is deliberately \strong{not} named \code{method} here, even
#'   though that is what \code{\link{rt2s_obs_headways}} calls it. The two live
#'   at different altitudes: inside a 20-argument assembler that also chooses a
#'   \code{pattern_source} and a \code{scaling_missing} policy, a bare
#'   \code{method} does not say \emph{method of what}. It is not an oversight to
#'   be tidied up.
#' @param reference_stops,min_revisit_gap_s Passed to
#'   \code{\link{rt2s_obs_headways}} when \code{headway_method = "passage"}.
#'   Explicit values are ignored with a warning when
#'   \code{headway_method = "trip_start"}.
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
#'   \strong{Identity contract:} the \code{route_ref} of \code{events} and of
#'   \code{headway_groups} must carry the same identifier as the baseline's
#'   \code{trips.route_id} (or \code{routes.route_short_name} under
#'   \code{route_key}), and \code{direction_id} must agree. A completely disjoint
#'   key set is an error; candidate keys with no baseline pattern are dropped
#'   with a warning, and baseline routes that are in no candidate headway group
#'   are reported and skipped rather than given an invented headway.
#' @param pattern_source Where the representative stop pattern comes from.
#'   \code{"observed"} (default) reconstructs it from \code{events} via
#'   \code{\link{rt2s_obs_travel_times}}. \code{"baseline"} anchors on the published
#'   pattern from \code{baseline} (see \code{\link{rt2s_baseline_patterns}}) and
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
#'   emitted headway group/scenario pair. \code{"error"} (default) reports the
#'   offending pairs. \code{"drop"} removes
#'   those trips from \strong{every} scenario, with a warning - never from just
#'   the scenario that lacks a ratio, which would leave the feeds with different
#'   trip sets.
#' @param headways Optional per-group headway override: a data.frame keyed
#'   \code{route_ref}, \code{direction_id}, \code{window}, \code{scenario} with a
#'   positive \code{headway_secs}. It supersedes the quantile-derived headway for
#'   exactly the headway groups it lists; unlisted groups keep their observed
#'   value. This is how a scenario whose frequency does not come from
#'   observations at all - a planned "scheduled" feed, via
#'   \code{\link{rt2s_baseline_headways}} - is expressed without a second way of
#'   naming scenarios. Rows naming a headway group that is not emitted are
#'   ignored with a warning.
#'
#'   A group with \strong{no} resolvable headway in some scenario - no observed
#'   quantile and no override - is dropped from \strong{every} scenario with
#'   \code{drop_reason = "no_within_window_headway"} or \code{"no_headway"},
#'   exactly as \code{scaling_missing = "drop"} behaves and for the same
#'   shared-trip-set reason. Use \code{extra_trips} to carry service that cannot
#'   be written as a repeating headway.
#' @param headway_groups Optional candidate headway groups, supplied rather than
#'   derived: a data.frame keyed \code{route_ref}, \code{direction_id},
#'   \code{window} - the \code{scaling}/\code{headways} key minus
#'   \code{scenario}, because candidacy is a property of the group and not of the
#'   scenario. Valid only with \code{pattern_source = "baseline"}; passing it
#'   under \code{"observed"} is an error, since an observed pattern can only be
#'   reconstructed for a group that has events.
#'
#'   Candidacy becomes the events-derived groups \strong{union} these, so a group
#'   with no observed runs is still emitted when \code{scaling} gives it a ratio
#'   and \code{headways} gives it a headway. A supplied group with no baseline
#'   stop pattern is \emph{not} an error: it warns and appears in
#'   \code{\link{rt2s_resolved_grid}} with
#'   \code{drop_reason = "no_stop_pattern"}, which is what makes a caller's drop
#'   funnel able to see it.
#' @param route_key Which baseline column supplies route identity, passed to
#'   \code{\link{rt2s_baseline_patterns}}. Baseline mode only.
#' @param extra_trips Optional individually-timed trips to add to the emitted
#'   feeds, as a \strong{named list keyed by scenario name} - the same names as
#'   \code{quantiles}, which is the single source of scenario identity, so a
#'   misspelled scenario is an error rather than a silent no-op. Each element is
#'   a \code{list(trips=, stop_times=)}:
#'   \itemize{
#'     \item \code{trips}: required \code{trip_id} and \code{route_id}; optional
#'       \code{direction_id} and \code{service_id}. An absent \code{service_id}
#'       is stamped with this feed's single synthesized service; a different one
#'       is an error.
#'     \item \code{stop_times}: required \code{trip_id}, \code{arrival_time},
#'       \code{departure_time}, \code{stop_id}, \code{stop_sequence}. Times are
#'       \strong{absolute clock strings} (\code{"HH:MM:SS"}, hours >= 24 allowed
#'       for trips running past midnight), \emph{not} offsets from
#'       \code{00:00:00} as the generated frequency trips use, and must be
#'       non-decreasing along the trip.
#'   }
#'   Any other element of the list is ignored, so a builder that also returns
#'   provenance can be passed through unchanged.
#'
#'   These trips get \strong{no \code{frequencies.txt} row}. That is what makes
#'   them exact-time trips: per the GTFS specification only trips listed in
#'   \code{frequencies.txt} are frequency-based, and the rest are read from
#'   \code{stop_times} as scheduled times. A feed may mix the two, so this is a
#'   standard-compliant feed, not a workaround - it is how a headway group that
#'   cannot be expressed as a repeating headway is carried. Since a group with no
#'   resolvable headway is now a drop, this is the \strong{only} way to carry
#'   such service.
#'
#'   Their stops and routes are added to \code{stops.txt} and \code{routes.txt},
#'   but every referenced \code{stop_id} must already be known (from \code{stops}
#'   or an emitted pattern) and every \code{route_id} must be an emitted or
#'   baseline route: a dangling reference is an invalid feed and is rejected
#'   rather than filled in. No extra \code{trip_id} may collide with a generated
#'   \code{route_direction_window} id.
#'
#'   \strong{No cross-scenario invariant is imposed.} A scenario may supply more,
#'   fewer or no extra trips than another, because the exact-time evidence for a
#'   headway group legitimately differs by scenario. The shared-trip-set guarantee below
#'   is scoped to the \emph{generated frequency} trips, which is where
#'   \code{scaling_missing} enforces it.
#'
#'   Extra trips are \strong{not} rows of \code{\link{rt2s_resolved_grid}}: the grid
#'   is one row per candidate \code{(route, direction, window)} headway group and
#'   extra trips are not groups. Reconciling a feed's \code{trips.txt} therefore
#'   means
#'   \code{c(grid[emitted == TRUE]$trip_id, <the ids you supplied>)}; the caller
#'   supplies the extra trips, so it already owns those ids.
#' @param strict_within_window Passed to \code{\link{rt2s_obs_headways}} when
#'   estimating headways from events. Logical; default \code{FALSE}. When \code{TRUE},
#'   configured windows must be pairwise non-overlapping.
#' @return A named list of gtfsio-convention feed objects, one per quantile
#'   (e.g. \code{$structural}, \code{$median}, \code{$reliable}); write each
#'   with \code{gtfsio::export_gtfs()}. Each carries \code{publishable} /
#'   \code{publish_blockers} attributes (see \code{\link{rt2s_publishable}}).
#'
#'   All scenario feeds share one \emph{generated} trip set by construction:
#'   \code{trip_id} is \code{route_direction_window} and is resolved once, before
#'   any scenario is built, so a contrast between two feeds is a contrast in
#'   service levels only. Trips added through \code{extra_trips} are outside that
#'   guarantee by design and may differ per scenario.
#'
#'   The list carries a \code{resolved_grid} attribute: one row per candidate
#'   \code{(route, direction, window)} headway group and scenario, recording the
#'   ratio and headway actually applied and, for groups that never reached the
#'   feed, why. Read it with \code{\link{rt2s_resolved_grid}}. Groups dropped for
#'   want of a stop pattern, a ratio or a headway are present and flagged rather
#'   than absent, so the grid reconciles against a caller's own drop accounting.
#'   With \code{strict_within_window = TRUE}, an observed group with fewer than
#'   two starts inside a window is retained with
#'   \code{drop_reason = "no_within_window_headway"}.
#' @seealso \code{\link{rt2s_resolved_grid}} for the resolved grid,
#'   \code{\link{rt2s_baseline_service_dates}} for \code{service_dates}.
#' @export
rt2s_frequencies <- function(
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
  strict_within_window = FALSE
) {
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
  check_reserved_window_name(windows)
  check_bool(strict_within_window, "strict_within_window")
  if (strict_within_window) {
    check_strict_windows(windows)
  }
  if (is.null(events) && is.null(headway_groups)) {
    stop(
      "'events' is NULL and no 'headway_groups' were supplied, so there is no ",
      "candidate headway group to build a feed from. Pass observed stop ",
      "events, or name the groups explicitly with headway_groups=.",
      call. = FALSE
    )
  }
  if (is.null(events) && strict_within_window) {
    warning(
      "'strict_within_window' has no headway-estimation effect when 'events' is NULL.",
      call. = FALSE
    )
  }
  dt <- if (is.null(events)) NULL else rt2s_events_validate(events)
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
  if (!anchored && !is.null(headway_groups)) {
    stop(
      "'headway_groups' only applies to pattern_source = \"baseline\"; an ",
      "observed pattern can only be reconstructed for groups that have ",
      "events.",
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
  if (!exact_times %in% c(0L, 1L)) {
    stop("'exact_times' must be 0 or 1.", call. = FALSE)
  }
  scen <- q$scenarios
  # Shape-checked here, before any analytics run, so a malformed caller table
  # fails immediately. Its references are checked later, once the generated trip
  # ids and the emitted stop/route sets exist.
  extra <- check_extra_trips(extra_trips, scen, service_id)

  # --- analytics (computed once, all scenarios) -----------------------------
  # Travel and headway probabilities are resolved separately (see
  # resolve_quantiles()) but share their names, so the scenario-keyed column
  # lookups below are unaffected by which spelling the caller used.
  # The two headway paths are called directly rather than through
  # rt2s_obs_headways(): its missing()-based ignored-argument warning cannot see
  # through this frame, and the equivalent warning was already raised above
  # against 'headway_method'.
  # With no events there are no headway analytics to run at all: every headway
  # then has to come from 'headways', and the empty shell below is what the
  # supplied groups are unioned onto.
  hw <- if (is.null(dt)) {
    empty_headway_grid(scen)
  } else if (identical(headway_method, "trip_start")) {
    headways_by_trip_start(
      dt,
      windows = windows,
      quantiles = q$headway,
      max_headway_secs = max_headway_secs,
      strict_within_window = strict_within_window
    )
  } else {
    headways_by_passage(
      dt,
      reference_stops = reference_stops,
      windows = windows,
      quantiles = q$headway,
      min_revisit_gap_s = min_revisit_gap_s,
      max_headway_secs = max_headway_secs,
      strict_within_window = strict_within_window
    )
  }
  hw <- hw[window != "other"]
  no_within_window <- data.table::data.table(
    route_ref = character(), direction_id = integer(), window = character()
  )
  if (strict_within_window && !anchored && !is.null(dt)) {
    attr_groups <- attr(hw, "no_within_window_groups", exact = TRUE)
    if (!is.null(attr_groups) && nrow(attr_groups) > 0L) {
      no_within_window <- data.table::copy(attr_groups)
    }
    data.table::setattr(hw, "no_within_window_groups", NULL)
    if ("no_within_window" %in% names(hw)) hw[, no_within_window := NULL]
  } else {
    if ("no_within_window" %in% names(hw)) hw[, no_within_window := NULL]
    data.table::setattr(hw, "no_within_window_groups", NULL)
  }
  # Candidacy is events-derived groups UNION caller-supplied ones. Under
  # pattern_source = "baseline" the pattern comes from 'baseline', the ratio
  # from 'scaling' and the headway from 'headways', so 'events' contributes
  # nothing to a supplied group's output and must not gate it either.
  n_supplied_new <- 0L
  if (!is.null(headway_groups)) {
    supplied <- check_headway_groups(headway_groups, windows)
    supplied <- supplied[
      !paste(route_ref, direction_id, window) %in%
        hw[, paste(route_ref, direction_id, window)]
    ]
    n_supplied_new <- nrow(supplied)
    if (n_supplied_new > 0L) {
      for (s in scen) {
        supplied[, (paste0("headway_", s)) := NA_integer_]
      }
      supplied[, n_headways := 0L]
      # fill = TRUE because the passage path carries an extra
      # reference_stop_ref column that a supplied group has no value for.
      hw <- data.table::rbindlist(
        list(hw, supplied),
        use.names = TRUE,
        fill = TRUE
      )
      data.table::setorder(hw, route_ref, direction_id, window)
    }
  }
  # The representative pattern comes from one of two sources. In baseline mode
  # rt2s_obs_travel_times() is not called at all: the pattern is the operator's
  # published one and the travel side of 'quantiles' is inert (documented).
  pat <- if (anchored) {
    rt2s_baseline_patterns(baseline, route_key = route_key)
  } else {
    rt2s_obs_travel_times(dt, q$travel)
  }
  if (nrow(hw) == 0L) {
    if (!is.null(headway_groups)) {
      stop(
        "'headway_groups' names no candidate headway group that survived ",
        "validation, so the feed would be empty.",
        call. = FALSE
      )
    }
    if (identical(headway_method, "trip_start")) {
      stop(
        "No (route, direction, window) headway group has a usable trip-start ",
        "headway; check that 'events' cover multiple runs inside the given ",
        "windows.",
        call. = FALSE
      )
    }
    stop(
      "No (route, direction, window) headway group has a usable passage ",
      "headway; check that reference-stop events contain multiple passages ",
      "inside the given windows, that input events preserve repeated visits, ",
      "and that 'min_revisit_gap_s' is below the true headway.",
      call. = FALSE
    )
  }

  # --- representative trips (shared across scenarios) -----------------------
  grp <- data.table::copy(hw)
  if ("no_within_window" %in% names(grp)) grp[, no_within_window := NULL]
  data.table::setattr(grp, "no_within_window_groups", NULL)

  grp[, route_id := ifelse(is.na(route_ref), "R1", as.character(route_ref))]
  grp[, trip_id := paste(
    route_id,
    ifelse(is.na(direction_id), "NA", as.character(direction_id)),
    window,
    sep = "_"
  )]
  # The candidate headway-group set, captured before any drop stage runs. Every
  # later stage removes trips from 'grp'; the resolved grid is reconstructed
  # against this snapshot so a dropped group stays visible with a reason instead
  # of vanishing from the accounting.
  candidate_groups <- unique(grp[, list(route_ref, direction_id, window, trip_id)])
  no_within_window_ids <- character()
  if (nrow(no_within_window) > 0L) {
    no_within_window[, route_id := ifelse(
      is.na(route_ref), "R1", as.character(route_ref)
    )]
    no_within_window[, trip_id := paste(
      route_id,
      ifelse(is.na(direction_id), "NA", as.character(direction_id)),
      window,
      sep = "_"
    )]
    no_within_window_ids <- unique(no_within_window$trip_id)
    missing_no_within <- no_within_window[
      !trip_id %in% candidate_groups$trip_id,
      list(route_ref, direction_id, window, trip_id)
    ]
    candidate_groups <- data.table::rbindlist(
      list(candidate_groups, missing_no_within), use.names = TRUE
    )
  }

  # window bounds as clock strings (normalise "HH:MM" -> "HH:MM:SS")
  win_start <- vapply(windows, function(w) secs_to_clock(hms_to_secs(w[1])), "")
  win_end <- vapply(windows, function(w) secs_to_clock(hms_to_secs(w[2])), "")

  # (route, direction) that have a stop pattern; drop groups lacking one.
  # In observed mode rt2s_obs_headways and rt2s_obs_travel_times share one "served"
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
  dropped_no_pattern <- as.character(unique(dropped$trip_id))
  if (nrow(dropped) > 0L) {
    warning(
      nrow(dropped),
      " candidate headway group(s) had no ",
      if (anchored) "baseline" else "served",
      " stop pattern and were dropped",
      if (anchored) {
        paste0(
          ", e.g. ",
          baseline_key_examples(unique(dropped[, list(route_ref, direction_id)])),
          "; check that the route_ref of 'events'/'headway_groups' uses the ",
          "same identifier as the baseline (see route_key=)"
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
      "No candidate headway group has a ",
      if (anchored) "baseline" else "served",
      " stop pattern, so no trip can be built and the feed would be empty.",
      if (anchored) {
        paste0(
          " Check the route/direction identity shared by ",
          "'events'/'headway_groups' and 'baseline'."
        )
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
      " baseline route-direction(s) are in no candidate headway group in any ",
      "window and are not emitted."
    )
  }

  # Ratios resolved once, over the whole (trip x scenario) grid, *before* the
  # scenario loop. That placement is what enforces the shared trip set: a
  # headway group missing a ratio for one scenario has to leave every scenario,
  # which cannot be decided from inside build_scenario().
  resolved <- if (anchored) {
    resolve_trip_ratios(grp, scaling, scen, windows, scaling_missing)
  } else {
    NULL
  }
  ratios <- resolved$ratios
  dropped_no_ratio <- if (anchored) resolved$dropped else character()
  if (anchored) {
    keep <- unique(ratios$trip_id)
    grp <- grp[trip_id %in% keep]
    if (nrow(grp) == 0L) {
      stop(
        "'scaling' covers none of the candidate headway groups, so the feed ",
        "would be empty.",
        call. = FALSE
      )
    }
  }
  headways <- check_headway_overrides(headways, grp, scen)

  # Headways resolved here for the same reason ratios are: a headway group with
  # no resolvable headway in one scenario has to leave *every* scenario, so the
  # generated trip set stays shared. It is a drop rather than a trip emitted
  # without a frequencies.txt row, because such a trip would be read as
  # exact-time and would advertise a departure at 00:00:00 that never runs.
  resolved_hw <- resolve_trip_headways(grp, headways, scen)
  trip_headways <- resolved_hw$headways
  dropped_no_headway <- resolved_hw$dropped
  if (length(dropped_no_headway) > 0L) {
    grp <- grp[!trip_id %in% dropped_no_headway]
    if (nrow(grp) == 0L) {
      stop(
        "No candidate headway group has a resolvable headway, so the feed ",
        "would be empty. Supply one per (route, direction, window, scenario) ",
        "through 'headways', or carry service that is not a repeating headway ",
        "through 'extra_trips'.",
        call. = FALSE
      )
    }
  }

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
  route_ids <- unique(as.character(grp$route_id))

  # Extra trips are not headway groups, but they are rows of the emitted feed, so
  # they have to widen the two derivations that are *filtered* to the frequency
  # material: build_stops_table() keeps only 'stop_ids', and routes.txt is built
  # from the observed route set. Without this their stops silently vanish from
  # stops.txt and their route reference dangles.
  if (!is.null(extra)) {
    check_extra_trips_refs(
      extra,
      generated_trip_ids = as.character(candidate_groups$trip_id),
      pattern_stop_ids = stop_ids,
      known_stop_ids = if (is.null(stops)) {
        character()
      } else {
        as.character(data.table::as.data.table(stops)$stop_id)
      },
      known_route_ids = unique(c(
        route_ids,
        if (anchored && !is.null(baseline$routes)) {
          as.character(data.table::as.data.table(baseline$routes)$route_id)
        } else {
          character()
        }
      ))
    )
    extra_stops <- unlist(lapply(extra, function(e) e$stop_times$stop_id))
    stop_ids <- sort(unique(c(stop_ids, as.character(extra_stops))))
    # Appended rather than re-sorted: routes.txt row order is the observed route
    # order, and an unused extra_trips argument must not perturb it.
    extra_routes <- unlist(lapply(extra, function(e) e$trips$route_id))
    route_ids <- c(route_ids, setdiff(unique(as.character(extra_routes)), route_ids))
  }

  stops_out <- build_stops_table(stop_ids, stops, emit, strict)
  blockers <- sink$blockers()

  # Inherited routes keep the operator's real route_type; scaffolding them as 3
  # would silently mislabel a tram or metro line.
  routes <- baseline_routes_table(
    route_ids = route_ids,
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

  # One resolved date set drives calendar.txt, calendar_dates.txt and
  # feed_info's span, so the three cannot disagree about what the feed covers.
  dates <- resolve_service_dates(service_dates, dt, n_supplied_new)
  calendar <- build_calendar(dates, service_id)
  calendar_dates <- build_calendar_dates(dates, service_id)

  feed_info <- data.table::data.table(
    feed_publisher_name = ag$name,
    feed_publisher_url = ag$url,
    feed_lang = feed_lang,
    feed_start_date = yyyymmdd(min(dates)),
    feed_end_date = yyyymmdd(max(dates))
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
    hw_s <- trip_headways[scenario == s]

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

    # Looked up rather than merged so frequencies.txt keeps the headway-group
    # row order, which a merge on trip_id would silently re-sort.
    freq <- grp[, list(
      trip_id,
      start_time = win_start[window],
      end_time = win_end[window],
      headway_secs = hw_s$headway_secs[match(
        as.character(trip_id),
        hw_s$trip_id
      )],
      exact_times = as.integer(exact_times)
    )]
    # Every surviving group resolved a positive headway above; this is the
    # backstop that keeps a non-positive value out of frequencies.txt.
    freq <- freq[!is.na(headway_secs) & headway_secs > 0L]

    trips_emitted <- trips_out[trip_id %in% unique(stop_times$trip_id)]

    # Extra trips join trips.txt and stop_times.txt only. They deliberately get
    # no frequencies.txt row - that is what makes them exact-time trips - and
    # they are appended after 'built' is derived below, because they are not
    # headway groups and must not appear in the resolved grid.
    trips_final <- trips_emitted
    stop_times_final <- stop_times
    ex <- extra[[s]]
    if (!is.null(ex)) {
      trips_final <- rbind(trips_final, ex$trips)
      data.table::setorderv(trips_final, c("route_id", "service_id", "trip_id"))
      stop_times_final <- rbind(stop_times_final, ex$stop_times)
      data.table::setorderv(stop_times_final, c("trip_id", "stop_sequence"))
    }

    feed <- list(
      agency = agency_tbl,
      stops = stops_out,
      routes = routes,
      trips = trips_final,
      stop_times = stop_times_final,
      frequencies = freq,
      calendar = calendar,
      feed_info = feed_info
    )
    # Only attached when the resolved dates actually have gaps, so a feed whose
    # span is contiguous emits no calendar_dates.txt at all. as_gtfs_object() is
    # gtfsio::new_gtfs(), so an extra table needs no further plumbing.
    if (nrow(calendar_dates) > 0L) {
      feed$calendar_dates <- calendar_dates
    }
    # What this scenario actually wrote, read back off the emitted tables rather
    # than predicted from the inputs: the grid is only worth reconciling against
    # if it reports the output, not the intent.
    built <- data.table::data.table(
      trip_id = as.character(trips_emitted$trip_id),
      scenario = s
    )
    built <- merge(
      built,
      freq[, list(trip_id = as.character(trip_id), headway_secs)],
      by = "trip_id",
      all.x = TRUE
    )
    built[, headway_source := NA_character_]
    built[
      hw_s,
      headway_source := i.headway_source,
      on = "trip_id"
    ]
    built[is.na(headway_secs), headway_source := NA_character_]
    list(feed = stamp_publishable(as_gtfs_object(feed), blockers), built = built)
  }

  out <- stats::setNames(lapply(scen, build_scenario), scen)
  grid <- resolved_grid(
    groups = candidate_groups,
    scen = scen,
    ratios = ratios,
    built = data.table::rbindlist(lapply(out, `[[`, "built")),
    dropped = list(
      no_stop_pattern = dropped_no_pattern,
      no_ratio = dropped_no_ratio,
      no_headway = dropped_no_headway,
      no_within_window_headway = no_within_window_ids
    )
  )
  out <- lapply(out, `[[`, "feed")
  attr(out, "resolved_grid") <- grid
  out
}

#' Empty headway grid with the canonical columns
#'
#' What headways_by_trip_start() would have returned had it been called. With
#' `events = NULL` there is nothing to summarise, so the supplied headway groups
#' are unioned onto this shell instead of onto a real result.
#' @noRd
empty_headway_grid <- function(scen) {
  out <- data.table::data.table(
    route_ref = character(),
    direction_id = integer(),
    window = character()
  )
  for (s in scen) {
    out[, (paste0("headway_", s)) := integer()]
  }
  out[, n_headways := integer()]
  out[]
}

#' Resolve the service dates the feed describes
#'
#' `service_dates` wins where given; otherwise the observed dates, reduced the
#' same way they always were. With neither there is nothing to write a calendar
#' from, which is an error rather than an invented span.
#' @noRd
resolve_service_dates <- function(service_dates, dt, n_supplied_new) {
  if (!is.null(service_dates)) {
    dates <- suppressWarnings(as.Date(service_dates))
    if (length(dates) == 0L || anyNA(dates)) {
      stop(
        "'service_dates' must be a non-empty vector of dates with no missing ",
        "values.",
        call. = FALSE
      )
    }
    return(sort(unique(dates)))
  }
  if (is.null(dt)) {
    stop(
      "No 'service_dates' were given and 'events' is NULL, so the feed has no ",
      "service span. Pass service_dates=, e.g. from ",
      "rt2s_baseline_service_dates().",
      call. = FALSE
    )
  }
  # The supplied groups may describe service that the observations never saw, so
  # a span derived from the observations can understate what the feed asserts.
  if (n_supplied_new > 0L) {
    warning(
      "'headway_groups' added ",
      n_supplied_new,
      " headway group(s) that 'events' does not cover, but the calendar span ",
      "still comes from 'events'. Pass 'service_dates' to state the span the ",
      "feed actually describes.",
      call. = FALSE
    )
  }
  sort(unique(as.Date(dt$service_date)))
}

#' calendar.txt for the single synthesized service
#' @noRd
build_calendar <- function(dates, service_id) {
  wd <- unique(as.POSIXlt(dates)$wday) # 0 = Sun .. 6 = Sat
  day_flag <- function(target) as.integer(target %in% wd)
  data.table::data.table(
    service_id = service_id,
    monday = day_flag(1),
    tuesday = day_flag(2),
    wednesday = day_flag(3),
    thursday = day_flag(4),
    friday = day_flag(5),
    saturday = day_flag(6),
    sunday = day_flag(0),
    start_date = yyyymmdd(min(dates)),
    end_date = yyyymmdd(max(dates))
  )
}

#' calendar_dates.txt removing the days inside the span that are not served
#'
#' calendar.txt can only say "these weekdays, between these two dates", so a
#' resolved date set with a hole in it - a strike day, a date whose file is
#' missing - would be overstated as service that ran. Each such date gets an
#' `exception_type = 2` row. A contiguous date set produces no rows at all, and
#' the caller then writes no calendar_dates.txt.
#' @noRd
build_calendar_dates <- function(dates, service_id) {
  span <- seq(min(dates), max(dates), by = "day")
  served_wday <- unique(as.POSIXlt(dates)$wday)
  gaps <- span[as.POSIXlt(span)$wday %in% served_wday & !span %in% dates]
  data.table::data.table(
    service_id = if (length(gaps) == 0L) character() else service_id,
    date = yyyymmdd(gaps),
    exception_type = rep(2L, length(gaps))
  )
}

#' Validate the optional per-scenario extra-trip tables (shape and values)
#'
#' Runs once, before the scenario loop, so a malformed table fails before any
#' feed is built. Mirrors check_headway_overrides(): coerce, check the names
#' against the scenario set, check the required columns, reject duplicates, and
#' report offenders by example.
#'
#' Reference resolution (stop_id / route_id / trip_id collision) is *not* done
#' here: it needs the generated trip ids and the emitted stop and route sets,
#' which do not exist yet. See check_extra_trips_refs().
#'
#' @return A named list, one element per scenario that actually contributes
#'   trips, each \code{list(trips=, stop_times=)} with canonical columns and
#'   types. Scenarios that supply nothing are absent from the result: the number
#'   of extra trips per headway group genuinely differs by scenario, so
#'   "supplied for one"
#'   does not imply "supplied for all".
#' @noRd
check_extra_trips <- function(extra_trips, scen, service_id) {
  if (is.null(extra_trips)) {
    return(NULL)
  }
  if (
    !is.list(extra_trips) ||
      is.data.frame(extra_trips) ||
      length(extra_trips) == 0L ||
      is.null(names(extra_trips)) ||
      any(is.na(names(extra_trips))) ||
      any(!nzchar(names(extra_trips)))
  ) {
    stop(
      "'extra_trips' must be a non-empty named list keyed by scenario name, ",
      "each element a list with 'trips' and 'stop_times'.",
      call. = FALSE
    )
  }
  dup <- unique(names(extra_trips)[duplicated(names(extra_trips))])
  if (length(dup) > 0L) {
    stop(
      "'extra_trips' names scenario(s) more than once: '",
      paste(dup, collapse = "', '"),
      "'.",
      call. = FALSE
    )
  }
  # The likeliest mistake is passing one scenario's material directly, whose
  # names then read as scenario names. Say so rather than reporting "trips" and
  # "stop_times" as undefined scenarios.
  if (all(c("trips", "stop_times") %in% names(extra_trips))) {
    stop(
      "'extra_trips' looks like a single list(trips=, stop_times=). It must be ",
      "a named list keyed by scenario name, e.g. list(median = list(trips = , ",
      "stop_times = )).",
      call. = FALSE
    )
  }
  bad_scen <- setdiff(names(extra_trips), scen)
  if (length(bad_scen) > 0L) {
    stop(
      "'extra_trips' names scenario(s) that 'quantiles' does not define: '",
      paste(bad_scen, collapse = "', '"),
      "'. 'quantiles' is the single source of scenario identity.",
      call. = FALSE
    )
  }
  out <- lapply(
    names(extra_trips),
    function(s) check_extra_trips_one(extra_trips[[s]], s, service_id)
  )
  names(out) <- names(extra_trips)
  out <- out[!vapply(out, is.null, logical(1L))]
  if (length(out) == 0L) NULL else out
}

#' Example offenders for an error message, capped
#' @noRd
extra_examples <- function(x, n = 5L) {
  x <- unique(as.character(x))
  paste0(
    "'",
    paste(utils::head(x, n), collapse = "', '"),
    "'",
    if (length(x) > n) paste0(" (and ", length(x) - n, " more)") else ""
  )
}

#' Validate and canonicalise one scenario's extra trips
#' @noRd
check_extra_trips_one <- function(x, s, service_id) {
  where <- paste0("extra_trips[[\"", s, "\"]]")
  if (is.null(x)) {
    return(NULL)
  }
  if (
    !is.list(x) ||
      is.data.frame(x) ||
      !all(c("trips", "stop_times") %in% names(x))
  ) {
    stop(
      "'",
      where,
      "' must be a list with 'trips' and 'stop_times' elements.",
      call. = FALSE
    )
  }
  trips <- x$trips
  st <- x$stop_times
  if (!is.null(trips) && !is.data.frame(trips)) {
    stop("'", where, "$trips' must be a data.frame.", call. = FALSE)
  }
  if (!is.null(st) && !is.data.frame(st)) {
    stop("'", where, "$stop_times' must be a data.frame.", call. = FALSE)
  }
  # A scenario that adds nothing is legitimate - a group expressible as a headway
  # in one scenario need not be in another - so an empty pair is not an error.
  if (is.null(trips) || nrow(trips) == 0L) {
    if (!is.null(st) && nrow(st) > 0L) {
      stop(
        "'",
        where,
        "$stop_times' has rows but '",
        where,
        "$trips' has none, so those stop times reference no trip.",
        call. = FALSE
      )
    }
    return(NULL)
  }
  if (is.null(st) || nrow(st) == 0L) {
    stop(
      "'",
      where,
      "$trips' has rows but '",
      where,
      "$stop_times' has none; a trip with no stop times is not a valid feed.",
      call. = FALSE
    )
  }

  trips <- data.table::as.data.table(trips)
  st <- data.table::as.data.table(st)
  validate_required_columns(
    trips,
    c("trip_id", "route_id"),
    paste0("'", where, "$trips'")
  )
  validate_required_columns(
    st,
    c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence"),
    paste0("'", where, "$stop_times'")
  )

  # --- trips ---------------------------------------------------------------
  svc <- if ("service_id" %in% names(trips)) {
    as.character(trips$service_id)
  } else {
    rep(NA_character_, nrow(trips))
  }
  # The feed synthesizes exactly one service, so an absent service_id is stamped
  # with it and a different one is a contradiction rather than a second service.
  blank_svc <- is.na(svc) | !nzchar(svc)
  svc[blank_svc] <- as.character(service_id)
  bad_svc <- unique(svc[svc != as.character(service_id)])
  if (length(bad_svc) > 0L) {
    stop(
      "'",
      where,
      "$trips$service_id' must be the feed's single synthesized service '",
      service_id,
      "', but names ",
      extra_examples(bad_svc),
      ". Leave the column out to have it stamped.",
      call. = FALSE
    )
  }
  trips_out <- data.table::data.table(
    route_id = as.character(trips$route_id),
    service_id = svc,
    trip_id = as.character(trips$trip_id),
    direction_id = if ("direction_id" %in% names(trips)) {
      suppressWarnings(as.integer(trips$direction_id))
    } else {
      NA_integer_
    }
  )
  blank_id <- is.na(trips_out$trip_id) | !nzchar(trips_out$trip_id)
  if (any(blank_id)) {
    stop(
      "'",
      where,
      "$trips$trip_id' has ",
      sum(blank_id),
      " missing or empty value(s).",
      call. = FALSE
    )
  }
  blank_route <- is.na(trips_out$route_id) | !nzchar(trips_out$route_id)
  if (any(blank_route)) {
    stop(
      "'",
      where,
      "$trips$route_id' has ",
      sum(blank_route),
      " missing or empty value(s).",
      call. = FALSE
    )
  }
  dup_trip <- unique(trips_out$trip_id[duplicated(trips_out$trip_id)])
  if (length(dup_trip) > 0L) {
    stop(
      "'",
      where,
      "$trips' has ",
      length(dup_trip),
      " duplicate trip_id(s): ",
      extra_examples(dup_trip),
      ".",
      call. = FALSE
    )
  }

  # --- stop_times ----------------------------------------------------------
  st_out <- data.table::data.table(
    trip_id = as.character(st$trip_id),
    arrival_time = as.character(st$arrival_time),
    departure_time = as.character(st$departure_time),
    stop_id = as.character(st$stop_id),
    stop_sequence = suppressWarnings(as.integer(st$stop_sequence))
  )
  orphan <- setdiff(unique(st_out$trip_id), trips_out$trip_id)
  if (length(orphan) > 0L) {
    stop(
      "'",
      where,
      "$stop_times' references ",
      length(orphan),
      " trip_id(s) that '",
      where,
      "$trips' does not list: ",
      extra_examples(orphan),
      ".",
      call. = FALSE
    )
  }
  n_by_trip <- st_out[, list(n = .N), by = "trip_id"]
  thin <- merge(
    trips_out[, list(trip_id)],
    n_by_trip,
    by = "trip_id",
    all.x = TRUE
  )
  thin[is.na(n), n := 0L]
  # Fewer than two stop times is a trip that goes nowhere; gtfs-validator reports
  # it as an ERROR, so accepting it here would ship an invalid feed.
  thin <- thin[n < 2L]
  if (nrow(thin) > 0L) {
    stop(
      nrow(thin),
      " extra trip(s) in '",
      where,
      "' have fewer than two stop_times rows: ",
      extra_examples(thin$trip_id),
      ".",
      call. = FALSE
    )
  }
  blank_stop <- is.na(st_out$stop_id) | !nzchar(st_out$stop_id)
  if (any(blank_stop)) {
    stop(
      "'",
      where,
      "$stop_times$stop_id' has ",
      sum(blank_stop),
      " missing or empty value(s).",
      call. = FALSE
    )
  }
  if (anyNA(st_out$stop_sequence) || any(st_out$stop_sequence < 0L)) {
    stop(
      "'",
      where,
      "$stop_times$stop_sequence' must be a non-negative whole number.",
      call. = FALSE
    )
  }
  dup_seq <- st_out[, list(n = .N), by = c("trip_id", "stop_sequence")][n > 1L]
  if (nrow(dup_seq) > 0L) {
    stop(
      "'",
      where,
      "$stop_times' repeats a stop_sequence within a trip, so the stop order is ",
      "ambiguous: ",
      extra_examples(dup_seq$trip_id),
      ".",
      call. = FALSE
    )
  }

  arr <- hms_to_secs(st_out$arrival_time)
  dep <- hms_to_secs(st_out$departure_time)
  unparsed <- unique(st_out$trip_id[is.na(arr) | is.na(dep)])
  if (length(unparsed) > 0L) {
    stop(
      "'",
      where,
      "$stop_times' has arrival_time/departure_time values that are not ",
      "\"HH:MM:SS\" clock strings, in trip(s) ",
      extra_examples(unparsed),
      ". Extra trips carry absolute clock times (hours >= 24 are allowed for ",
      "trips that run past midnight).",
      call. = FALSE
    )
  }
  st_out[, c("arr_s", "dep_s") := list(arr, dep)]
  data.table::setorderv(st_out, c("trip_id", "stop_sequence"))
  # Non-decreasing along the trip means the interleaved arrival/departure
  # sequence never goes backwards: dwell is non-negative at each stop, and the
  # next arrival is not before this departure.
  st_out[, prev_dep := data.table::shift(dep_s, 1L), by = "trip_id"]
  bad_time <- unique(st_out[
    dep_s < arr_s | (!is.na(prev_dep) & arr_s < prev_dep),
    trip_id
  ])
  if (length(bad_time) > 0L) {
    stop(
      "'",
      where,
      "$stop_times' is not non-decreasing along the trip (a departure before ",
      "its arrival, or an arrival before the previous departure), in trip(s) ",
      extra_examples(bad_time),
      ".",
      call. = FALSE
    )
  }
  # Re-encode from the parsed seconds so extra trips and generated trips write
  # the same "HH:MM:SS" spelling, exactly as window bounds are normalised.
  st_out[, arrival_time := secs_to_clock(arr_s)]
  st_out[, departure_time := secs_to_clock(dep_s)]
  st_out[, c("arr_s", "dep_s", "prev_dep") := NULL]

  data.table::setorderv(trips_out, c("route_id", "service_id", "trip_id"))
  list(trips = trips_out[], stop_times = st_out[])
}

#' Check that extra trips reference things the feed actually contains
#'
#' Dangling references are an invalid feed per GTFS. The lenient NA-coordinates
#' path in build_stops_table() exists for *observed* data with genuinely unknown
#' coordinates; a caller-supplied table naming a stop nothing knows about is a
#' typo, and inventing a coordinate-less stop for it would hide that.
#' @noRd
check_extra_trips_refs <- function(
  extra,
  generated_trip_ids,
  pattern_stop_ids,
  known_stop_ids,
  known_route_ids
) {
  if (is.null(extra)) {
    return(invisible(NULL))
  }
  allowed_stops <- unique(c(pattern_stop_ids, known_stop_ids))
  for (s in names(extra)) {
    where <- paste0("extra_trips[[\"", s, "\"]]")
    e <- extra[[s]]
    clash <- intersect(e$trips$trip_id, generated_trip_ids)
    if (length(clash) > 0L) {
      stop(
        length(clash),
        " extra trip_id(s) in '",
        where,
        "' collide with generated frequency trip_id(s): ",
        extra_examples(clash),
        ". Generated ids are route_direction_window; give the extra trips ",
        "distinct ids.",
        call. = FALSE
      )
    }
    bad_stops <- setdiff(unique(e$stop_times$stop_id), allowed_stops)
    if (length(bad_stops) > 0L) {
      stop(
        length(bad_stops),
        " stop_id(s) referenced by '",
        where,
        "' are in neither the emitted stop patterns nor 'stops': ",
        extra_examples(bad_stops),
        ". A dangling stop reference is an invalid feed, so it is not filled ",
        "in with placeholder coordinates.",
        call. = FALSE
      )
    }
    bad_routes <- setdiff(unique(e$trips$route_id), known_route_ids)
    if (length(bad_routes) > 0L) {
      stop(
        length(bad_routes),
        " route_id(s) referenced by '",
        where,
        "' are in neither the emitted routes nor the baseline routes.txt: ",
        extra_examples(bad_routes),
        ".",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Assemble the resolved (headway group x scenario) grid
#'
#' One row per candidate \code{(route_ref, direction_id, window)} times scenario,
#' carrying what was actually applied and, for the groups that never reached the
#' feed, why. Built from the candidate snapshot rather than from the surviving
#' trips so that the row count is invariant across drop stages - which is the
#' property a caller reconciling a drop funnel is asserting against.
#' @noRd
resolved_grid <- function(groups, scen, ratios, built, dropped) {
  grid <- merge(
    groups,
    data.table::CJ(trip_id = unique(groups$trip_id), scenario = scen, unique = TRUE),
    by = "trip_id",
    allow.cartesian = TRUE
  )
  grid[, emitted := FALSE]
  grid[, ratio := NA_real_]
  grid[, headway_secs := NA_integer_]
  grid[, headway_source := NA_character_]

  # Reasons are assigned in pipeline order, so a headway group that would fail
  # more than one stage reports the first one it hit - the same convention a
  # stage-by-stage funnel uses, and the one that keeps the stage counts summing
  # to the total.
  grid[, drop_reason := NA_character_]
  grid[trip_id %in% dropped$no_stop_pattern, drop_reason := "no_stop_pattern"]
  grid[
    is.na(drop_reason) & trip_id %in% dropped$no_ratio,
    drop_reason := "no_ratio"
  ]

  if (!is.null(ratios) && nrow(ratios) > 0L) {
    grid[
      data.table::as.data.table(ratios),
      ratio := i.ratio,
      on = c("trip_id", "scenario")
    ]
  }
  if (nrow(built) > 0L) {
    grid[built, emitted := TRUE, on = c("trip_id", "scenario")]
    grid[built, headway_secs := i.headway_secs, on = c("trip_id", "scenario")]
    grid[
      built,
      headway_source := i.headway_source,
      on = c("trip_id", "scenario")
    ]
  }
  grid[
    emitted == FALSE & is.na(drop_reason) & trip_id %in% dropped$no_within_window_headway,
    drop_reason := "no_within_window_headway"
  ]
  grid[
    emitted == FALSE & is.na(drop_reason) & trip_id %in% dropped$no_headway,
    drop_reason := "no_headway"
  ]
  data.table::setcolorder(
    grid,
    c(
      "route_ref",
      "direction_id",
      "window",
      "scenario",
      "trip_id",
      "ratio",
      "headway_secs",
      "headway_source",
      "emitted",
      "drop_reason"
    )
  )
  data.table::setorderv(grid, c("scenario", "route_ref", "direction_id", "window"))
  grid[]
}

#' Resolved Headway-Group Grid Behind a Frequency Feed Set
#'
#' Returns the resolved \code{(route, direction, window, scenario)} grid that
#' \code{\link{rt2s_frequencies}} built from: what was emitted, what was
#' applied to it, and what was dropped before it reached the feed. This is the
#' programmatic counterpart to the assembly warnings - a pipeline that
#' reconciles its own headway-group accounting against the feed can gate on it
#' instead of re-deriving the outcome from the written files, which is not always
#' possible.
#'
#' @param feeds The list returned by \code{\link{rt2s_frequencies}}.
#' @return A data.table with one row per candidate headway group and scenario:
#'   \describe{
#'     \item{\code{route_ref}, \code{direction_id}, \code{window},
#'       \code{scenario}}{The headway group, keyed exactly as \code{scaling} and
#'       \code{headways} key theirs.}
#'     \item{\code{trip_id}}{The generated representative trip id, matching
#'       \code{trips.txt} when the group was emitted.}
#'     \item{\code{ratio}}{The running-time ratio applied, or \code{NA} under
#'       \code{pattern_source = "observed"}, where no ratio exists.}
#'     \item{\code{headway_secs}}{The headway actually written to
#'       \code{frequencies.txt}, or \code{NA} when the group was dropped.}
#'     \item{\code{headway_source}}{\code{"observed"} for a quantile-derived
#'       headway, \code{"override"} when \code{headways} supplied it,
#'       \code{NA} when the group was dropped.}
#'     \item{\code{emitted}}{Whether the trip reached this scenario's
#'       \code{trips.txt}.}
#'     \item{\code{drop_reason}}{\code{NA} when emitted, else, in pipeline order,
#'       \code{"no_stop_pattern"} (no served/baseline stop pattern),
#'       \code{"no_ratio"} (removed from every scenario under
#'       \code{scaling_missing = "drop"}), \code{"no_headway"} (no observed
#'       quantile and no \code{headways} override), or
#'       \code{"no_within_window_headway"} (strict observed mode found fewer
#'       than two starts in the configured window).}
#'   }
#'   \code{"no_headway"} is a drop rather than a trip emitted without a
#'   \code{frequencies.txt} row. Per GTFS a trip absent from
#'   \code{frequencies.txt} is read as exact-time, so emitting one whose
#'   \code{stop_times} are offsets from \code{00:00:00} would advertise a
#'   departure at midnight that never runs. Service that cannot be written as a
#'   repeating headway belongs in \code{rt2s_frequencies(extra_trips=)}. An
#'   emitted row therefore always carries a positive \code{headway_secs}.
#' @examples
#' \dontrun{
#' feeds <- rt2s_frequencies(events, windows = win)
#' grid <- rt2s_resolved_grid(feeds)
#' # every candidate headway group is accounted for, in every scenario
#' table(grid$scenario, grid$drop_reason, useNA = "ifany")
#' }
#' @export
rt2s_resolved_grid <- function(feeds) {
  grid <- attr(feeds, "resolved_grid", exact = TRUE)
  if (is.null(grid)) {
    stop(
      "'feeds' carries no resolved grid; it must be the list returned by ",
      "rt2s_frequencies().",
      call. = FALSE
    )
  }
  grid
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
      m <- rt2s_monotone_offsets(get(travel_col), dwell_median)
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
#' real grids carry far more trip-scenario pairs than distinct pattern-ratio
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
      m <- rt2s_monotone_offsets(travel_base * r, dwell_base * r)
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
