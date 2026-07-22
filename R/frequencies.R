# Frequency-based realized feed assembly. Collapses many observed runs of a
# route-direction into one representative trip per time window plus a
# frequencies.txt headway, emitting one feed per reliability quantile
# (structural / median / reliable). Builds on the summarise module for the
# analytics and reuses the scaffold module's agency/stops/publish-gate helpers
# for the spec-required surrounding files.

#' Force a representative stop pattern to non-decreasing times.
#'
#' Travel offsets are clamped to >= 0 and made monotone with a forward pass so
#' each stop's arrival is at least the previous stop's departure (the GTFS
#' along-trip monotonicity rule), stricter than a bare cummax on travel time.
#' @noRd
monotone_offsets <- function(travel, dwell) {
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
#' (p95) - each applying its quantile to \strong{both} travel time and headway,
#' so the reliable feed is slower with longer headways than the structural one.
#'
#' The representative stop_times are offsets from a \code{00:00:00} trip start
#' (\code{exact_times = 0} frequency semantics: only relative offsets matter),
#' clamped non-decreasing. Spec-required surrounding files (agency, routes,
#' stops, calendar, feed_info) and the publish gate are built exactly as
#' \code{\link{snapshot_scaffold}} builds them.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#'   Restrict them to the service dates that form one service pattern before
#'   calling (e.g. weekdays only) - no day-type filtering is imposed here.
#' @param windows Named list of \code{c(start, end)} time strings defining the
#'   frequency windows, passed to \code{\link{time_window}} (overnight windows
#'   such as \code{c("22:00", "26:00")} are supported). Required: a
#'   frequency-based feed needs defined windows. Trips outside every window are
#'   not emitted.
#' @param quantiles Named numeric vector of reliability quantiles in
#'   \code{[0, 1]}; one feed is produced per entry, named by its name. Default
#'   \code{c(structural = 0.05, median = 0.5, reliable = 0.95)}.
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
#' @param max_headway_secs Passed to \code{\link{obs_headways}}; gaps above it
#'   are treated as between-service breaks. Default 10800 (3 h).
#' @return A named list of gtfsio-convention feed objects, one per quantile
#'   (e.g. \code{$structural}, \code{$median}, \code{$reliable}); write each
#'   with \code{gtfsio::export_gtfs()}. Each carries \code{publishable} /
#'   \code{publish_blockers} attributes (see \code{\link{snapshot_publishable}}).
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
  max_headway_secs = 3L * 3600L
) {
  dt <- validate_events(events)
  check_quantiles(quantiles)
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
  scen <- names(quantiles)

  # --- analytics (computed once, all scenarios) -----------------------------
  hw <- obs_headways(dt, windows, quantiles, max_headway_secs)
  hw <- hw[window != "other"]
  tt <- obs_travel_times(dt, quantiles)
  if (nrow(hw) == 0L) {
    stop(
      "No (route, direction, window) group has a usable headway; check that ",
      "'events' cover multiple runs inside the given windows.",
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
  # obs_headways and obs_travel_times share one "served" definition, so a
  # headway group is normally guaranteed a pattern - the drop/guard below are a
  # backstop against a broken invariant, never returning an empty feed.
  patt_key <- unique(tt[, list(route_ref, direction_id)])
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
      " (route, direction, window) group(s) had a headway but no served ",
      "stop pattern and were dropped.",
      call. = FALSE
    )
  }
  grp <- grp[!is.na(has_pattern)]
  if (nrow(grp) == 0L) {
    stop(
      "No (route, direction, window) group with a headway has a served stop ",
      "pattern, so no trip can be built and the feed would be empty. Supply ",
      "'events' with served stops for the routes that have headways.",
      call. = FALSE
    )
  }

  # --- surrounding files + publish gate (emit warnings once) ----------------
  sink <- make_blocker_sink(strict)
  emit <- sink$emit
  ag <- resolve_agency(agency, emit, strict)
  if (missing(route_type)) {
    message("[INFO] route_type not given; scaffolding routes as 3 (bus).")
  }

  stop_ids <- sort(unique(tt[
    paste(route_ref, direction_id) %in%
      unique(grp[, paste(route_ref, direction_id)]),
    stop_ref
  ]))
  stops_out <- build_stops_table(stop_ids, stops, emit, strict)
  blockers <- sink$blockers()

  routes <- unique(grp[, list(route_id)])
  routes[, agency_id := "AG1"]
  routes[, route_short_name := route_id]
  routes[, route_long_name := ""]
  routes[, route_type := as.integer(route_type)]
  data.table::setcolorder(
    routes,
    c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type")
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
    agency_id = "AG1",
    agency_name = ag$name,
    agency_url = ag$url,
    agency_timezone = ag$timezone
  )

  # --- one feed per scenario ------------------------------------------------
  build_scenario <- function(s) {
    travel_col <- paste0("travel_", s)
    headway_col <- paste0("headway_", s)

    pat <- data.table::copy(tt)
    data.table::setorder(pat, route_ref, direction_id, stop_sequence)
    pattern <- pat[,
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

    st <- merge(
      grp[, list(route_ref, direction_id, trip_id)],
      pattern,
      by = c("route_ref", "direction_id"),
      allow.cartesian = TRUE
    )
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
