#' Scaffold a Standard-Compliant GTFS Feed from Observed Stop Events
#'
#' Baseline-free assembly: synthesizes every spec-required GTFS file from
#' observed stop events plus user-supplied agency metadata, with deterministic,
#' properly linked identifiers. Call it directly when there is no planned feed
#' to inherit from at all; \code{\link{rt2s_assemble}} also falls back to it
#' when it is given no \code{baseline}.
#'
#' What cannot come from GTFS-RT and must be supplied (or is filled with a
#' flagged placeholder): agency name/url/timezone (spec-required), stop
#' coordinates (\code{stops} argument, e.g. from
#' \code{gps2gtfs::g2g_stops_from_positions()}), and \code{route_type}
#' (defaults to 3, bus, with a warning).
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @param agency Named list with \code{name}, \code{url}, \code{timezone}.
#'   Placeholders + a warning when omitted.
#' @param stops Optional table of stop locations: \code{stop_id} plus
#'   \code{latitude}/\code{longitude} (or \code{stop_lat}/\code{stop_lon}),
#'   optionally \code{stop_name}. Stops present in events but absent here get
#'   NA coordinates and a warning (the result will not validate until they
#'   are filled).
#' @param route_type GTFS route type for scaffolded routes. Default 3 (bus).
#' @param shapes Optional \code{shapes.txt}-shaped table (e.g. from
#'   \code{gps2gtfs::g2g_shapes_from_trips()}). Linked to trips via
#'   \code{trips.shape_id} when the events carry a matching \code{shape_ref}
#'   (see \code{rt2s_events_from_stop_times(shape_ref_prefix=)}); shapes not
#'   referenced by any trip are dropped, and references without matching
#'   geometry warn (or error under \code{strict}).
#' @param tz Timezone of the service days; used to derive GTFS clock strings
#'   (with >24:00:00) from absolute event times. Defaults to
#'   \code{agency$timezone}, else "UTC".
#' @param feed_lang Primary language of the feed, written to
#'   \code{feed_info.feed_lang} (spec-required in feed_info.txt). Default
#'   \code{"en"}.
#' @param feed_contact_email,feed_contact_url Optional recommended
#'   \code{feed_info.txt} contact fields. Written only when supplied.
#' @param strict Logical. When \code{TRUE}, conditions that would yield a
#'   non-publishable feed - placeholder agency metadata or stops missing
#'   spec-required coordinates - raise an error instead of a warning. Use it
#'   as a publish gate: a scaffold that returns under \code{strict = TRUE}
#'   has no known spec-required gaps introduced by scaffolding (it is not a
#'   substitute for full GTFS validation). Default \code{FALSE}.
#' @return A gtfsio-convention feed object (class \code{gtfs}, named list of
#'   data.tables) - write it with \code{gtfsio::export_gtfs()}.
#' @export
rt2s_scaffold <- function(
  events,
  agency = NULL,
  stops = NULL,
  route_type = 3L,
  shapes = NULL,
  tz = NULL,
  feed_lang = "en",
  feed_contact_email = NULL,
  feed_contact_url = NULL,
  strict = FALSE
) {
  events <- rt2s_events_validate(events)

  # Publish blockers: conditions that make the feed non-spec-compliant. They
  # are recorded on the returned object (attr "publishable" / "publish_blockers")
  # so a downstream publish step can check them programmatically instead of
  # relying on a human reading warnings. In strict mode any of them is an error.
  sink <- make_blocker_sink(strict)
  emit <- sink$emit

  ag <- resolve_agency(agency, emit, strict)
  agency_name <- ag$name
  agency_url <- ag$url
  agency_tz <- ag$timezone
  if (is.null(tz)) {
    tz <- if (!is.null(agency$timezone)) agency$timezone else "UTC"
  }
  if (missing(route_type)) {
    message("[INFO] route_type not given; scaffolding routes as 3 (bus).")
  }

  served <- events[!(provenance %in% c("canceled", "skipped"))]
  if (nrow(served) == 0L) {
    stop(
      "No served stop events (everything is canceled/skipped); nothing to ",
      "scaffold.",
      call. = FALSE
    )
  }

  # --- trips & services -----------------------------------------------------
  trips_key <- unique(served[, .(
    trip_ref, service_date, route_ref, direction_id, shape_ref
  )])
  recurring <- trips_key[, .N, by = trip_ref][N > 1L, trip_ref]
  trips_key[, trip_id := ifelse(
    trip_ref %in% recurring,
    paste0(trip_ref, "_", yyyymmdd(service_date)),
    trip_ref
  )]
  trips_key[, service_id := paste0("SVC_", yyyymmdd(service_date))]
  trips_key[, route_id := ifelse(is.na(route_ref), "R1", as.character(route_ref))]

  routes <- unique(trips_key[, .(route_id)])
  routes[, agency_id := "AG1"]
  routes[, route_short_name := route_id]
  routes[, route_long_name := ""]
  route_type_val <- as.integer(route_type)
  routes[, route_type := route_type_val]
  data.table::setcolorder(
    routes,
    c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type")
  )

  calendar_dates <- unique(trips_key[, .(
    service_id,
    date = yyyymmdd(service_date)
  )])
  calendar_dates[, exception_type := 1L]

  # --- stop_times -----------------------------------------------------------
  st <- merge(
    served,
    trips_key[, .(trip_ref, service_date, trip_id)],
    by = c("trip_ref", "service_date")
  )
  data.table::setorderv(st, c("trip_id", "arrival_time"))
  st[, seq_final := stop_sequence]
  st[is.na(seq_final), seq_final := seq_len(.N), by = trip_id]
  stop_times <- st[, .(
    trip_id,
    arrival_time = gtfs_clock(arrival_time, service_date, tz),
    departure_time = gtfs_clock(departure_time, service_date, tz),
    stop_id = stop_ref,
    stop_sequence = as.integer(seq_final)
  )]
  data.table::setorderv(stop_times, c("trip_id", "stop_sequence"))

  # --- stops ----------------------------------------------------------------
  stop_ids <- sort(unique(stop_times$stop_id))
  stops_out <- build_stops_table(stop_ids, stops, emit, strict)

  # --- shapes ---------------------------------------------------------------
  # Link trips.shape_id to the supplied shapes via the shape_ref carried on
  # the events (set to match gps2gtfs::g2g_shapes_from_trips()). Keep only
  # shapes that are actually referenced; error/warn on references with no
  # matching geometry.
  shapes_out <- NULL
  trip_shape <- rep(NA_character_, nrow(trips_key))
  if (!is.null(shapes)) {
    shp <- data.table::as.data.table(shapes)
    validate_required_columns(
      shp,
      c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
      "shapes"
    )
    shp[, shape_id := as.character(shape_id)]
    if (any(!is.na(trips_key$shape_ref))) {
      trip_shape <- as.character(trips_key$shape_ref)
      referenced <- unique(trip_shape[!is.na(trip_shape)])
      missing_shapes <- setdiff(referenced, unique(shp$shape_id))
      if (length(missing_shapes) > 0L) {
        emit(
          length(missing_shapes),
          " trip(s) reference a shape_id absent from 'shapes' (e.g. ",
          paste(utils::head(missing_shapes, 3L), collapse = ", "),
          "); their shape_id is dropped.",
          if (isTRUE(strict)) " (strict mode)" else ""
        )
        trip_shape[trip_shape %in% missing_shapes] <- NA_character_
      }
      shapes_out <- shp[shape_id %in% referenced]
    } else {
      emit(
        "'shapes' supplied but events carry no shape_ref, so shapes cannot ",
        "be linked to trips (pass shape_ref_prefix= to ",
        "rt2s_events_from_stop_times()). Shapes are dropped.",
        if (isTRUE(strict)) " (strict mode)" else ""
      )
    }
  }

  # --- assemble object ------------------------------------------------------
  trips_out <- trips_key[, .(
    route_id,
    service_id,
    trip_id,
    direction_id = as.integer(direction_id)
  )]
  if (!is.null(shapes_out) && any(!is.na(trip_shape))) {
    trips_out[, shape_id := trip_shape]
  }
  data.table::setorderv(trips_out, c("route_id", "service_id", "trip_id"))

  feed_info <- data.table::data.table(
    feed_publisher_name = agency_name,
    feed_publisher_url = agency_url,
    feed_lang = feed_lang,
    feed_start_date = min(calendar_dates$date),
    feed_end_date = max(calendar_dates$date)
  )
  if (!is.null(feed_contact_email)) {
    feed_info[, feed_contact_email := as.character(feed_contact_email)]
  }
  if (!is.null(feed_contact_url)) {
    feed_info[, feed_contact_url := as.character(feed_contact_url)]
  }

  feed <- list(
    agency = data.table::data.table(
      agency_id = "AG1",
      agency_name = agency_name,
      agency_url = agency_url,
      agency_timezone = agency_tz
    ),
    stops = stops_out,
    routes = routes,
    trips = trips_out,
    stop_times = stop_times,
    calendar_dates = calendar_dates,
    feed_info = feed_info
  )
  if (!is.null(shapes_out)) {
    feed$shapes <- shapes_out
  }
  stamp_publishable(as_gtfs_object(feed), sink$blockers())
}

#' Record Publish-Readiness on an Assembled Feed
#'
#' Stamps \code{publishable} (logical) and \code{publish_blockers} (character
#' vector of reasons) attributes on a feed object. Read them with
#' \code{\link{rt2s_publishable}}.
#' @noRd
stamp_publishable <- function(feed, blockers) {
  attr(feed, "publishable") <- length(blockers) == 0L
  attr(feed, "publish_blockers") <- as.character(blockers)
  feed
}

#' Collect publish blockers while warning (or erroring under strict mode).
#'
#' Returns a list with \code{emit(..., blocker=)} - which records a blocker
#' string and then \code{warning()}s, or \code{stop()}s when \code{strict} -
#' and \code{blockers()} to read what was collected. Shared by the scaffold
#' and frequency assemblers so both gate publication identically.
#' @noRd
make_blocker_sink <- function(strict) {
  env <- new.env(parent = emptyenv())
  env$blockers <- character(0)
  emit <- function(..., blocker = NULL) {
    if (!is.null(blocker)) {
      env$blockers <- c(env$blockers, blocker)
    }
    if (isTRUE(strict)) {
      stop(..., call. = FALSE)
    } else {
      warning(..., call. = FALSE)
    }
  }
  list(emit = emit, blockers = function() env$blockers)
}

#' Resolve agency metadata to name/url/timezone, flagging placeholders.
#' @noRd
resolve_agency <- function(agency, emit, strict) {
  if (is.null(agency) || is.null(agency$name)) {
    emit(
      "No agency metadata supplied; agency.txt gets placeholder values. ",
      "Pass agency = list(name=, url=, timezone=) - these spec-required ",
      "fields are not derivable from GTFS-RT.",
      if (isTRUE(strict)) " (strict mode)" else "",
      blocker = "agency.txt has placeholder values (name/url/timezone not supplied)"
    )
  }
  list(
    name = if (!is.null(agency$name)) agency$name else "Unknown agency (placeholder)",
    url = if (!is.null(agency$url)) agency$url else "https://example.org",
    timezone = if (!is.null(agency$timezone)) agency$timezone else "Etc/UTC"
  )
}

#' Build stops.txt for the given stop_ids from a user stops table, flagging
#' any stop that lacks spec-required coordinates as a publish blocker.
#' @noRd
build_stops_table <- function(stop_ids, stops, emit, strict) {
  if (!is.null(stops)) {
    sdt <- data.table::as.data.table(stops)
    if ("latitude" %in% names(sdt)) {
      data.table::setnames(sdt, "latitude", "stop_lat")
    }
    if ("longitude" %in% names(sdt)) {
      data.table::setnames(sdt, "longitude", "stop_lon")
    }
    validate_required_columns(sdt, c("stop_id", "stop_lat", "stop_lon"), "stops")
    if (!"stop_name" %in% names(sdt)) {
      sdt[, stop_name := paste("Stop", stop_id)]
    }
    sdt <- sdt[, .(
      stop_id = as.character(stop_id),
      stop_name = as.character(stop_name),
      stop_lat = as.double(stop_lat),
      stop_lon = as.double(stop_lon)
    )]
    sdt <- unique(sdt, by = "stop_id")
  } else {
    sdt <- data.table::data.table(
      stop_id = character(),
      stop_name = character(),
      stop_lat = double(),
      stop_lon = double()
    )
  }
  missing_stops <- setdiff(stop_ids, sdt$stop_id)
  if (length(missing_stops) > 0L) {
    emit(
      length(missing_stops),
      " stop(s) have no coordinates (spec-required stop_lat/stop_lon are ",
      "NA). Supply 'stops' - e.g. estimated from Vehicle Positions with ",
      "gps2gtfs::g2g_stops_from_positions() - before publishing.",
      if (isTRUE(strict)) " (strict mode)" else "",
      blocker = paste0(
        length(missing_stops),
        " stop(s) missing spec-required coordinates"
      )
    )
    sdt <- rbind(
      sdt,
      data.table::data.table(
        stop_id = missing_stops,
        stop_name = paste("Stop", missing_stops),
        stop_lat = NA_real_,
        stop_lon = NA_real_
      )
    )
  }
  stops_out <- sdt[stop_id %in% stop_ids]
  data.table::setkeyv(stops_out, "stop_id")
  stops_out
}

#' Publish-Readiness of an Assembled Feed
#'
#' Reports whether \code{\link{rt2s_scaffold}} / \code{\link{rt2s_assemble}}
#' produced a spec-compliant feed, and if not, why. This is the programmatic
#' counterpart to the assembly warnings: a publish/export step can gate on it
#' instead of relying on a human noticing a warning.
#'
#' @param feed A feed object returned by \code{rt2s_scaffold()} or
#'   \code{rt2s_assemble()}.
#' @return A list with \code{publishable} (logical) and \code{blockers}
#'   (character vector; empty when publishable). A feed with blockers is still
#'   a valid R object for inspection/analysis - it just should not be published
#'   as standard GTFS until the blockers are resolved (or built with
#'   \code{strict = TRUE}, which refuses to produce one).
#' @examples
#' \dontrun{
#' feed <- rt2s_scaffold(events)
#' status <- rt2s_publishable(feed)
#' if (!status$publishable) stop(paste(status$blockers, collapse = "; "))
#' }
#' @export
rt2s_publishable <- function(feed) {
  pub <- attr(feed, "publishable")
  blk <- attr(feed, "publish_blockers")
  list(
    publishable = isTRUE(pub),
    blockers = if (is.null(blk)) character(0) else as.character(blk)
  )
}

# Rank of `x` within each (a, b) group, ties in row order and NA `x` last, as
# frank(x, ties.method = "first") by group would give. One stable sort of the
# whole table instead of a frank() call per group, which on a daily feed (one
# group per trip and stop) dominated the assembly time.
rank_within <- function(a, b, x) {
  o <- order(a, b, x, na.last = TRUE, method = "radix")
  out <- integer(length(x))
  out[o] <- data.table::rowid(a[o], b[o])
  out
}

# Order-preserving pairing of two sorted numeric vectors with length(x) <=
# length(y): returns, for each x[i], the index of its partner in y, the
# partners strictly increasing and the summed |x - y| minimal. Dynamic
# programming over (i, j); the inputs are the visits of one trip at one stop,
# so they are tiny.
monotone_match <- function(x, y) {
  m <- length(x)
  k <- length(y)
  cost <- matrix(Inf, m, k)
  from <- matrix(NA_integer_, m, k)
  for (i in seq_len(m)) {
    for (j in i:(k - m + i)) {
      d <- abs(x[i] - y[j])
      if (i == 1L) {
        cost[i, j] <- d
      } else {
        prev <- cost[i - 1L, seq_len(j - 1L)]
        b <- which.min(prev)
        cost[i, j] <- d + prev[b]
        from[i, j] <- b
      }
    }
  }
  out <- integer(m)
  out[m] <- which.min(cost[m, ])
  for (i in rev(seq_len(m - 1L))) {
    out[i] <- from[i + 1L, out[i + 1L]]
  }
  out
}

# Planned seconds after the service-day origin for baseline stop_times rows
# of whole trips: arrival_time, else departure_time; a row with neither (a
# non-timepoint) is interpolated linearly over stop_sequence between the
# trip's timed rows. Rows that cannot be placed stay NA.
planned_secs <- function(trip, sequence, arr, dep) {
  secs <- if (is.null(arr)) rep(NA_real_, length(trip)) else gtfs_time_secs(arr)
  if (!is.null(dep)) {
    no_arr <- is.na(secs)
    secs[no_arr] <- gtfs_time_secs(dep[no_arr])
  }
  untimed <- unique(trip[is.na(secs)])
  if (length(untimed) == 0L) {
    return(secs)
  }
  by_trip <- split(seq_along(trip), trip)[untimed]
  for (r in by_trip) {
    timed <- r[!is.na(secs[r])]
    if (length(timed) < 2L || anyNA(sequence[r])) {
      next
    }
    gap <- r[is.na(secs[r])]
    secs[gap] <- stats::approx(
      sequence[timed], secs[timed], xout = sequence[gap], rule = 2, ties = mean
    )$y
  }
  secs
}

# Planned-visit rank paired with each observed event. A stop the planned trip
# serves once pairs by observation order (the first observed visit takes it,
# later ones fall through). A stop the trip serves several times pairs each
# observed visit with the planned visit nearest in planned time, keeping both
# in order, so an unobserved first visit does not shift the second visit onto
# the first one's stop_sequence. Without usable times on either side the group
# keeps observation order. Work is linear in the rows: only trips with a
# repeated stop are timed, and each repeated group is visited once.
pair_visits <- function(trip, stop, obs_secs, base) {
  rank <- rank_within(trip, stop, obs_secs)
  base_key <- paste(base$trip_ref, base$stop_ref, sep = "\r")
  rep_rows <- which(duplicated(base_key) | duplicated(base_key, fromLast = TRUE))
  if (length(rep_rows) == 0L) {
    return(rank)
  }
  obs_key <- paste(trip, stop, sep = "\r")
  obs_rows <- which(obs_key %in% base_key[rep_rows])
  if (length(obs_rows) == 0L) {
    return(rank)
  }
  timed_rows <- which(base$trip_ref %in% trip[obs_rows])
  psecs <- rep(NA_real_, length(base_key))
  psecs[timed_rows] <- planned_secs(
    base$trip_ref[timed_rows],
    base$base_sequence[timed_rows],
    base$arrival_time[timed_rows],
    base$departure_time[timed_rows]
  )
  obs_groups <- split(obs_rows, obs_key[obs_rows])
  plan_groups <- split(rep_rows, base_key[rep_rows])[names(obs_groups)]
  for (g in names(obs_groups)) {
    rows <- obs_groups[[g]]
    rows <- rows[order(obs_secs[rows], rows)]
    plan <- plan_groups[[g]]
    plan <- plan[order(base$visit_rank[plan])]
    o <- obs_secs[rows]
    p <- psecs[plan]
    if (anyNA(o) || anyNA(p)) {
      next
    }
    if (length(o) <= length(p)) {
      rank[rows] <- base$visit_rank[plan][monotone_match(o, p)]
    } else {
      hit <- monotone_match(p, o)
      rank[rows] <- NA_integer_
      rank[rows[hit]] <- base$visit_rank[plan]
    }
  }
  rank
}

#' Assemble a Realized GTFS Feed from Observed Stop Events
#'
#' Turns observed stop events into one static GTFS feed describing the service
#' that actually operated. With a \code{baseline} (planned) feed, the realized
#' feed inherits agency, routes, stops, and shapes wholesale, keeps official
#' trip identifiers, and replaces \code{stop_times.txt} with the observed times
#' of the trips that actually ran on \code{service_date} - so
#' planned-vs-realized joins are direct. Without a baseline, a compliant feed is
#' scaffolded from scratch via \code{\link{rt2s_scaffold}}.
#'
#' In baseline mode each observed event is paired with at most one planned
#' \code{stop_times} row. Where the planned trip serves a stop more than once
#' (a loop), each observed visit takes the planned visit nearest in planned
#' time, with observed and planned visits kept in the same order, so a missed
#' first pass does not shift the second pass onto the first one's
#' \code{stop_sequence}. A planned row without times (a non-timepoint) is
#' placed by linear interpolation over \code{stop_sequence}; only where a trip
#' has fewer than two timed rows does the k-th observed visit take the k-th
#' planned one. An observed visit without a planned
#' partner is numbered chronologically after the planned ones. The output
#' therefore has exactly one row per observed event.
#'
#' This is the entry point to use when each observed run should stay its own
#' trip. To collapse many runs into one representative trip per time window with
#' a \code{frequencies.txt} headway instead, use
#' \code{\link{rt2s_frequencies}}.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @param baseline Optional planned static GTFS feed: a gtfsio/gtfstools-style
#'   object or a path to a GTFS zip.
#' @param service_date The service day the snapshot describes (one realized
#'   feed per service day in baseline mode). Defaults to the single date in
#'   \code{events}; must be given when events span several dates. In both
#'   modes only events whose \code{service_date} equals this day are kept and
#'   their clock strings are rendered relative to its GTFS origin (noon minus
#'   12h: midnight, except on a daylight-saving change day); it is an error
#'   when no event falls on it.
#' @param tz Timezone for GTFS clock strings. Defaults to the baseline's
#'   \code{agency_timezone} (baseline mode) or "UTC".
#' @param feed_lang Primary feed language written to
#'   \code{feed_info.feed_lang} in both modes. Default \code{"en"}.
#' @param feed_contact_email,feed_contact_url Optional recommended
#'   \code{feed_info.txt} contact fields, written only when supplied.
#' @param ... In scaffold mode (no baseline), passed to
#'   \code{\link{rt2s_scaffold}} (\code{agency}, \code{stops},
#'   \code{route_type}, \code{shapes}, \code{strict}).
#' @return A gtfsio-convention feed object (class \code{gtfs}); write it with
#'   \code{gtfsio::export_gtfs()}.
#' @export
rt2s_assemble <- function(
  events,
  baseline = NULL,
  service_date = NULL,
  tz = NULL,
  feed_lang = "en",
  feed_contact_email = NULL,
  feed_contact_url = NULL,
  ...
) {
  events <- rt2s_events_validate(events)

  # `service_date` is both the argument and an events column. Inside a
  # data.table bracket a bare symbol resolves to a column of that name, so the
  # day filter, the clock origin and the calendar dates below use the local
  # `svc_date` only outside `[` (the filter indexes with the logical vector
  # `keep_day`), never `service_date` or `svc_date` within a bracket.
  if (is.null(baseline)) {
    if (!is.null(service_date)) {
      svc_date <- as.Date(service_date)
      keep_day <- events$service_date == svc_date
      events <- events[keep_day]
      if (nrow(events) == 0L) {
        stop("No events on service date ", svc_date, ".", call. = FALSE)
      }
    }
    return(rt2s_scaffold(
      events,
      tz = tz,
      feed_lang = feed_lang,
      feed_contact_email = feed_contact_email,
      feed_contact_url = feed_contact_url,
      ...
    ))
  }

  baseline <- read_gtfs_input(baseline)
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

  dates <- unique(events$service_date)
  if (is.null(service_date)) {
    if (length(dates) != 1L) {
      stop(
        "Events span ",
        length(dates),
        " service dates; baseline mode builds one realized feed per day. ",
        "Pass 'service_date'.",
        call. = FALSE
      )
    }
    service_date <- dates
  }
  svc_date <- as.Date(service_date)
  keep_day <- events$service_date == svc_date
  day_events <- events[keep_day]
  if (nrow(day_events) == 0L) {
    stop("No events on service date ", svc_date, ".", call. = FALSE)
  }

  if (is.null(tz)) {
    tz <- "UTC"
    ag <- baseline$agency
    if (!is.null(ag) && "agency_timezone" %in% names(ag) && nrow(ag) > 0L) {
      tz <- as.character(ag$agency_timezone[[1L]])
    }
  }

  baseline_trips <- data.table::as.data.table(baseline$trips)
  baseline_st <- data.table::as.data.table(baseline$stop_times)
  baseline_trip_ids <- as.character(baseline_trips$trip_id)

  served <- day_events[!(provenance %in% c("canceled", "skipped"))]
  matched <- served[trip_ref %in% baseline_trip_ids]
  unmatched_refs <- setdiff(unique(served$trip_ref), baseline_trip_ids)
  if (length(unmatched_refs) > 0L) {
    warning(
      length(unmatched_refs),
      " observed trip(s) have no counterpart in the baseline and were ",
      "dropped (e.g. ",
      paste(utils::head(unmatched_refs, 3), collapse = ", "),
      "). Assemble them separately in scaffold mode if needed.",
      call. = FALSE
    )
  }
  if (nrow(matched) == 0L) {
    stop(
      "None of the observed trips match baseline trip_ids; check that the ",
      "feeds belong to the same system.",
      call. = FALSE
    )
  }

  # stop_sequence precedence: event value > baseline numbering > chronology.
  # A trip may visit the same stop more than once (a loop), so (trip, stop)
  # does not identify a planned row. Each observed event is paired with at
  # most one planned visit at its stop (pair_visits(): by planned time where
  # the stop repeats, else by order); an event without a partner falls
  # through to the chronology fallback below. The join is therefore
  # one-to-one and can never multiply rows.
  base_seq <- data.table::data.table(
    trip_ref = as.character(baseline_st$trip_id),
    stop_ref = as.character(baseline_st$stop_id),
    base_sequence = as.integer(baseline_st$stop_sequence)
  )
  data.table::set(
    base_seq,
    j = "visit_rank",
    value = rank_within(base_seq$trip_ref, base_seq$stop_ref, base_seq$base_sequence)
  )
  # Observed times on the planned clock's scale (seconds after the GTFS
  # service-day origin); an event without an arrival uses its departure.
  observed <- matched$arrival_time
  observed[is.na(observed)] <- matched$departure_time[is.na(observed)]
  observed_secs <- as.numeric(difftime(
    observed,
    gtfs_day_origin(svc_date, tz),
    units = "secs"
  ))
  data.table::set(
    matched,
    j = "visit_rank",
    value = pair_visits(
      matched$trip_ref,
      matched$stop_ref,
      observed_secs,
      list(
        trip_ref = base_seq$trip_ref,
        stop_ref = base_seq$stop_ref,
        base_sequence = base_seq$base_sequence,
        visit_rank = base_seq$visit_rank,
        arrival_time = baseline_st$arrival_time,
        departure_time = baseline_st$departure_time
      )
    )
  )
  st <- merge(
    matched,
    base_seq,
    by = c("trip_ref", "stop_ref", "visit_rank"),
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(st) != nrow(matched)) {
    stop(
      "Internal error: pairing ", nrow(matched), " observed stop events with ",
      "the baseline stop_times produced ", nrow(st), " rows; the join must ",
      "be one-to-one.",
      call. = FALSE
    )
  }
  st[, visit_rank := NULL]
  st[, seq_final := stop_sequence]
  st[is.na(seq_final), seq_final := base_sequence]
  data.table::setorderv(st, c("trip_ref", "arrival_time"))
  st[is.na(seq_final), seq_final := seq_len(.N) + 10000L, by = trip_ref]

  # Built outside `[`: inside a data.table bracket a local such as `svc_date`
  # or `tz` would resolve to an events column of the same name.
  stop_times <- data.table::data.table(
    trip_id = st$trip_ref,
    arrival_time = gtfs_clock(st$arrival_time, svc_date, tz),
    departure_time = gtfs_clock(st$departure_time, svc_date, tz),
    stop_id = st$stop_ref,
    stop_sequence = as.integer(st$seq_final)
  )
  data.table::setorderv(stop_times, c("trip_id", "stop_sequence"))

  # Realized service: exactly the trips that ran, on one service id
  service_id <- paste0("SVC_", yyyymmdd(svc_date))
  realized_trips <- baseline_trips[
    as.character(baseline_trips$trip_id) %in% unique(matched$trip_ref)
  ]
  realized_trips <- data.table::copy(realized_trips)
  data.table::set(realized_trips, j = "service_id", value = service_id)

  feed <- lapply(baseline, data.table::as.data.table)
  feed$trips <- realized_trips
  feed$stop_times <- stop_times
  feed$calendar <- NULL
  feed$calendar_dates <- data.table::data.table(
    service_id = service_id,
    date = yyyymmdd(svc_date),
    exception_type = 1L
  )
  feed$feed_info <- data.table::data.table(
    feed_publisher_name = if (!is.null(feed$agency)) {
      as.character(feed$agency$agency_name[[1L]])
    } else {
      "gtfsrt2static"
    },
    feed_publisher_url = if (
      !is.null(feed$agency) && "agency_url" %in% names(feed$agency)
    ) {
      as.character(feed$agency$agency_url[[1L]])
    } else {
      "https://example.org"
    },
    feed_lang = feed_lang,
    feed_start_date = yyyymmdd(svc_date),
    feed_end_date = yyyymmdd(svc_date)
  )
  if (!is.null(feed_contact_email)) {
    feed$feed_info[, feed_contact_email := as.character(feed_contact_email)]
  }
  if (!is.null(feed_contact_url)) {
    feed$feed_info[, feed_contact_url := as.character(feed_contact_url)]
  }
  feed <- feed[!vapply(feed, is.null, logical(1))]

  # Publish-readiness: baseline feeds inherit agency/stops, so they are
  # normally complete, but verify the two spec-required essentials.
  blockers <- character(0)
  if (is.null(feed$agency) || nrow(feed$agency) == 0L) {
    blockers <- c(blockers, "baseline feed has no agency.txt")
  }
  if (!is.null(feed$stops) && all(c("stop_lat", "stop_lon") %in% names(feed$stops))) {
    n_na <- sum(is.na(feed$stops$stop_lat) | is.na(feed$stops$stop_lon))
    if (n_na > 0L) {
      blockers <- c(blockers, paste0(n_na, " stop(s) missing spec-required coordinates"))
    }
  }
  stamp_publishable(as_gtfs_object(feed), blockers)
}