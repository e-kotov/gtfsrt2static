#' Scaffold a Standard-Compliant GTFS Feed from Observed Stop Events
#'
#' Baseline-free assembly: synthesizes every spec-required GTFS file from
#' observed stop events plus user-supplied agency metadata, with
#' deterministic, properly linked identifiers. Used by
#' \code{\link{snapshot_assemble}} when no baseline feed is given.
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
#'   \code{gps2gtfs::g2g_shapes_from_trips()}); included verbatim.
#' @param tz Timezone of the service days; used to derive GTFS clock strings
#'   (with >24:00:00) from absolute event times. Defaults to
#'   \code{agency$timezone}, else "UTC".
#' @return A gtfsio-convention feed object (class \code{gtfs}, named list of
#'   data.tables) - write it with \code{gtfsio::export_gtfs()}.
#' @export
snapshot_scaffold <- function(
  events,
  agency = NULL,
  stops = NULL,
  route_type = 3L,
  shapes = NULL,
  tz = NULL
) {
  events <- validate_events(events)

  if (is.null(agency) || is.null(agency$name)) {
    warning(
      "No agency metadata supplied; agency.txt gets placeholder values. ",
      "Pass agency = list(name=, url=, timezone=) - these spec-required ",
      "fields are not derivable from GTFS-RT.",
      call. = FALSE
    )
  }
  agency_name <- if (!is.null(agency$name)) agency$name else "Unknown agency (placeholder)"
  agency_url <- if (!is.null(agency$url)) agency$url else "https://example.org"
  agency_tz <- if (!is.null(agency$timezone)) agency$timezone else "Etc/UTC"
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
  trips_key <- unique(served[, .(trip_ref, service_date, route_ref, direction_id)])
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
    warning(
      length(missing_stops),
      " stop(s) have no coordinates (spec-required stop_lat/stop_lon are ",
      "NA). Supply 'stops' - e.g. estimated from Vehicle Positions with ",
      "gps2gtfs::g2g_stops_from_positions() - before publishing.",
      call. = FALSE
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

  # --- assemble object ------------------------------------------------------
  trips_out <- trips_key[, .(
    route_id,
    service_id,
    trip_id,
    direction_id = as.integer(direction_id)
  )]
  data.table::setorderv(trips_out, c("route_id", "service_id", "trip_id"))

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
    feed_info = data.table::data.table(
      feed_publisher_name = agency_name,
      feed_publisher_url = agency_url,
      feed_lang = "en",
      feed_start_date = min(calendar_dates$date),
      feed_end_date = max(calendar_dates$date)
    )
  )
  if (!is.null(shapes)) {
    feed$shapes <- data.table::as.data.table(shapes)
  }
  as_gtfs_object(feed)
}

#' Assemble a Realized GTFS Feed from Observed Stop Events
#'
#' The main entry point of the assembly module. With a \code{baseline}
#' (planned) feed, the realized feed inherits agency, routes, stops, and
#' shapes wholesale, keeps official trip identifiers, and replaces
#' \code{stop_times.txt} with the observed times of the trips that actually
#' ran on \code{service_date} - so planned-vs-realized joins are direct.
#' Without a baseline, a compliant feed is scaffolded from scratch via
#' \code{\link{snapshot_scaffold}}.
#'
#' @param events Observed stop events (see \link{observed-stop-events}).
#' @param baseline Optional planned static GTFS feed: a gtfsio/gtfstools-style
#'   object or a path to a GTFS zip.
#' @param service_date The service day the snapshot describes (one realized
#'   feed per service day in baseline mode). Defaults to the single date in
#'   \code{events}; must be given when events span several dates.
#' @param tz Timezone for GTFS clock strings. Defaults to the baseline's
#'   \code{agency_timezone} (baseline mode) or "UTC".
#' @param ... In scaffold mode (no baseline), passed to
#'   \code{\link{snapshot_scaffold}} (\code{agency}, \code{stops},
#'   \code{route_type}, \code{shapes}).
#' @return A gtfsio-convention feed object (class \code{gtfs}); write it with
#'   \code{gtfsio::export_gtfs()}.
#' @export
snapshot_assemble <- function(
  events,
  baseline = NULL,
  service_date = NULL,
  tz = NULL,
  ...
) {
  events <- validate_events(events)

  if (is.null(baseline)) {
    if (!is.null(service_date)) {
      events <- events[events$service_date == as.Date(service_date)]
    }
    return(snapshot_scaffold(events, tz = tz, ...))
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
  service_date <- as.Date(service_date)
  day_events <- events[events$service_date == service_date]
  if (nrow(day_events) == 0L) {
    stop("No events on service date ", service_date, ".", call. = FALSE)
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

  # stop_sequence precedence: event value > baseline numbering > chronology
  base_seq <- baseline_st[, .(
    trip_ref = as.character(trip_id),
    stop_ref = as.character(stop_id),
    base_sequence = as.integer(stop_sequence)
  )]
  st <- merge(
    matched,
    base_seq,
    by = c("trip_ref", "stop_ref"),
    all.x = TRUE,
    sort = FALSE
  )
  st[, seq_final := stop_sequence]
  st[is.na(seq_final), seq_final := base_sequence]
  data.table::setorderv(st, c("trip_ref", "arrival_time"))
  st[is.na(seq_final), seq_final := seq_len(.N) + 10000L, by = trip_ref]

  stop_times <- st[, .(
    trip_id = trip_ref,
    arrival_time = gtfs_clock(arrival_time, service_date, tz),
    departure_time = gtfs_clock(departure_time, service_date, tz),
    stop_id = stop_ref,
    stop_sequence = as.integer(seq_final)
  )]
  data.table::setorderv(stop_times, c("trip_id", "stop_sequence"))

  # Realized service: exactly the trips that ran, on one service id
  service_id <- paste0("SVC_", yyyymmdd(service_date))
  realized_trips <- baseline_trips[
    as.character(baseline_trips$trip_id) %in% unique(matched$trip_ref)
  ]
  realized_trips <- data.table::copy(realized_trips)
  svc_id <- service_id
  realized_trips[, service_id := svc_id]

  feed <- lapply(baseline, data.table::as.data.table)
  feed$trips <- realized_trips
  feed$stop_times <- stop_times
  feed$calendar <- NULL
  feed$calendar_dates <- data.table::data.table(
    service_id = service_id,
    date = yyyymmdd(service_date),
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
    feed_lang = "en",
    feed_start_date = yyyymmdd(service_date),
    feed_end_date = yyyymmdd(service_date)
  )
  feed <- feed[!vapply(feed, is.null, logical(1))]
  as_gtfs_object(feed)
}