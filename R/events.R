#' The Observed Stop Events Schema
#'
#' Observed stop events are the package's canonical intermediate table - the
#' point where every input path converges before assembly and comparison:
#' one row per trip, stop, and service date, holding the *actual* (or best
#' observed) arrival and departure times plus a provenance audit trail.
#'
#' Columns:
#' \describe{
#'   \item{trip_ref}{Trip identity. The GTFS-RT/baseline \code{trip_id} when
#'     known, a synthetic id for inferred trips. Never NA.}
#'   \item{route_ref}{Route identity, NA when unknown.}
#'   \item{direction_id}{GTFS direction (0/1), NA when unknown.}
#'   \item{service_date}{\code{Date}. The service day the trip belongs to
#'     (attributed by trip start; post-midnight stops keep their trip's
#'     date).}
#'   \item{stop_ref}{Stop identity; NA only on trip-level rows
#'     (\code{provenance == "canceled"}).}
#'   \item{stop_sequence}{Integer order along the trip; optional (NA), the
#'     assembler guarantees a spec-compliant sequence.}
#'   \item{arrival_time, departure_time}{Absolute \code{POSIXct} times; the
#'     assembler derives GTFS clock strings (with >24:00:00) from them.}
#'   \item{provenance}{One of \code{"observed"} (reported at/after passage),
#'     \code{"predicted-last"} (last prediction before passage),
#'     \code{"propagated"}, \code{"skipped"} (stop not served),
#'     \code{"canceled"} (whole trip did not run).}
#'   \item{vehicle_ref}{Vehicle identity, NA when unknown.}
#'   \item{source}{One of \code{"trip_updates"}, \code{"positions"},
#'     \code{"gps"}.}
#' }
#'
#' @name observed-stop-events
NULL

event_provenance_levels <- c(
  "observed",
  "predicted-last",
  "propagated",
  "skipped",
  "canceled"
)
event_source_levels <- c("trip_updates", "positions", "gps")
event_columns <- c(
  "trip_ref",
  "route_ref",
  "direction_id",
  "service_date",
  "stop_ref",
  "stop_sequence",
  "arrival_time",
  "departure_time",
  "provenance",
  "vehicle_ref",
  "source"
)

#' Validate an Observed Stop Events Table
#'
#' Checks that a table conforms to the observed stop events schema (see
#' \link{observed-stop-events}). Called internally by all converters and by
#' the assembler; exported so external producers can verify their own tables.
#'
#' @param events A data.frame/data.table of observed stop events.
#' @return The validated events table as a keyed data.table (invisibly usable
#'   in pipes); errors describe the first violation found.
#' @export
validate_events <- function(events) {
  dt <- data.table::as.data.table(events)
  validate_required_columns(dt, event_columns, "observed stop events")

  if (anyNA(dt$trip_ref) || any(!nzchar(trimws(as.character(dt$trip_ref))))) {
    stop("'trip_ref' must not contain missing values.", call. = FALSE)
  }
  if (!inherits(dt$service_date, "Date") || anyNA(dt$service_date)) {
    stop("'service_date' must be a Date without missing values.", call. = FALSE)
  }
  bad_prov <- setdiff(unique(as.character(dt$provenance)), event_provenance_levels)
  if (length(bad_prov) > 0L) {
    stop(
      "Unknown 'provenance' value(s): ",
      paste(bad_prov, collapse = ", "),
      ". Allowed: ",
      paste(event_provenance_levels, collapse = ", "),
      call. = FALSE
    )
  }
  bad_src <- setdiff(unique(as.character(dt$source)), event_source_levels)
  if (length(bad_src) > 0L) {
    stop(
      "Unknown 'source' value(s): ",
      paste(bad_src, collapse = ", "),
      call. = FALSE
    )
  }
  for (col in c("arrival_time", "departure_time")) {
    if (!inherits(dt[[col]], "POSIXct")) {
      stop("'", col, "' must be POSIXct.", call. = FALSE)
    }
  }
  not_canceled <- as.character(dt$provenance) != "canceled"
  if (anyNA(dt$stop_ref[not_canceled])) {
    stop(
      "'stop_ref' may be NA only on canceled trip rows.",
      call. = FALSE
    )
  }
  both <- !is.na(dt$arrival_time) & !is.na(dt$departure_time)
  if (any(both & dt$departure_time < dt$arrival_time)) {
    stop(
      "'departure_time' must not precede 'arrival_time'.",
      call. = FALSE
    )
  }

  data.table::setcolorder(dt, event_columns)
  data.table::setkeyv(dt, c("service_date", "trip_ref", "stop_sequence"))
  dt[]
}

#' Observed Stop Events from gps2gtfs Stop Times
#'
#' Converts the \code{$stop_times} table produced by
#' \code{gps2gtfs::g2g_extract_trips_and_stop_times()} into observed stop
#' events (see \link{observed-stop-events}).
#'
#' @param stop_times The gps2gtfs stop times table: columns \code{trip_id},
#'   \code{vehicle_id}, \code{date}, \code{direction} (1/2),
#'   \code{stop_id}, \code{arrival_time}/\code{departure_time}
#'   ("HH:MM:SS" within the date).
#' @param tz Timezone the "HH:MM:SS" strings refer to (the timezone the GPS
#'   data was cleaned in). Default "UTC".
#' @param route_ref Optional route identity to stamp on all events (gps2gtfs
#'   models one route per run). Default NA.
#' @param source Provenance source label: \code{"positions"} (GTFS-RT
#'   Vehicle Positions, default) or \code{"gps"} (raw AVL/logger data).
#' @param trip_ref_prefix Prefix for synthetic trip identities. Trip refs are
#'   \code{<prefix><yyyymmdd>_<trip_id>} so they stay unique across service
#'   dates. Default \code{"g2g_"}.
#' @return A validated observed stop events data.table.
#' @export
snapshot_from_stop_times <- function(
  stop_times,
  tz = "UTC",
  route_ref = NA_character_,
  source = c("positions", "gps"),
  trip_ref_prefix = "g2g_"
) {
  source <- match.arg(source)
  dt <- data.table::as.data.table(stop_times)
  validate_required_columns(
    dt,
    c(
      "trip_id",
      "vehicle_id",
      "date",
      "direction",
      "stop_id",
      "arrival_time",
      "departure_time"
    ),
    "stop_times"
  )

  service_date <- as.Date(as.character(dt$date))
  out <- data.table::data.table(
    trip_ref = paste0(
      trip_ref_prefix,
      format(service_date, "%Y%m%d"),
      "_",
      dt$trip_id
    ),
    route_ref = rep(as.character(route_ref), nrow(dt)),
    direction_id = as.integer(dt$direction) - 1L,
    service_date = service_date,
    stop_ref = as.character(dt$stop_id),
    stop_sequence = NA_integer_,
    arrival_time = as.POSIXct(paste(dt$date, dt$arrival_time), tz = tz),
    departure_time = as.POSIXct(paste(dt$date, dt$departure_time), tz = tz),
    provenance = rep("observed", nrow(dt)),
    vehicle_ref = as.character(dt$vehicle_id),
    source = rep(source, nrow(dt))
  )
  data.table::setorderv(out, c("trip_ref", "arrival_time"))
  out[, stop_sequence := seq_len(.N), by = trip_ref]
  validate_events(out)
}

#' Observed Stop Events from GTFS-Realtime Trip Updates
#'
#' Reduces an archive of Trip Updates (many successive predictions per trip
#' and stop across polls) to observed stop events (see
#' \link{observed-stop-events}): per (trip, service date, stop), the latest
#' report wins - labeled \code{"observed"} when it was issued at or after
#' the vehicle's departure from the stop, \code{"predicted-last"} otherwise.
#' \code{SKIPPED} stops and \code{CANCELED}/\code{DELETED} trips become
#' explicit negative-information rows; \code{NO_DATA} rows are dropped with
#' a message.
#'
#' @param updates A data.frame as returned by
#'   \code{gtfsrealtime::read_gtfsrt_trip_updates()}: one row per stop-time
#'   update with \code{trip_id}, \code{stop_id} and/or \code{stop_sequence},
#'   \code{arrival_time}/\code{arrival_delay},
#'   \code{departure_time}/\code{departure_delay}, schedule relationships,
#'   \code{start_date}, \code{vehicle_id}, \code{file_timestamp}.
#' @param baseline Optional baseline static GTFS feed (object or zip path).
#'   Required to resolve delay-only updates (a delay without an absolute
#'   time can only be interpreted against the scheduled time).
#' @param tz Timezone of the feed's service days (used to resolve delay-only
#'   updates against baseline scheduled times). Default "UTC".
#' @return A validated observed stop events data.table.
#' @export
snapshot_from_trip_updates <- function(updates, baseline = NULL, tz = "UTC") {
  dt <- data.table::as.data.table(updates)
  validate_required_columns(dt, c("trip_id", "file_timestamp"), "updates")

  # Tolerate absent optional columns
  optional <- c(
    route_id = NA_character_,
    direction_id = NA_integer_,
    start_date = NA_character_,
    vehicle_id = NA_character_,
    stop_id = NA_character_,
    stop_sequence = NA_integer_,
    arrival_time = NA,
    arrival_delay = NA_real_,
    departure_time = NA,
    departure_delay = NA_real_,
    trip_schedule_relationship = NA_character_,
    stop_schedule_relationship = NA_character_
  )
  for (col in names(optional)) {
    if (!col %in% names(dt)) {
      dt[, (col) := optional[[col]]]
    }
  }
  for (col in c("arrival_time", "departure_time")) {
    if (!inherits(dt[[col]], "POSIXct")) {
      dt[, (col) := as.POSIXct(dt[[col]], tz = tz)]
    }
  }

  # Service date: explicit start_date, else the poll's local date
  dt[, service_date := parse_start_date(start_date)]
  dt[
    is.na(service_date),
    service_date := as.Date(file_timestamp, tz = tz)
  ]
  if (anyNA(dt$service_date)) {
    stop(
      "Could not determine a service date for some updates (missing both ",
      "'start_date' and 'file_timestamp').",
      call. = FALSE
    )
  }
  dt[, trip_schedule_relationship := as.character(trip_schedule_relationship)]
  dt[, stop_schedule_relationship := as.character(stop_schedule_relationship)]

  # Trip-level cancellations
  canceled_keys <- unique(dt[
    trip_schedule_relationship %in% c("CANCELED", "DELETED"),
    .(trip_id, service_date, route_id, direction_id, vehicle_id)
  ])
  dt <- dt[
    !trip_schedule_relationship %in% c("CANCELED", "DELETED")
  ]

  # NO_DATA carries no realized information
  n_nodata <- sum(dt$stop_schedule_relationship %in% "NO_DATA")
  if (n_nodata > 0L) {
    message("[INFO] Dropped ", n_nodata, " NO_DATA stop-time update(s).")
    dt <- dt[!stop_schedule_relationship %in% "NO_DATA"]
  }

  # Delay-only updates need scheduled times from a baseline
  dt[, delay_only_arr := is.na(arrival_time) & !is.na(arrival_delay)]
  dt[, delay_only_dep := is.na(departure_time) & !is.na(departure_delay)]
  if (any(dt$delay_only_arr | dt$delay_only_dep)) {
    if (is.null(baseline)) {
      stop(
        sum(dt$delay_only_arr | dt$delay_only_dep),
        " update(s) carry a delay without an absolute time; resolving them ",
        "requires the baseline feed's scheduled times. Pass 'baseline'.",
        call. = FALSE
      )
    }
    baseline <- read_gtfs_input(baseline)
    sched <- data.table::as.data.table(baseline$stop_times)
    validate_required_columns(
      sched,
      c("trip_id", "stop_id", "arrival_time", "departure_time"),
      "baseline stop_times"
    )
    sched <- sched[, .(
      trip_id = as.character(trip_id),
      stop_id = as.character(stop_id),
      sched_arr = as.character(arrival_time),
      sched_dep = as.character(departure_time)
    )]
    dt[, stop_id := as.character(stop_id)]
    dt <- merge(
      dt,
      sched,
      by.x = c("trip_id", "stop_id"),
      by.y = c("trip_id", "stop_id"),
      all.x = TRUE,
      sort = FALSE
    )
    clock_to_posix <- function(clock, service_date) {
      parts <- data.table::tstrsplit(clock, ":", fixed = TRUE)
      secs <- as.numeric(parts[[1]]) * 3600 +
        as.numeric(parts[[2]]) * 60 +
        as.numeric(parts[[3]])
      as.POSIXct(paste(as.character(service_date), "00:00:00"), tz = tz) + secs
    }
    dt[
      delay_only_arr & !is.na(sched_arr),
      arrival_time := clock_to_posix(sched_arr, service_date) + arrival_delay
    ]
    dt[
      delay_only_dep & !is.na(sched_dep),
      departure_time := clock_to_posix(sched_dep, service_date) + departure_delay
    ]
    unresolved <- dt[
      (delay_only_arr & is.na(arrival_time)) |
        (delay_only_dep & is.na(departure_time))
    ]
    if (nrow(unresolved) > 0L) {
      warning(
        nrow(unresolved),
        " delay-only update(s) reference trips/stops absent from the ",
        "baseline and were left without times.",
        call. = FALSE
      )
    }
    dt[, c("sched_arr", "sched_dep") := NULL]
  }
  dt[, c("delay_only_arr", "delay_only_dep") := NULL]

  # Reduce successive predictions: latest report per (trip, date, stop) wins
  dt[, stop_key := ifelse(
    !is.na(stop_id) & nzchar(as.character(stop_id)),
    as.character(stop_id),
    paste0("seq_", stop_sequence)
  )]
  data.table::setorderv(dt, "file_timestamp")
  reduced <- dt[, .SD[.N], by = .(trip_id, service_date, stop_key)]

  skipped <- reduced$stop_schedule_relationship %in% "SKIPPED"
  best_time <- data.table::fifelse(
    is.na(reduced$departure_time),
    reduced$arrival_time,
    reduced$departure_time
  )
  provenance <- data.table::fifelse(
    skipped,
    "skipped",
    data.table::fifelse(
      !is.na(best_time) & reduced$file_timestamp >= best_time,
      "observed",
      "predicted-last"
    )
  )

  # Blank out times on skipped stops by index assignment (fifelse with a bare
  # NA POSIXct would strip the timezone attribute)
  arr_out <- reduced$arrival_time
  dep_out <- reduced$departure_time
  arr_out[skipped] <- NA
  dep_out[skipped] <- NA

  out <- data.table::data.table(
    trip_ref = as.character(reduced$trip_id),
    route_ref = as.character(reduced$route_id),
    direction_id = as.integer(reduced$direction_id),
    service_date = reduced$service_date,
    stop_ref = as.character(reduced$stop_id),
    stop_sequence = as.integer(reduced$stop_sequence),
    arrival_time = arr_out,
    departure_time = dep_out,
    provenance = provenance,
    vehicle_ref = as.character(reduced$vehicle_id),
    source = "trip_updates"
  )

  if (nrow(canceled_keys) > 0L) {
    out <- rbind(
      out,
      data.table::data.table(
        trip_ref = as.character(canceled_keys$trip_id),
        route_ref = as.character(canceled_keys$route_id),
        direction_id = as.integer(canceled_keys$direction_id),
        service_date = canceled_keys$service_date,
        stop_ref = NA_character_,
        stop_sequence = NA_integer_,
        arrival_time = as.POSIXct(NA),
        departure_time = as.POSIXct(NA),
        provenance = "canceled",
        vehicle_ref = as.character(canceled_keys$vehicle_id),
        source = "trip_updates"
      )
    )
  }

  validate_events(out)
}