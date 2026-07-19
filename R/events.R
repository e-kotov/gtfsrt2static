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
#'   \item{shape_ref}{Shape identity linking the trip to \code{shapes.txt},
#'     NA when unknown. Set by \code{snapshot_from_stop_times(shape_ref_prefix=)}
#'     to match the ids produced by \code{gps2gtfs::g2g_shapes_from_trips()};
#'     the assembler writes it to \code{trips.shape_id}.}
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
  "shape_ref",
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
#'   \code{vehicle_id}, \code{direction} (1/2), \code{stop_id}, and
#'   \code{arrival_time}/\code{departure_time} as absolute \code{POSIXct}
#'   values (gps2gtfs >= 0.2.0). Legacy "HH:MM:SS" strings are also
#'   accepted; they additionally require the \code{date} column they are
#'   interpreted within (and cannot represent a trip whose stop visits fall
#'   on the wrong side of midnight unambiguously - prefer POSIXct).
#' @param tz Timezone the legacy "HH:MM:SS" strings refer to (the timezone
#'   the GPS data was cleaned in). Ignored for POSIXct input, which carries
#'   its own timezone. Default "UTC".
#' @param route_ref Optional route identity to stamp on all events (gps2gtfs
#'   models one route per run). Default NA.
#' @param source Provenance source label: \code{"positions"} (GTFS-RT
#'   Vehicle Positions, default) or \code{"gps"} (raw AVL/logger data).
#' @param trip_ref_prefix Prefix for synthetic trip identities. When a trip
#'   has no official id (see \code{trip_id_col}), its trip ref is
#'   \code{<prefix><yyyymmdd>_<trip_id>} so synthetic refs stay unique across
#'   service dates. Default \code{"g2g_"}.
#' @param trip_id_col Optional name of a column carrying the official trip
#'   identity (e.g. \code{"provided_trip_id"}, as emitted by gps2gtfs's fast
#'   path from a GTFS-Realtime \code{trip_id}). Where present and non-missing,
#'   its value becomes the \code{trip_ref} verbatim, so baseline-mode
#'   assembly can match observed trips to the baseline \code{trips.txt};
#'   trips lacking it fall back to a synthetic ref. \code{NULL} (default)
#'   auto-detects a \code{provided_trip_id} column and uses it when present.
#' @details Each trip is attributed to one service day: the day (in the
#'   times' timezone) of its first observed stop. Post-midnight stops keep
#'   their trip's service date, so an overnight trip yields a single
#'   \code{trip_ref} and the assembler renders its late stops as
#'   \code{>24:00:00} clock times.
#' @param shape_ref_prefix Optional character prefix for populating
#'   \code{shape_ref} from the internal \code{trip_id}, as
#'   \code{<prefix><trip_id>}. Set it to the \code{shape_id_prefix} used with
#'   \code{gps2gtfs::g2g_shapes_from_trips()} (default there \code{"SHP_"}) so
#'   the assembler can link \code{trips.shape_id} to those shapes. \code{NULL}
#'   (default) leaves \code{shape_ref} as NA (no shape linkage).
#' @return A validated observed stop events data.table.
#' @export
snapshot_from_stop_times <- function(
  stop_times,
  tz = "UTC",
  route_ref = NA_character_,
  source = c("positions", "gps"),
  trip_ref_prefix = "g2g_",
  trip_id_col = NULL,
  shape_ref_prefix = NULL
) {
  if (
    !is.null(shape_ref_prefix) &&
      (length(shape_ref_prefix) != 1L || !is.character(shape_ref_prefix))
  ) {
    stop("'shape_ref_prefix' must be a single string or NULL.", call. = FALSE)
  }
  source <- match.arg(source)
  dt <- data.table::as.data.table(stop_times)
  validate_required_columns(
    dt,
    c(
      "trip_id",
      "vehicle_id",
      "direction",
      "stop_id",
      "arrival_time",
      "departure_time"
    ),
    "stop_times"
  )

  # Resolve the official-trip-id column: explicit arg, else auto-detect the
  # provided_trip_id column gps2gtfs's fast path emits.
  if (is.null(trip_id_col) && "provided_trip_id" %in% names(dt)) {
    trip_id_col <- "provided_trip_id"
  }
  if (!is.null(trip_id_col)) {
    validate_required_columns(dt, trip_id_col, "stop_times")
    official_id <- as.character(dt[[trip_id_col]])
  } else {
    official_id <- rep(NA_character_, nrow(dt))
  }

  if (inherits(dt$arrival_time, "POSIXct")) {
    if (!inherits(dt$departure_time, "POSIXct")) {
      stop(
        "'arrival_time' and 'departure_time' must both be POSIXct ",
        "(or both \"HH:MM:SS\" strings).",
        call. = FALSE
      )
    }
    arrival <- dt$arrival_time
    departure <- dt$departure_time
  } else {
    # Legacy gps2gtfs (< 0.2.0) encoding: "HH:MM:SS" within the calendar
    # date of the stop visit. A visit spanning midnight makes the departure
    # string wrap behind the arrival; repair by shifting it one day.
    validate_required_columns(dt, "date", "stop_times")
    arrival <- as.POSIXct(paste(dt$date, dt$arrival_time), tz = tz)
    departure <- as.POSIXct(paste(dt$date, dt$departure_time), tz = tz)
    wrapped <- !is.na(arrival) & !is.na(departure) & departure < arrival
    departure[wrapped] <- departure[wrapped] + 86400
  }
  if (anyNA(arrival) || anyNA(departure)) {
    stop(
      "stop_times 'arrival_time'/'departure_time' must not contain missing ",
      "or unparseable values.",
      call. = FALSE
    )
  }

  shape_ref <- if (is.null(shape_ref_prefix)) {
    rep(NA_character_, nrow(dt))
  } else {
    paste0(shape_ref_prefix, as.character(dt$trip_id))
  }

  out <- data.table::data.table(
    trip_key = as.character(dt$trip_id),
    official_id = official_id,
    route_ref = rep(as.character(route_ref), nrow(dt)),
    shape_ref = shape_ref,
    direction_id = as.integer(dt$direction) - 1L,
    stop_ref = as.character(dt$stop_id),
    stop_sequence = NA_integer_,
    arrival_time = arrival,
    departure_time = departure,
    provenance = rep("observed", nrow(dt)),
    vehicle_ref = as.character(dt$vehicle_id),
    source = rep(source, nrow(dt))
  )
  # One service day per trip - the day of its first observed stop, in the
  # times' own timezone - so a midnight-crossing trip keeps a single
  # trip_ref instead of being split across two service dates.
  out[,
    service_date := as.Date(format(min(arrival_time), "%Y-%m-%d")),
    by = trip_key
  ]
  # trip_ref: the official id verbatim where supplied (so baseline-mode
  # assembly can match the baseline trips.txt), else a synthetic ref that
  # stays unique across service dates.
  has_official <- !is.na(out$official_id) & nzchar(trimws(out$official_id))
  out[, trip_ref := paste0(
    trip_ref_prefix,
    format(service_date, "%Y%m%d"),
    "_",
    trip_key
  )]
  out[has_official, trip_ref := official_id]
  out[, c("trip_key", "official_id") := NULL]
  data.table::setorderv(out, c("trip_ref", "arrival_time"))
  out[, stop_sequence := seq_len(.N), by = trip_ref]
  validate_events(out)
}

#' Fill Missing trip_id from the GTFS-RT TripDescriptor
#'
#' Where \code{trip_id} is absent, build a stable synthetic identity from the
#' descriptor fields (route_id, direction_id, start_date, start_time). Rows
#' that already have a trip_id keep it. A row with no usable descriptor at all
#' is left NA (it has no recoverable trip identity).
#' @param dt A data.table with a \code{trip_id} column and any of the
#'   descriptor columns.
#' @param prefix Prefix for synthesized ids (marks them as descriptor-derived).
#' @return \code{dt} with \code{trip_id} filled where it was missing.
#' @noRd
synthesize_trip_id <- function(dt, prefix = "rtd_") {
  tid <- as.character(dt$trip_id)
  missing_tid <- is.na(tid) | !nzchar(trimws(tid))
  if (!any(missing_tid)) {
    return(dt)
  }
  desc_cols <- intersect(
    c("route_id", "direction_id", "start_date", "start_time"),
    names(dt)
  )
  parts <- lapply(desc_cols, function(col) {
    v <- as.character(dt[[col]][missing_tid])
    v[is.na(v)] <- ""
    v
  })
  if (length(parts) == 0L || all(vapply(parts, function(v) all(!nzchar(v)), logical(1)))) {
    warning(
      sum(missing_tid),
      " update(s) have neither trip_id nor any TripDescriptor field ",
      "(route_id/direction_id/start_date/start_time); their trip identity ",
      "cannot be recovered.",
      call. = FALSE
    )
    return(dt)
  }
  synthetic <- paste0(prefix, do.call(paste, c(parts, sep = "_")))
  dt <- data.table::copy(dt)
  dt[which(missing_tid), trip_id := synthetic]
  message(
    "[INFO] ",
    sum(missing_tid),
    " update(s) had no trip_id; identified their trip by the TripDescriptor (",
    paste(desc_cols, collapse = ", "),
    ")."
  )
  dt
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
#'   \code{start_date}, \code{vehicle_id}, \code{file_timestamp}. When
#'   \code{trip_id} is absent (some producers, e.g. HSL, identify trips only by
#'   the GTFS-RT TripDescriptor), a stable identity is synthesized from
#'   \code{route_id}, \code{direction_id}, \code{start_date}, and
#'   \code{start_time} so predictions still group per operated trip.
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
    start_time = NA_character_,
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

  # GTFS-RT identifies a trip by either trip_id or the TripDescriptor
  # (route_id, direction_id, start_time, start_date). Producers such as HSL
  # populate only the latter, leaving trip_id NA. Synthesize a stable trip
  # identity from the descriptor where trip_id is absent, so the reduction
  # groups per operated trip instead of collapsing all NA-trip_id rows into
  # one. Runs before cancellation keys and reduction, which key on trip_id.
  dt <- synthesize_trip_id(dt)

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
    shape_ref = NA_character_,
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
        shape_ref = NA_character_,
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