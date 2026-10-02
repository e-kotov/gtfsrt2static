#' Origin of a GTFS Service Day
#'
#' GTFS clock times count from "noon minus 12h" of the service day in the
#' agency timezone. That is midnight on most days, but one hour off it on the
#' days a daylight-saving change happens, where counting from midnight would
#' put every later time an hour out.
#'
#' @param service_date Date vector.
#' @param tz Timezone of the service day.
#' @return POSIXct vector.
#' @noRd
gtfs_day_origin <- function(service_date, tz) {
  if (length(service_date) == 0L) {
    # paste(character(0), "12:00:00") would recycle to " 12:00:00"
    return(as.POSIXct(numeric(0), tz = tz))
  }
  as.POSIXct(paste(as.character(service_date), "12:00:00"), tz = tz) - 43200
}

#' GTFS Service Date of an Absolute Time
#'
#' The local date of \code{time} in \code{tz}, except where \code{time}
#' falls before that date's GTFS origin (the first hour of a daylight-saving
#' fall-back day): such a time can only be written as a clock past 24:00 on
#' the day before, so it belongs to that day.
#'
#' @param time POSIXct vector.
#' @param tz Timezone of the service day.
#' @return Date vector.
#' @noRd
gtfs_service_date <- function(time, tz) {
  d <- as.Date(format(time, "%Y-%m-%d", tz = tz))
  days <- unique(d[!is.na(d)])
  origin <- gtfs_day_origin(days, tz)[match(d, days)]
  before <- !is.na(time) & time < origin
  d[before] <- d[before] - 1L
  d
}

#' Format Absolute Times as GTFS Clock Strings (Allowing >24:00:00)
#'
#' Converts absolute POSIXct times to "HH:MM:SS" strings relative to the
#' origin of the trip's service date (noon minus 12h, see
#' \code{gtfs_day_origin()}) in the given timezone. Post-midnight
#' stops of a trip attributed to the previous service date correctly render
#' as hours >= 24, per the GTFS specification.
#'
#' @param time POSIXct vector.
#' @param service_date Date vector (recycled).
#' @param tz Timezone of the service day.
#' @return Character vector of clock strings; NA in, NA out.
#' @noRd
gtfs_clock <- function(time, service_date, tz) {
  secs <- round(as.numeric(difftime(
    time,
    gtfs_day_origin(service_date, tz),
    units = "secs"
  )))
  out <- rep(NA_character_, length(secs))
  ok <- !is.na(secs)
  if (any(ok & secs < 0)) {
    stop(
      sum(ok & secs < 0),
      " time(s) fall before the start of their service date; check the ",
      "'tz' argument and service date attribution.",
      call. = FALSE
    )
  }
  h <- secs %/% 3600L
  m <- (secs %% 3600L) %/% 60L
  s <- secs %% 60L
  out[ok] <- sprintf("%02d:%02d:%02d", h[ok], m[ok], s[ok])
  out
}

#' Integer YYYYMMDD from a Date
#' @noRd
yyyymmdd <- function(d) {
  as.integer(format(as.Date(d), "%Y%m%d"))
}

#' Format non-negative integer seconds as a GTFS clock string
#'
#' Hours of 24 or more are allowed (88200 becomes "24:30:00"). Used for
#' frequency-trip stop_times, whose times are offsets from trip start
#' (00:00:00).
#'
#' NA or negative input is a programming error, not a value to encode: silently
#' producing "NA:NA:NA" (or a negative clock) would ship an invalid GTFS field.
#' It signals that unserved stops (NA offsets) leaked into a representative
#' pattern, so fail loudly instead.
#' @noRd
secs_to_clock <- function(secs) {
  secs <- as.integer(round(secs))
  if (anyNA(secs)) {
    stop(
      sum(is.na(secs)),
      " time offset(s) are NA and cannot be encoded as a GTFS clock string ",
      "(would be \"NA:NA:NA\"). This usually means unserved (skipped/canceled) ",
      "stops entered a representative pattern.",
      call. = FALSE
    )
  }
  if (any(secs < 0L)) {
    stop(
      sum(secs < 0L),
      " time offset(s) are negative and cannot be encoded as a GTFS clock ",
      "string.",
      call. = FALSE
    )
  }
  h <- secs %/% 3600L
  m <- (secs %% 3600L) %/% 60L
  s <- secs %% 60L
  sprintf("%02d:%02d:%02d", h, m, s)
}

#' Parse GTFS-RT start_date (YYYYMMDD) Values to Date
#' @noRd
parse_start_date <- function(x) {
  as.Date(as.character(x), format = "%Y%m%d")
}

validate_required_columns <- function(dt, required, name) {
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L) {
    stop(
      "Missing required columns in ",
      name,
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

#' Coerce a GTFS Input to a Feed Object
#' @noRd
read_gtfs_input <- function(gtfs) {
  if (is.character(gtfs) && length(gtfs) == 1L) {
    gtfs <- gtfsio::import_gtfs(gtfs)
  }
  if (!is.list(gtfs) || is.null(names(gtfs))) {
    stop(
      "'baseline' must be a GTFS feed object (named list of data.frames) or ",
      "a path to a GTFS zip file.",
      call. = FALSE
    )
  }
  gtfs
}

#' Build a gtfsio-Convention Feed Object from a Named List of data.tables
#' @noRd
as_gtfs_object <- function(x) {
  gtfsio::new_gtfs(x)
}