#' Format Absolute Times as GTFS Clock Strings (Allowing >24:00:00)
#'
#' Converts absolute POSIXct times to "HH:MM:SS" strings relative to the
#' midnight of the trip's service date in the given timezone. Post-midnight
#' stops of a trip attributed to the previous service date correctly render
#' as hours >= 24, per the GTFS specification.
#'
#' @param time POSIXct vector.
#' @param service_date Date vector (recycled).
#' @param tz Timezone of the service day.
#' @return Character vector of clock strings; NA in, NA out.
#' @noRd
gtfs_clock <- function(time, service_date, tz) {
  midnight <- as.POSIXct(paste(as.character(service_date), "00:00:00"), tz = tz)
  secs <- round(as.numeric(difftime(time, midnight, units = "secs")))
  out <- rep(NA_character_, length(secs))
  ok <- !is.na(secs)
  if (any(ok & secs < 0)) {
    stop(
      sum(ok & secs < 0),
      " time(s) fall before the midnight of their service date; check the ",
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