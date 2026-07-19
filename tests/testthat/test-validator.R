# Integration test: run the MobilityData GTFS validator against an assembled
# feed and assert it has no ERROR-severity notices. It is heavy (needs Java
# and downloads the validator jar), so it is opt-in: set
# GTFSRT2STATIC_RUN_VALIDATOR=1 to run it. It self-skips when Java, network,
# or gtfstools are unavailable, so ordinary `R CMD check` never runs it.

resolve_java <- function() {
  if (nzchar(Sys.which("java"))) {
    return(TRUE)
  }
  # Common Homebrew OpenJDK locations, not always on PATH.
  cands <- c(
    "/opt/homebrew/opt/openjdk/bin",
    "/usr/local/opt/openjdk/bin",
    file.path(Sys.getenv("JAVA_HOME"), "bin")
  )
  hit <- cands[nzchar(cands) & file.exists(file.path(cands, "java"))]
  if (length(hit) > 0L) {
    Sys.setenv(PATH = paste(hit[1], Sys.getenv("PATH"), sep = .Platform$path.sep))
    return(nzchar(Sys.which("java")))
  }
  FALSE
}

validator_notices <- function(feed) {
  zip <- tempfile(fileext = ".zip")
  gtfsio::export_gtfs(feed, zip)
  vdir <- tempfile()
  dir.create(vdir)
  gtfstools::download_validator(vdir)
  jar <- list.files(vdir, pattern = "\\.jar$", full.names = TRUE)[1]
  out <- tempfile()
  gtfstools::validate_gtfs(
    zip,
    output_path = out,
    validator_path = jar,
    quiet = TRUE
  )
  report <- jsonlite::fromJSON(file.path(out, "report.json"))
  report$notices
}

test_that("assembled feeds have no ERROR-level MobilityData validator notices", {
  skip_on_cran()
  if (!nzchar(Sys.getenv("GTFSRT2STATIC_RUN_VALIDATOR"))) {
    skip(paste(
      "Set GTFSRT2STATIC_RUN_VALIDATOR=1 to run the MobilityData validator",
      "harness (needs Java + network + gtfstools)."
    ))
  }
  skip_if_not_installed("gtfstools")
  skip_if_not_installed("jsonlite")
  skip_if_offline()
  if (!resolve_java()) {
    skip("No Java runtime found; MobilityData validator cannot run.")
  }

  tz <- "America/New_York"
  # An overnight trip (crosses midnight -> exercises >24:00:00 encoding) plus a
  # morning trip, both carrying official trip ids.
  st <- data.frame(
    trip_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    vehicle_id = "v1",
    direction = c(1L, 1L, 1L, 2L, 2L, 2L),
    stop_id = c("S1", "S2", "S3", "S3", "S2", "S1"),
    arrival_time = as.POSIXct(c(
      "2026-07-14 23:50:00", "2026-07-14 23:57:00", "2026-07-15 00:05:00",
      "2026-07-15 06:10:00", "2026-07-15 06:17:00", "2026-07-15 06:25:00"
    ), tz = tz),
    departure_time = as.POSIXct(c(
      "2026-07-14 23:50:30", "2026-07-14 23:57:30", "2026-07-15 00:05:30",
      "2026-07-15 06:10:30", "2026-07-15 06:17:30", "2026-07-15 06:25:30"
    ), tz = tz),
    provided_trip_id = c("T1", "T1", "T1", "T2", "T2", "T2")
  )
  events <- snapshot_from_stop_times(st, route_ref = "R1", shape_ref_prefix = "SHP_")

  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1", "SHP_1", "SHP_2", "SHP_2", "SHP_2"),
    shape_pt_lat = c(40.71, 40.72, 40.73, 40.73, 40.72, 40.71),
    shape_pt_lon = c(-74.01, -74.02, -74.03, -74.03, -74.02, -74.01),
    shape_pt_sequence = c(1L, 2L, 3L, 1L, 2L, 3L),
    shape_dist_traveled = c(0, 100, 200, 0, 100, 200)
  )

  feed <- snapshot_scaffold(
    events,
    agency = list(
      name = "Test Transit",
      url = "https://example.org",
      timezone = tz
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      stop_lat = c(40.71, 40.72, 40.73),
      stop_lon = c(-74.01, -74.02, -74.03)
    ),
    shapes = shapes,
    route_type = 3L,
    tz = tz,
    feed_contact_url = "https://example.org/contact",
    strict = TRUE
  )

  # trips link to shapes (referential integrity checked by validator too)
  expect_true(all(feed$trips$shape_id %in% feed$shapes$shape_id))

  notices <- validator_notices(feed)
  errors <- if (is.null(notices) || length(notices) == 0L) {
    character()
  } else {
    as.character(notices$code[notices$severity == "ERROR"])
  }

  expect_identical(
    errors,
    character(),
    info = paste("Validator ERROR notices:", paste(errors, collapse = ", "))
  )

  # Overnight stop must be encoded past 24:00:00, not wrapped to 00:05:00.
  expect_true(any(grepl("^24:", feed$stop_times$arrival_time)))
})
