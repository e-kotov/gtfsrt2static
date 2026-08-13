# Integration test: run the MobilityData GTFS validator against an assembled
# feed and assert it has no ERROR-severity notices. It is heavy (needs Java
# and the validator jar), so it is opt-in: set GTFSRT2STATIC_RUN_VALIDATOR=1 to
# run it. It self-skips when Java, network, or gtfstools are unavailable, so
# ordinary `R CMD check` never runs it.
#
# Point GTFSRT2STATIC_VALIDATOR_JAR at an already-downloaded jar to reuse it
# instead of fetching one. Batch nodes routinely have enough network to satisfy
# skip_if_offline() while still being unable to reach GitHub, which made the
# download the harness's least reliable step; a cached jar also removes the
# network requirement entirely.

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

skip_unless_validator_enabled <- function() {
  skip_on_cran()
  if (!nzchar(Sys.getenv("GTFSRT2STATIC_RUN_VALIDATOR"))) {
    skip(paste(
      "Set GTFSRT2STATIC_RUN_VALIDATOR=1 to run the MobilityData validator",
      "harness (needs Java + network + gtfstools)."
    ))
  }
  skip_if_not_installed("gtfstools")
  skip_if_not_installed("jsonlite")
  # Only the download needs the network; a cached jar makes the harness usable
  # on an isolated node.
  if (!nzchar(cached_validator_jar())) {
    skip_if_offline()
  }
  if (!resolve_java()) {
    skip("No Java runtime found; MobilityData validator cannot run.")
  }
}

#' Path to a caller-supplied validator jar, or "" when none is configured.
cached_validator_jar <- function() {
  path <- Sys.getenv("GTFSRT2STATIC_VALIDATOR_JAR")
  if (!nzchar(path)) {
    return("")
  }
  if (!file.exists(path)) {
    stop(
      "GTFSRT2STATIC_VALIDATOR_JAR is set but names no existing file: ",
      path,
      call. = FALSE
    )
  }
  path
}

# Resolved once per session: downloading the same jar for each scenario wasted
# the slowest step of the harness four times over.
validator_jar_cache <- new.env(parent = emptyenv())

validator_jar <- function() {
  cached <- cached_validator_jar()
  if (nzchar(cached)) {
    return(cached)
  }
  if (is.null(validator_jar_cache$path)) {
    vdir <- tempfile()
    dir.create(vdir)
    gtfstools::download_validator(vdir)
    validator_jar_cache$path <-
      list.files(vdir, pattern = "\\.jar$", full.names = TRUE)[1]
  }
  validator_jar_cache$path
}

validator_errors <- function(notices) {
  if (is.null(notices) || length(notices) == 0L) {
    return(character())
  }
  as.character(notices$code[notices$severity == "ERROR"])
}

validator_notices <- function(feed) {
  zip <- tempfile(fileext = ".zip")
  gtfsio::export_gtfs(feed, zip)
  jar <- validator_jar()
  out <- tempfile()
  old <- options(
    parallelly.availableCores.methods = c("Slurm", "system", "fallback"),
    parallelly.availableCores.fallback = 1L
  )
  on.exit(options(old), add = TRUE)
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
  skip_unless_validator_enabled()

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
  events <- rt2s_events_from_stop_times(st, route_ref = "R1", shape_ref_prefix = "SHP_")

  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1", "SHP_1", "SHP_2", "SHP_2", "SHP_2"),
    shape_pt_lat = c(40.71, 40.72, 40.73, 40.73, 40.72, 40.71),
    shape_pt_lon = c(-74.01, -74.02, -74.03, -74.03, -74.02, -74.01),
    shape_pt_sequence = c(1L, 2L, 3L, 1L, 2L, 3L),
    shape_dist_traveled = c(0, 100, 200, 0, 100, 200)
  )

  feed <- rt2s_scaffold(
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
  errors <- validator_errors(notices)

  expect_identical(
    errors,
    character(),
    info = paste("Validator ERROR notices:", paste(errors, collapse = ", "))
  )

  # Overnight stop must be encoded past 24:00:00, not wrapped to 00:05:00.
  expect_true(any(grepl("^24:", feed$stop_times$arrival_time)))
})

test_that("frequency-based scenario feeds have no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "Test Transit", url = "https://example.org", timezone = "UTC"),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      stop_lat = c(40.71, 40.72, 40.73),
      stop_lon = c(-74.01, -74.02, -74.03)
    ),
    strict = TRUE
  )
  for (s in names(feeds)) {
    notices <- validator_notices(feeds[[s]])
    errors <- validator_errors(notices)
    expect_identical(
      errors,
      character(),
      info = paste0(s, " validator ERRORs: ", paste(errors, collapse = ", "))
    )
  }
})

test_that("baseline-anchored frequency feeds have no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  # Four scenarios over two windows, the shape the anchored mode exists for:
  # a planned "scheduled" feed at published running times and frequency, plus
  # three observed ones. Agency, stops and routes are inherited from the
  # baseline, so nothing is passed explicitly here - which is also what makes
  # this a test of the inheritance path, not just of the arithmetic.
  windows <- list(early = c("06:00", "06:15"), later = c("06:15", "09:00"))
  baseline <- make_baseline_freq()
  scenarios <- c("scheduled", "structural", "median", "reliable")
  ratios <- c(scheduled = 1, structural = 0.87, median = 1.05, reliable = 1.31)

  scaling <- do.call(rbind, lapply(names(windows), function(w) {
    grid <- make_scaling(scenarios, 1, window = w)
    grid$ratio <- as.numeric(ratios[grid$scenario])
    grid
  }))
  sched <- rt2s_baseline_headways(baseline, windows = windows)
  sched$scenario <- "scheduled"

  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = windows,
    quantiles = list(
      scheduled = c(headway = 0.50),
      structural = c(travel = 0.05, headway = 0.50),
      median = c(travel = 0.50, headway = 0.50),
      reliable = c(travel = 0.95, headway = 0.95)
    ),
    baseline = baseline,
    pattern_source = "baseline",
    scaling = scaling,
    headways = sched,
    strict = TRUE
  )
  expect_identical(names(feeds), scenarios)

  # The invariant the whole design rests on: every scenario emits the same
  # trips over the same stops in the same order, so a contrast between two
  # feeds is a contrast in service levels and nothing else.
  ref <- feeds$median
  for (s in scenarios) {
    expect_identical(sort(feeds[[s]]$trips$trip_id), sort(ref$trips$trip_id))
    expect_identical(
      feeds[[s]]$stop_times[order(trip_id, stop_sequence)]$stop_id,
      ref$stop_times[order(trip_id, stop_sequence)]$stop_id
    )
  }

  for (s in scenarios) {
    notices <- validator_notices(feeds[[s]])
    errors <- validator_errors(notices)
    expect_identical(
      errors,
      character(),
      info = paste0(
        "Baseline-anchored ", s, " validator ERRORs: ",
        paste(errors, collapse = ", ")
      )
    )
  }
})

test_that("a mixed frequency + exact-time feed has no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  # The load-bearing question for extra_trips=: GTFS says only trips listed in
  # frequencies.txt are frequency-based and the rest are exact-time, so a feed
  # may mix the two - but only gtfs-validator can confirm that the feed this
  # package writes for that mix is actually spec-valid. Nothing cheaper answers
  # it, because the defect would be a referential or field-level one across two
  # differently-timed trip kinds, not an arithmetic slip.
  windows <- list(am_peak = c("06:00", "09:00"))
  extra <- make_extra_trips(c("X_exact_1", "X_exact_2"))

  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = windows,
    agency = list(name = "Test Transit", url = "https://example.org", timezone = "UTC"),
    stops = make_stops_with_extra(),
    route_type = 3L,
    strict = TRUE,
    # deliberately asymmetric: the counts differ across scenarios, which the
    # contract allows, and one scenario has none at all
    extra_trips = list(
      median = extra,
      reliable = make_extra_trips("X_exact_1")
    )
  )

  expect_setequal(
    feeds$median$trips$trip_id,
    c("R1_0_am_peak", "X_exact_1", "X_exact_2")
  )
  # the mix itself: a frequency row for the generated trip and none for the
  # exact-time ones
  expect_identical(feeds$median$frequencies$trip_id, "R1_0_am_peak")
  expect_false(any(extra$trips$trip_id %in% feeds$median$frequencies$trip_id))
  # S9 is reached only by an extra trip, so referential integrity here is the
  # widened stop_ids derivation working
  expect_true("S9" %in% feeds$median$stops$stop_id)

  for (s in names(feeds)) {
    notices <- validator_notices(feeds[[s]])
    errors <- validator_errors(notices)
    expect_identical(
      errors,
      character(),
      info = paste0(
        "Mixed-feed ", s, " validator ERRORs: ", paste(errors, collapse = ", ")
      )
    )
  }
})

test_that("passage-based frequency feeds have no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  feed <- suppressWarnings(
    rt2s_frequencies(
      make_events_shared_trip_ref_passages(),
      windows = list(am = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      agency = list(name = "Test Transit", url = "https://example.org", timezone = "UTC"),
      stops = data.frame(
        stop_id = c("S1", "S2", "S3"),
        stop_lat = c(40.71, 40.72, 40.73),
        stop_lon = c(-74.01, -74.02, -74.03)
      ),
      route_type = 3L,
      strict = TRUE,
      headway_method = "passage",
      reference_stops = "S1"
    )
  )$median

  notices <- validator_notices(feed)
  errors <- validator_errors(notices)
  expect_identical(
    errors,
    character(),
    info = paste("Passage validator ERROR notices:", paste(errors, collapse = ", "))
  )
})

test_that("a headway_groups-driven feed has no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  # FR-7's regime end to end: no observations at all, so candidacy, the ratio,
  # the headway and the calendar span are every one of them caller-supplied.
  # The load-bearing question is whether such a feed is a valid GTFS feed;
  # nothing cheaper answers it.
  windows <- list(early = c("06:00", "06:15"), later = c("06:15", "09:00"))
  baseline <- make_baseline_freq()
  scenarios <- c("scheduled", "median")

  feeds <- rt2s_frequencies(
    events = NULL,
    windows = windows,
    quantiles = list(
      scheduled = c(headway = 0.50),
      median = c(headway = 0.50)
    ),
    baseline = baseline,
    pattern_source = "baseline",
    service_dates = rt2s_baseline_service_dates(baseline, "weekday"),
    scaling = do.call(rbind, lapply(names(windows), function(w) {
      grid <- make_scaling(scenarios, 1, window = w)
      grid$ratio <- ifelse(grid$scenario == "median", 1.05, 1)
      grid
    })),
    headways = do.call(rbind, lapply(names(windows), function(w) {
      make_headway_overrides(scenarios, c(900L, 720L), window = w)
    })),
    headway_groups = make_headway_groups(window = names(windows)),
    strict = TRUE
  )
  expect_identical(names(feeds), scenarios)
  # both windows are emitted even though no event ever fell in either
  for (s in scenarios) {
    expect_setequal(feeds[[s]]$trips$trip_id, c("R1_0_early", "R1_0_later"))
  }

  for (s in scenarios) {
    notices <- validator_notices(feeds[[s]])
    errors <- validator_errors(notices)
    expect_identical(
      errors,
      character(),
      info = paste0(
        "headway_groups ", s, " validator ERRORs: ",
        paste(errors, collapse = ", ")
      )
    )
  }
})

test_that("a feed with calendar_dates gaps has no ERROR-level validator notices", {
  skip_unless_validator_enabled()

  # A span with holes in it: 2026-03-11..03-31 with 03-18 and 03-19 absent, the
  # shape a working set of daily files takes when two days are missing. The two
  # exception rows must be a valid calendar_dates.txt, not just a plausible one.
  span <- seq(as.Date("2026-03-11"), as.Date("2026-03-31"), by = "day")
  dates <- span[!span %in% as.Date(c("2026-03-18", "2026-03-19"))]

  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      stop_lat = c(40.71, 40.72, 40.73),
      stop_lon = c(-74.01, -74.02, -74.03)
    ),
    service_dates = dates,
    strict = TRUE
  )
  cd <- feeds$median$calendar_dates
  expect_identical(cd$date, c(20260318L, 20260319L))

  notices <- validator_notices(feeds$median)
  errors <- validator_errors(notices)
  expect_identical(
    errors,
    character(),
    info = paste0(
      "calendar_dates validator ERRORs: ",
      paste(errors, collapse = ", ")
    )
  )
})
