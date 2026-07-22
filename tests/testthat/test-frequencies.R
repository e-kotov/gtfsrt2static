# Scenario numbers are hand-worked in
# private/gtfsrt2static-phase0-groundtruth.md (§5). Windowing/quantiles reuse
# the summarise module already tested in test-summarise.R.

clock_secs <- function(x) {
  p <- data.table::tstrsplit(x, ":", fixed = TRUE)
  as.integer(p[[1]]) * 3600L + as.integer(p[[2]]) * 60L + as.integer(p[[3]])
}

freq_stops <- function() {
  data.frame(
    stop_id = c("S1", "S2", "S3"),
    stop_lat = c(40.71, 40.72, 40.73),
    stop_lon = c(-74.01, -74.02, -74.03)
  )
}

test_that("snapshot_frequencies emits one feed per scenario with exact numbers", {
  feeds <- snapshot_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  expect_identical(names(feeds), c("structural", "median", "reliable"))

  # headways: structural p05=612, median p50=720, reliable p95=1044
  expect_identical(feeds$structural$frequencies$headway_secs, 612L)
  expect_identical(feeds$median$frequencies$headway_secs, 720L)
  expect_identical(feeds$reliable$frequencies$headway_secs, 1044L)
  # exact_times default 0; window bounds normalised to HH:MM:SS
  expect_identical(feeds$median$frequencies$exact_times, 0L)
  expect_identical(feeds$median$frequencies$start_time, "06:00:00")
  expect_identical(feeds$median$frequencies$end_time, "09:00:00")

  # representative stop_times: first stop anchored at 0; median S2=310, S3=625
  med <- feeds$median$stop_times[order(stop_sequence)]
  expect_identical(clock_secs(med$arrival_time), c(0L, 310L, 625L))
  expect_identical(clock_secs(med$departure_time), c(30L, 340L, 655L))
  # structural (p05): S2=283, S3=583 ; reliable (p95): S2=354, S3=692
  expect_identical(
    clock_secs(feeds$structural$stop_times[order(stop_sequence)]$arrival_time),
    c(0L, 283L, 583L)
  )
  expect_identical(
    clock_secs(feeds$reliable$stop_times[order(stop_sequence)]$arrival_time),
    c(0L, 354L, 692L)
  )
})

test_that("scenarios order as reliable >= median >= structural", {
  feeds <- snapshot_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  last <- function(f) {
    st <- f$stop_times[order(stop_sequence)]
    clock_secs(st$arrival_time[nrow(st)])
  }
  expect_gt(last(feeds$reliable), last(feeds$median))
  expect_gt(last(feeds$median), last(feeds$structural))
  # reliable headway longer than structural
  expect_gt(feeds$reliable$frequencies$headway_secs, feeds$structural$frequencies$headway_secs)
})

test_that("feed is referentially consistent and gtfsio-shaped", {
  feeds <- snapshot_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  f <- feeds$median
  expect_s3_class(f, "gtfs")
  expect_true(all(f$stop_times$stop_id %in% f$stops$stop_id))
  expect_true(all(f$stop_times$trip_id %in% f$trips$trip_id))
  expect_true(all(f$frequencies$trip_id %in% f$trips$trip_id))
  expect_true(all(f$trips$route_id %in% f$routes$route_id))
  expect_true(all(f$trips$service_id %in% f$calendar$service_id))
  # calendar active on Tuesday only (the fixture's single weekday)
  expect_identical(f$calendar$tuesday, 1L)
  expect_identical(f$calendar$sunday, 0L)
  # publishable: full agency + coords -> no blockers
  expect_true(snapshot_publishable(f)$publishable)
})

test_that("monotonicity guard forces non-decreasing representative times", {
  # Ordered by median offset the p05 quantile is non-monotone (a downstream
  # stop has a smaller free-flow travel than an upstream one); the guard clamps.
  ev <- make_events_from_offsets(
    route = "Rg", direction_id = 0L, date = "2026-07-14",
    starts = c(P1 = "07:00:00", P2 = "07:30:00"),
    stops = c("S0", "Y", "X"),
    offsets = list(P1 = c(0, 200, 100), P2 = c(0, 300, 900)),
    dwell = 0L
  )
  # canonical order S0(0) < Y(250) < X(500); raw p05: Y=205 > X=140 (non-monotone)
  tt <- obs_travel_times(ev)
  expect_lt(tt[stop_ref == "X", travel_p05], tt[stop_ref == "Y", travel_p05])

  feeds <- snapshot_frequencies(
    ev, windows = list(am = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = data.frame(
      stop_id = c("S0", "Y", "X"),
      stop_lat = c(1, 2, 3), stop_lon = c(1, 2, 3)
    )
  )
  a <- clock_secs(feeds$structural$stop_times[order(stop_sequence)]$arrival_time)
  expect_true(all(diff(a) >= 0)) # guard made it monotone
  expect_identical(a[2], a[3]) # X clamped up to Y's arrival
})

test_that("missing coordinates / agency are publish blockers (or strict errors)", {
  ev <- make_events_clean()
  win <- list(am = c("06:00", "09:00"))
  feeds <- suppressWarnings(snapshot_frequencies(ev, windows = win))
  st <- snapshot_publishable(feeds$median)
  expect_false(st$publishable)
  expect_true(any(grepl("agency", st$blockers)))
  expect_true(any(grepl("coordinates", st$blockers)))
  # every scenario carries the same blockers
  expect_false(snapshot_publishable(feeds$reliable)$publishable)
  # strict -> error
  expect_error(snapshot_frequencies(ev, windows = win, strict = TRUE))
})

test_that("skipped stops never leak NA into generated GTFS clock fields", {
  feeds <- snapshot_frequencies(
    make_events_with_skipped(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  for (nm in names(feeds)) {
    f <- feeds[[nm]]
    # the always-skipped S3 must not appear in the representative pattern
    expect_false("S3" %in% f$stop_times$stop_id, info = nm)
    # cheap non-validator assertion: no clock field contains "NA"
    for (col in c("arrival_time", "departure_time")) {
      expect_false(any(grepl("NA", f$stop_times[[col]])), info = paste(nm, col))
    }
    for (col in c("start_time", "end_time")) {
      expect_false(any(grepl("NA", f$frequencies[[col]])), info = paste(nm, col))
    }
  }
})

test_that("snapshot_frequencies rejects trips whose only timed rows are unserved", {
  # skipped-only 'trips' with stale times produce no served start -> no headway
  expect_error(
    suppressWarnings(snapshot_frequencies(
      make_events_skipped_only_trips(),
      windows = list(am = c("06:00", "09:00"))
    )),
    "usable headway"
  )
})

test_that("snapshot_frequencies validates its arguments", {
  ev <- make_events_clean()
  expect_error(snapshot_frequencies(ev), "windows")
  expect_error(snapshot_frequencies(ev, windows = list(c("06:00", "09:00"))), "named")
  expect_error(
    snapshot_frequencies(ev, windows = list(am = c("06:00", "09:00")), exact_times = 2L),
    "exact_times"
  )
  # no multi-run group -> informative error
  expect_error(
    suppressWarnings(snapshot_frequencies(
      make_events_degenerate()[route_ref == "R3"],
      windows = list(am = c("08:00", "10:00"))
    )),
    "usable headway"
  )
})
