# Scenario numbers are hand-worked from the synthetic fixtures.
# Windowing/quantiles reuse the summarise module already tested in
# test-summarise.R.

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

# make_events_clean() runs start 06:00/06:10/06:22/06:40. Splitting at 06:15
# puts two runs (one headway) in each window, which is the minimum that gives
# both windows a trip of its own.
two_windows <- function() {
  list(early = c("06:00", "06:15"), later = c("06:15", "09:00"))
}

test_that("rt2s_frequencies emits one feed per scenario with exact numbers", {
  feeds <- rt2s_frequencies(
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
  feeds <- rt2s_frequencies(
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
  feeds <- rt2s_frequencies(
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
  expect_true(rt2s_publishable(f)$publishable)
})

test_that("rt2s_monotone_offsets clamps, rounds, and propagates dwell", {
  offsets <- rt2s_monotone_offsets(
    travel = c(-2.4, 10.6, 8.2, 11.2),
    dwell = c(-0.4, 1.6, 3.4, 0.6)
  )

  expect_identical(offsets$arrival, c(0L, 11L, 13L, 16L))
  expect_identical(offsets$departure, c(0L, 13L, 16L, 17L))
  expect_identical(offsets$departure - offsets$arrival, c(0L, 2L, 3L, 1L))
  expect_true(all(diff(offsets$arrival) >= 0L))
  expect_true(all(diff(offsets$departure) >= 0L))
})

test_that("rt2s_monotone_offsets validates its input vectors", {
  expect_error(rt2s_monotone_offsets(c(0, 1), 0), "equal length")
  expect_error(rt2s_monotone_offsets(c(0, NA), c(0, 1)), "finite numeric")
  expect_error(rt2s_monotone_offsets(c(0, Inf), c(0, 1)), "finite numeric")
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
  tt <- rt2s_obs_travel_times(ev)
  expect_lt(tt[stop_ref == "X", travel_p05], tt[stop_ref == "Y", travel_p05])

  feeds <- rt2s_frequencies(
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
  feeds <- suppressWarnings(rt2s_frequencies(ev, windows = win))
  st <- rt2s_publishable(feeds$median)
  expect_false(st$publishable)
  expect_true(any(grepl("agency", st$blockers)))
  expect_true(any(grepl("coordinates", st$blockers)))
  # every scenario carries the same blockers
  expect_false(rt2s_publishable(feeds$reliable)$publishable)
  # strict -> error
  expect_error(rt2s_frequencies(ev, windows = win, strict = TRUE))
})

test_that("skipped stops never leak NA into generated GTFS clock fields", {
  feeds <- rt2s_frequencies(
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

test_that("rt2s_frequencies rejects trips whose only timed rows are unserved", {
  # skipped-only 'trips' with stale times produce no served start -> no headway
  expect_error(
    suppressWarnings(rt2s_frequencies(
      make_events_skipped_only_trips(),
      windows = list(am = c("06:00", "09:00"))
    )),
    "trip-start headway"
  )
})

test_that("rt2s_frequencies can use passage-derived headways", {
  ev <- make_events_shared_trip_ref_passages()
  win <- list(am = c("06:00", "09:00"))
  expect_error(
    suppressWarnings(rt2s_frequencies(ev, windows = win)),
    "trip-start headway"
  )

  expect_warning(
    feeds <- rt2s_frequencies(
      ev,
      windows = win,
      quantiles = c(median = 0.5),
      agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
      stops = freq_stops(),
      headway_method = "passage",
      reference_stops = "S1"
    ),
    "visited more than once"
  )
  expect_identical(names(feeds), "median")
  expect_identical(feeds$median$frequencies$headway_secs, 750L)
  expect_true(rt2s_publishable(feeds$median)$publishable)
})

test_that("rt2s_frequencies excludes unknown-direction passage groups", {
  expect_warning(
    expect_warning(
      feeds <- rt2s_frequencies(
        make_events_partly_unknown_direction(),
        windows = list(am = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
        stops = freq_stops(),
        headway_method = "passage",
        reference_stops = "S1"
      ),
      "unknown 'direction_id'"
    ),
    "visited more than once"
  )
  expect_identical(names(feeds), "median")

  trips <- as.data.frame(feeds$median$trips)[, c(
    "route_id",
    "service_id",
    "trip_id",
    "direction_id"
  ), drop = FALSE]
  expect_identical(
    trips,
    data.frame(
      route_id = "R36",
      service_id = "SVC1",
      trip_id = "R36_0_am",
      direction_id = 0L,
      stringsAsFactors = FALSE
    )
  )

  frequencies <- as.data.frame(feeds$median$frequencies)[, c(
    "trip_id",
    "start_time",
    "end_time",
    "headway_secs",
    "exact_times"
  ), drop = FALSE]
  expect_identical(
    frequencies,
    data.frame(
      trip_id = "R36_0_am",
      start_time = "06:00:00",
      end_time = "09:00:00",
      headway_secs = 600L,
      exact_times = 0L,
      stringsAsFactors = FALSE
    )
  )
})

test_that("rt2s_frequencies reports all-unknown passage directions", {
  expect_warning(
    expect_error(
      rt2s_frequencies(
        make_events_all_unknown_direction(),
        windows = list(am = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
        stops = freq_stops(),
        headway_method = "passage",
        reference_stops = "S1"
      ),
      "No direction-unique reference stop is available after excluding"
    ),
    "unknown 'direction_id'"
  )
})

test_that("rt2s_frequencies default and explicit trip-start methods match", {
  args <- list(
    events = make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  default <- do.call(rt2s_frequencies, args)
  explicit <- do.call(
    rt2s_frequencies,
    c(args, list(headway_method = "trip_start"))
  )
  expect_identical(
    lapply(default, `[[`, "frequencies"),
    lapply(explicit, `[[`, "frequencies")
  )
  expect_identical(
    lapply(default, `[[`, "stop_times"),
    lapply(explicit, `[[`, "stop_times")
  )
  expect_identical(
    lapply(default, `[[`, "trips"),
    lapply(explicit, `[[`, "trips")
  )
})

test_that("rt2s_frequencies warns about ignored passage arguments", {
  args <- list(
    events = make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  expect_warning(
    do.call(rt2s_frequencies, c(args, list(reference_stops = "S1"))),
    "reference_stops"
  )
  expect_warning(
    do.call(rt2s_frequencies, c(args, list(min_revisit_gap_s = 60L))),
    "min_revisit_gap_s"
  )
})

test_that("rt2s_frequencies reports passage-specific empty-headway errors", {
  events <- rt2s_events_from_trip_updates(make_updates())
  expect_warning(
    expect_error(
      rt2s_frequencies(
        events,
        windows = list(am = c("06:00", "09:00")),
        headway_method = "passage",
        reference_stops = "S1"
      ),
      "passage headway"
    ),
    "only one passage"
  )
})

test_that("rt2s_frequencies validates its arguments", {
  ev <- make_events_clean()
  expect_error(rt2s_frequencies(ev), "windows")
  expect_error(rt2s_frequencies(ev, windows = list(c("06:00", "09:00"))), "named")
  expect_error(
    rt2s_frequencies(ev, windows = list(am = c("06:00", "09:00")), exact_times = 2L),
    "exact_times"
  )
  # no multi-run group -> informative error
  expect_error(
    suppressWarnings(rt2s_frequencies(
      make_events_degenerate()[route_ref == "R3"],
      windows = list(am = c("08:00", "10:00"))
    )),
    "trip-start headway"
  )
})

# --- FR-2: decoupled travel and headway quantiles ---------------------------

test_that("quantiles can decouple travel time from headway", {
  # The point of the list form: a free-flow running time at a *typical*
  # frequency. Coupled quantiles cannot express this - a p05 travel time would
  # drag a p05 headway (612) along with it.
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = list(
      structural = c(travel = 0.05, headway = 0.50),
      median = c(travel = 0.50, headway = 0.50),
      reliable = c(travel = 0.95, headway = 0.95)
    ),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  # structural and median now share the p50 headway...
  expect_identical(feeds$structural$frequencies$headway_secs, 720L)
  expect_identical(feeds$median$frequencies$headway_secs, 720L)
  expect_identical(feeds$reliable$frequencies$headway_secs, 1044L)
  # ...while their travel times still differ (p05 vs p50).
  expect_identical(
    clock_secs(feeds$structural$stop_times[order(stop_sequence)]$arrival_time),
    c(0L, 283L, 583L)
  )
  expect_identical(
    clock_secs(feeds$median$stop_times[order(stop_sequence)]$arrival_time),
    c(0L, 310L, 625L)
  )
})

test_that("quantiles decouple in the other direction too", {
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = list(mixed = c(travel = 0.05, headway = 0.95)),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  expect_identical(feeds$mixed$frequencies$headway_secs, 1044L)
  expect_identical(
    clock_secs(feeds$mixed$stop_times[order(stop_sequence)]$arrival_time),
    c(0L, 283L, 583L)
  )
})

test_that("a coupled list is identical to the bare numeric form", {
  args <- list(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  bare <- do.call(
    rt2s_frequencies,
    c(args, list(quantiles = c(structural = 0.05, median = 0.5)))
  )
  listed <- do.call(
    rt2s_frequencies,
    c(args, list(quantiles = list(structural = 0.05, median = 0.5)))
  )
  # An omitted side inherits the given one, so this is also identical.
  inherited <- do.call(
    rt2s_frequencies,
    c(args, list(quantiles = list(
      structural = c(travel = 0.05),
      median = c(headway = 0.5)
    )))
  )
  for (scenario in c("structural", "median")) {
    for (tbl in names(bare[[scenario]])) {
      expect_equal(listed[[scenario]][[tbl]], bare[[scenario]][[tbl]])
      expect_equal(inherited[[scenario]][[tbl]], bare[[scenario]][[tbl]])
    }
  }
})

test_that("all scenario feeds share one trip set", {
  # The fixture's runs start 06:00/06:10/06:22/06:40, so this boundary is what
  # actually splits them into two populated windows (one gap each).
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = two_windows(),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  ref <- feeds$median
  for (f in feeds) {
    expect_identical(sort(f$trips$trip_id), sort(ref$trips$trip_id))
    expect_identical(
      sort(unique(f$stop_times$trip_id)),
      sort(unique(ref$stop_times$trip_id))
    )
    expect_identical(sort(f$frequencies$trip_id), sort(ref$frequencies$trip_id))
  }
})

test_that("resolve_quantiles rejects malformed scenario quantiles", {
  ev <- make_events_clean()
  win <- list(am = c("06:00", "09:00"))
  bad <- function(q) {
    expect_error(rt2s_frequencies(ev, windows = win, quantiles = q))
  }
  bad(list())
  bad(list(0.5))
  bad(list(a = c(travle = 0.5)))
  bad(list(a = c(travel = 0.5, headway = 1.5)))
  bad(list(a = "0.5"))
  bad(c(a = 0.5, a = 0.6))
  expect_error(
    rt2s_frequencies(
      ev,
      windows = win,
      quantiles = data.frame(a = 0.5)
    ),
    "data.frame"
  )
})

# --- FR-1: baseline-anchored patterns ---------------------------------------

anchored_feeds <- function(scenarios, ratio, ...) {
  rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = stats::setNames(rep(0.5, length(scenarios)), scenarios),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    scaling = make_scaling(scenarios, ratio),
    ...
  )
}

test_that("baseline mode reproduces the planned pattern at ratio 1", {
  f <- anchored_feeds("median", 1)$median
  st <- f$stop_times[order(stop_sequence)]
  # The published pattern, rebased on its first departure and clamped.
  expect_identical(clock_secs(st$arrival_time), c(0L, 120L, 270L))
  expect_identical(clock_secs(st$departure_time), c(30L, 150L, 300L))
  # Dense stop_sequence from the fixture's 4:6.
  expect_identical(st$stop_sequence, 1:3)
  expect_identical(st$stop_id, c("S1", "S2", "S3"))
  # The headway is still observed: only the pattern source changed.
  expect_identical(f$frequencies$headway_secs, 720L)
  expect_identical(f$trips$trip_id, "R1_0_am_peak")
})

test_that("baseline mode scales offsets by the ratio exactly", {
  doubled <- anchored_feeds("median", 2)$median$stop_times[order(stop_sequence)]
  expect_identical(clock_secs(doubled$arrival_time), c(0L, 240L, 540L))
  expect_identical(clock_secs(doubled$departure_time), c(60L, 300L, 600L))

  halved <- anchored_feeds("median", 0.5)$median$stop_times[order(stop_sequence)]
  expect_identical(clock_secs(halved$arrival_time), c(0L, 60L, 135L))
  expect_identical(clock_secs(halved$departure_time), c(15L, 75L, 150L))
})

test_that("baseline ratios vary independently by window and by scenario", {
  scaling <- rbind(
    make_scaling("fast", 1, window = "early"),
    make_scaling("fast", 2, window = "later"),
    make_scaling("slow", 3, window = "early"),
    make_scaling("slow", 4, window = "later")
  )
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = two_windows(),
    quantiles = c(fast = 0.5, slow = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    scaling = scaling
  )
  last_arrival <- function(f, trip) {
    st <- f$stop_times[trip_id == trip][order(stop_sequence)]
    clock_secs(st$arrival_time[nrow(st)])
  }
  # 270 s of planned running time scaled by each cell's own ratio: four
  # distinct values prove the pattern is resolved per trip, not per route.
  expect_identical(last_arrival(feeds$fast, "R1_0_early"), 270L)
  expect_identical(last_arrival(feeds$fast, "R1_0_later"), 540L)
  expect_identical(last_arrival(feeds$slow, "R1_0_early"), 810L)
  expect_identical(last_arrival(feeds$slow, "R1_0_later"), 1080L)
  # ...and the trip set is still shared.
  expect_identical(sort(feeds$fast$trips$trip_id), sort(feeds$slow$trips$trip_id))
})

test_that("trips sharing a pattern and ratio resolve once, without fan-out", {
  # Two windows, same ratio: internally both trips map to a single scaled
  # pattern (the offsets are resolved per distinct pattern-ratio pair, not per
  # trip). This pins that the de-duplication neither drops nor duplicates rows.
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = two_windows(),
    quantiles = c(median = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    scaling = rbind(
      make_scaling("median", 1.5, window = "early"),
      make_scaling("median", 1.5, window = "later")
    )
  )
  st <- feeds$median$stop_times
  expect_identical(sort(unique(st$trip_id)), c("R1_0_early", "R1_0_later"))
  # exactly 3 stops per trip, no cartesian blow-up
  expect_identical(nrow(st), 6L)
  expect_identical(as.integer(table(st$trip_id)), c(3L, 3L))
  # both trips carry the same scaled offsets: 270 * 1.5 = 405
  for (trip in unique(st$trip_id)) {
    one <- st[trip_id == trip][order(stop_sequence)]
    expect_identical(clock_secs(one$arrival_time), c(0L, 180L, 405L))
    expect_identical(clock_secs(one$departure_time), c(45L, 225L, 450L))
  }
})

test_that("baseline mode inherits agency, stops and real route_type", {
  # No agency= or stops= given: a complete baseline supplies both, so there is
  # nothing left to block publication.
  f <- anchored_feeds("median", 1)$median
  expect_true(rt2s_publishable(f)$publishable)
  expect_identical(f$agency$agency_name, "Baseline Transit")
  expect_identical(f$agency$agency_id, "AGB")
  expect_true(all(!is.na(f$stops$stop_lat)))
  # route_type 0 (tram) inherited, not the scaffold's 3 (bus).
  expect_identical(f$routes$route_type, 0L)
  expect_identical(f$routes$agency_id, "AGB")
  # calendar still describes the observed span, not the baseline's calendar.
  expect_identical(f$calendar$tuesday, 1L)
  expect_identical(f$calendar$saturday, 0L)
})

test_that("an explicit agency and stops still win over the baseline", {
  f <- anchored_feeds(
    "median",
    1,
    agency = list(name = "Override", url = "https://o.org", timezone = "UTC")
  )$median
  expect_identical(f$agency$agency_name, "Override")
  # routes keep referential integrity against the single emitted agency
  expect_identical(unique(f$routes$agency_id), unique(f$agency$agency_id))
})

test_that("baseline mode is referentially consistent", {
  f <- anchored_feeds(c("a", "b"), 1)$a
  expect_s3_class(f, "gtfs")
  expect_true(all(f$stop_times$stop_id %in% f$stops$stop_id))
  expect_true(all(f$stop_times$trip_id %in% f$trips$trip_id))
  expect_true(all(f$frequencies$trip_id %in% f$trips$trip_id))
  expect_true(all(f$trips$route_id %in% f$routes$route_id))
  expect_true(all(f$trips$service_id %in% f$calendar$service_id))
})

test_that("baseline mode rejects incoherent argument combinations", {
  ev <- make_events_clean()
  win <- list(am_peak = c("06:00", "09:00"))
  expect_error(
    rt2s_frequencies(ev, windows = win, pattern_source = "baseline"),
    "needs a 'baseline'"
  )
  expect_error(
    rt2s_frequencies(
      ev,
      windows = win,
      baseline = make_baseline_freq(),
      pattern_source = "baseline"
    ),
    "needs 'scaling'"
  )
  expect_error(
    rt2s_frequencies(ev, windows = win, baseline = make_baseline_freq()),
    "pattern_source is \"observed\""
  )
  expect_error(
    rt2s_frequencies(
      ev,
      windows = win,
      scaling = make_scaling("median", 1)
    ),
    "only applies to"
  )
})

test_that("scaling is validated per cell", {
  bad <- function(scaling, regexp) {
    expect_error(
      rt2s_frequencies(
        make_events_clean(),
        windows = list(am_peak = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        baseline = make_baseline_freq(),
        pattern_source = "baseline",
        scaling = scaling
      ),
      regexp
    )
  }
  bad(make_scaling("median", 0), "strictly greater than 0")
  bad(make_scaling("median", -1), "strictly greater than 0")
  bad(make_scaling("median", NA_real_), "strictly greater than 0")
  bad(make_scaling("median", Inf), "strictly greater than 0")
  bad(make_scaling("typo", 1), "quantiles' does not define")
  bad(make_scaling("median", 1, window = "nope"), "windows' does not define")
  bad(rbind(make_scaling("median", 1), make_scaling("median", 2)), "duplicate")
  bad(make_scaling("median", 1)[0, ], "no rows")
  bad(make_scaling("median", 1)[, c("route_ref", "ratio")], "Missing required")
})

test_that("a missing scaling cell errors, or drops from every scenario", {
  ev <- make_events_clean()
  # "later" has no ratio for scenario b.
  scaling <- rbind(
    make_scaling(c("a", "b"), 1, window = "early"),
    make_scaling("a", 1, window = "later")
  )
  call_it <- function(...) {
    rt2s_frequencies(
      ev,
      windows = two_windows(),
      quantiles = c(a = 0.5, b = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = scaling,
      ...
    )
  }
  expect_error(call_it(), "no ratio for")
  expect_warning(feeds <- call_it(scaling_missing = "drop"), "dropped from every")
  # The incomplete cell leaves *both* feeds, so the trip sets stay identical.
  expect_identical(feeds$a$trips$trip_id, "R1_0_early")
  expect_identical(feeds$b$trips$trip_id, "R1_0_early")
})

test_that("a disjoint route identity is a dedicated error", {
  b <- make_baseline_freq()
  b$trips$route_id <- "OTHER"
  b$routes$route_id <- "OTHER"
  expect_error(
    rt2s_frequencies(
      make_events_clean(),
      windows = list(am_peak = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      baseline = b,
      pattern_source = "baseline",
      scaling = make_scaling("median", 1)
    ),
    "No \\(route, direction\\) key is shared"
  )
})

# --- Phase 4: the planned "scheduled" scenario ------------------------------

test_that("headways= overrides the observed quantile for named cells only", {
  windows <- list(am_peak = c("06:00", "09:00"))
  sh <- rt2s_baseline_headways(make_baseline_freq(), windows = windows)
  sh$scenario <- "scheduled"
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = windows,
    quantiles = list(scheduled = c(headway = 0.5), median = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    scaling = make_scaling(c("scheduled", "median"), 1)
  , headways = sh)
  # scheduled takes the planned headway; median keeps the observed one.
  expect_identical(feeds$scheduled$frequencies$headway_secs, 750L)
  expect_identical(feeds$median$frequencies$headway_secs, 720L)
  # Both still share the trip set and the planned pattern.
  expect_identical(feeds$scheduled$trips$trip_id, feeds$median$trips$trip_id)
  expect_identical(
    feeds$scheduled$stop_times$arrival_time,
    feeds$median$stop_times$arrival_time
  )
})

test_that("headways= is validated and ignores cells that are not emitted", {
  windows <- list(am_peak = c("06:00", "09:00"))
  base_call <- function(headways) {
    rt2s_frequencies(
      make_events_clean(),
      windows = windows,
      quantiles = c(median = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = make_scaling("median", 1),
      headways = headways
    )
  }
  ok <- data.frame(
    route_ref = "R1", direction_id = 0L, window = "am_peak",
    scenario = "median", headway_secs = 999L
  )
  expect_identical(base_call(ok)$median$frequencies$headway_secs, 999L)

  bad_scen <- ok
  bad_scen$scenario <- "nope"
  expect_error(base_call(bad_scen), "does not define")

  nonpositive <- ok
  nonpositive$headway_secs <- 0L
  expect_error(base_call(nonpositive), "positive whole number")

  unknown <- ok
  unknown$route_ref <- "ZZ"
  expect_warning(base_call(unknown), "not\n?\\s*emitted|not emitted")
})

# --- FR-6: the resolved grid ------------------------------------------------

test_that("the resolved grid covers every cell x scenario in observed mode", {
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = two_windows(),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  grid <- rt2s_resolved_grid(feeds)

  expect_s3_class(grid, "data.table")
  expect_identical(
    names(grid),
    c(
      "route_ref", "direction_id", "window", "scenario", "trip_id",
      "ratio", "headway_secs", "headway_source", "emitted", "drop_reason"
    )
  )
  # 2 windows x 3 scenarios, one row each, no duplicates.
  expect_identical(nrow(grid), 6L)
  expect_identical(anyDuplicated(grid, by = c("trip_id", "scenario")), 0L)
  expect_identical(sort(unique(grid$scenario)), c("median", "reliable", "structural"))
  expect_identical(sort(unique(grid$window)), c("early", "later"))

  # Observed mode has no ratios at all, and nothing is dropped.
  expect_true(all(is.na(grid$ratio)))
  expect_true(all(grid$emitted))
  expect_true(all(is.na(grid$drop_reason)))
  expect_identical(unique(grid$headway_source), "observed")
})

test_that("the grid's emitted rows reconcile against what was written", {
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = two_windows(),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops()
  )
  grid <- rt2s_resolved_grid(feeds)

  # This is the invariant the downstream drop funnel asserts: the grid's final
  # stage equals the trips actually written, per scenario.
  for (s in names(feeds)) {
    g <- grid[scenario == s]
    expect_identical(
      sort(g[emitted == TRUE]$trip_id),
      sort(feeds[[s]]$trips$trip_id)
    )
    freq <- feeds[[s]]$frequencies
    expect_identical(
      sort(g[!is.na(headway_secs)]$trip_id),
      sort(freq$trip_id)
    )
    # and the headway reported is the headway written
    expect_identical(
      g[freq, on = "trip_id"]$headway_secs,
      freq$headway_secs
    )
  }
})

test_that("cells dropped for want of a ratio stay in the grid, flagged", {
  scaling <- rbind(
    make_scaling(c("a", "b"), 1, window = "early"),
    make_scaling("a", 1, window = "later")
  )
  expect_warning(
    feeds <- rt2s_frequencies(
      make_events_clean(),
      windows = two_windows(),
      quantiles = c(a = 0.5, b = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = scaling,
      scaling_missing = "drop"
    ),
    "dropped from every"
  )
  grid <- rt2s_resolved_grid(feeds)

  # Both candidate cells are still accounted for in both scenarios, even though
  # only one of them reached either feed.
  expect_identical(nrow(grid), 4L)
  expect_identical(sort(unique(grid$window)), c("early", "later"))

  kept <- grid[window == "early"]
  expect_true(all(kept$emitted))
  expect_true(all(is.na(kept$drop_reason)))
  expect_identical(unique(kept$ratio), 1)

  # The drop applies to *every* scenario, not just the one that lacked a ratio.
  gone <- grid[window == "later"]
  expect_identical(nrow(gone), 2L)
  expect_false(any(gone$emitted))
  expect_identical(unique(gone$drop_reason), "no_ratio")
  expect_true(all(is.na(gone$headway_secs)))
  # A dropped cell reports no ratio even for the scenario that had one: it was
  # never applied, and the grid reports what was applied.
  expect_true(all(is.na(gone$ratio)))
})

test_that("a cell with a headway but no stop pattern is flagged, not silent", {
  # The baseline serves R1 only, so R2's headway group has no anchored pattern.
  r2 <- make_events_from_offsets(
    route = "R2",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(E = "06:05:00", F = "06:20:00"),
    stops = c("S1", "S2"),
    offsets = list(E = c(0, 300), F = c(0, 300))
  )
  ev <- rbind(make_events_clean(), r2)
  expect_warning(
    feeds <- rt2s_frequencies(
      ev,
      windows = list(am_peak = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = make_scaling("median", 1)
    ),
    "no baseline"
  )
  grid <- rt2s_resolved_grid(feeds)
  expect_identical(sort(unique(grid$route_ref)), c("R1", "R2"))
  expect_identical(grid[route_ref == "R2"]$drop_reason, "no_stop_pattern")
  expect_false(grid[route_ref == "R2"]$emitted)
  expect_true(grid[route_ref == "R1"]$emitted)
  expect_true(is.na(grid[route_ref == "R1"]$drop_reason))
})

test_that("the grid distinguishes an overridden headway from an observed one", {
  windows <- list(am_peak = c("06:00", "09:00"))
  sh <- rt2s_baseline_headways(make_baseline_freq(), windows = windows)
  sh$scenario <- "scheduled"
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = windows,
    quantiles = list(scheduled = c(headway = 0.5), median = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    scaling = make_scaling(c("scheduled", "median"), 1),
    headways = sh
  )
  grid <- rt2s_resolved_grid(feeds)
  expect_identical(grid[scenario == "scheduled"]$headway_source, "override")
  expect_identical(grid[scenario == "scheduled"]$headway_secs, 750L)
  expect_identical(grid[scenario == "median"]$headway_source, "observed")
  expect_identical(grid[scenario == "median"]$headway_secs, 720L)
})

test_that("the grid records the ratio actually applied, per scenario", {
  feeds <- anchored_feeds(c("fast", "slow"), c(0.5, 2))
  grid <- rt2s_resolved_grid(feeds)
  expect_identical(grid[scenario == "fast"]$ratio, 0.5)
  expect_identical(grid[scenario == "slow"]$ratio, 2)
})

test_that("rt2s_resolved_grid rejects anything that is not a frequency feed set", {
  expect_error(rt2s_resolved_grid(list()), "no resolved grid")
  scaffold <- suppressWarnings(rt2s_scaffold(make_events_clean()))
  expect_error(rt2s_resolved_grid(scaffold), "no resolved grid")
})

# --- extra_trips (FR-5) -------------------------------------------------------
# A real feed is not purely frequency-based. Cells that cannot be expressed as a
# repeating headway are carried as individually-timed trips with no
# frequencies.txt row - which is what the GTFS specification means by a mixed
# feed, since only trips listed in frequencies.txt are frequency-based.

# Frequency material and extra trips over the same events, with S9 reachable.
extra_feeds <- function(extra_trips, ...) {
  suppressWarnings(rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    stops = make_stops_with_extra(),
    extra_trips = extra_trips,
    ...
  ))
}

test_that("extra trips reach trips.txt and stop_times.txt but never frequencies.txt", {
  feeds <- extra_feeds(list(median = make_extra_trips(c("X1", "X2"))))
  med <- feeds$median

  expect_setequal(med$trips$trip_id, c("R1_0_am_peak", "X1", "X2"))
  expect_true(all(c("X1", "X2") %in% med$stop_times$trip_id))
  expect_identical(nrow(med$stop_times[trip_id == "X1"]), 3L)
  # The load-bearing assertion: no frequencies row, so these are exact-time
  # trips read straight from stop_times.
  expect_false(any(c("X1", "X2") %in% med$frequencies$trip_id))
  expect_identical(med$frequencies$trip_id, "R1_0_am_peak")
})

test_that("extra trips carry absolute clock times, not offsets from 00:00:00", {
  feeds <- extra_feeds(list(median = make_extra_trips("X1")))
  st <- feeds$median$stop_times[trip_id == "X1"]
  data.table::setorderv(st, "stop_sequence")
  expect_identical(st$arrival_time, c("07:00:00", "07:05:00", "07:11:00"))
  expect_identical(st$departure_time, c("07:00:30", "07:05:30", "07:11:00"))
  # while the generated frequency trip is still offsets from trip start
  gen <- feeds$median$stop_times[trip_id == "R1_0_am_peak"]
  expect_identical(gen$arrival_time[[1L]], "00:00:00")
})

test_that("an extra trip's stops and routes reach stops.txt and routes.txt", {
  feeds <- extra_feeds(list(median = make_extra_trips("X1")))
  # S9 is on no emitted frequency pattern, so it is in stops.txt only because
  # the stop_ids derivation was widened; build_stops_table() filters to that set.
  expect_true("S9" %in% feeds$median$stops$stop_id)
  expect_false(is.na(feeds$median$stops[stop_id == "S9"]$stop_lat))
  # every stop_id and route_id referenced by the feed resolves
  expect_true(all(feeds$median$stop_times$stop_id %in% feeds$median$stops$stop_id))
  expect_true(all(feeds$median$trips$route_id %in% feeds$median$routes$route_id))
})

test_that("an extra trip on a baseline route with no observed headway is emitted", {
  b <- make_baseline_freq()
  b$routes <- rbind(b$routes, data.frame(
    route_id = "R7",
    agency_id = "AGB",
    route_short_name = "7",
    route_long_name = "Unobserved Line",
    route_type = 0L
  ))
  feeds <- suppressWarnings(rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    baseline = b,
    pattern_source = "baseline",
    scaling = make_scaling("median", 1),
    extra_trips = list(median = make_extra_trips("X1", route_id = "R7",
                                                 stops = c("S1", "S2", "S3")))
  ))
  expect_true("R7" %in% feeds$median$routes$route_id)
  # the inherited row keeps the operator's route_type rather than scaffolding 3
  expect_identical(feeds$median$routes[route_id == "R7"]$route_type, 0L)
  expect_true("X1" %in% feeds$median$trips$trip_id)
})

test_that("extra trips are stamped with the feed's single synthesized service", {
  feeds <- extra_feeds(list(median = make_extra_trips("X1")), service_id = "WD")
  expect_identical(unique(feeds$median$trips$service_id), "WD")
  expect_identical(feeds$median$calendar$service_id, "WD")
})

test_that("an extra trip naming a different service_id is an error", {
  expect_error(
    extra_feeds(list(median = make_extra_trips("X1", service_id = "OTHER"))),
    "single synthesized service"
  )
  # ... and an explicitly matching one is accepted
  feeds <- extra_feeds(list(median = make_extra_trips("X1", service_id = "SVC1")))
  expect_identical(unique(feeds$median$trips$service_id), "SVC1")
})

test_that("extra trips differing in count across scenarios are accepted", {
  # Reverses the original FR-5 note: exact-trip evidence is drawn per scenario
  # (scheduled from the timetable, others from observed passages) and filtered by
  # that scenario's own ratio, so the counts genuinely differ. Requiring matching
  # id sets would reject correct data.
  feeds <- extra_feeds(list(
    median = make_extra_trips(c("X1", "X2", "X3")),
    reliable = make_extra_trips("X1")
  ))
  expect_length(feeds, 3L)
  expect_setequal(feeds$structural$trips$trip_id, "R1_0_am_peak")
  expect_setequal(feeds$median$trips$trip_id, c("R1_0_am_peak", "X1", "X2", "X3"))
  expect_setequal(feeds$reliable$trips$trip_id, c("R1_0_am_peak", "X1"))
})

test_that("a scenario supplying no extra trips is accepted", {
  feeds <- extra_feeds(list(
    median = make_extra_trips("X1"),
    reliable = list(trips = NULL, stop_times = NULL)
  ))
  expect_setequal(feeds$reliable$trips$trip_id, "R1_0_am_peak")
})

test_that("extra trips are not rows of the resolved grid", {
  # Decision: the grid is one row per candidate (route, direction, window) cell.
  # Extra trips are not cells, and adding them would produce rows whose
  # route_ref, window, ratio, headway_secs and drop_reason are all NA.
  feeds <- extra_feeds(list(median = make_extra_trips(c("X1", "X2"))))
  grid <- rt2s_resolved_grid(feeds)
  expect_false(any(c("X1", "X2") %in% grid$trip_id))
  expect_identical(nrow(grid), 3L)
  # so the funnel closes only when the caller adds back the ids it supplied
  expect_identical(
    sort(c(grid[scenario == "median" & emitted == TRUE]$trip_id, "X1", "X2")),
    sort(feeds$median$trips$trip_id)
  )
})

test_that("extra_trips = NULL leaves every feed byte-identical", {
  args <- list(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    stops = make_stops_with_extra()
  )
  without <- suppressWarnings(do.call(rt2s_frequencies, args))
  explicit <- suppressWarnings(
    do.call(rt2s_frequencies, c(args, list(extra_trips = NULL)))
  )
  expect_identical(without, explicit)
})

test_that("extra_trips must be a named list keyed by a defined scenario", {
  expect_error(
    extra_feeds(list(make_extra_trips("X1"))),
    "named list keyed by scenario name"
  )
  # one scenario's material passed directly - names read as scenario names, so
  # the message has to name the actual mistake
  expect_error(
    extra_feeds(make_extra_trips("X1")),
    "looks like a single list"
  )
  expect_error(
    extra_feeds(list(nope = make_extra_trips("X1"))),
    "'quantiles' does not define"
  )
  expect_error(
    extra_feeds(list(median = "not a list")),
    "must be a list with 'trips' and 'stop_times'"
  )
  expect_error(
    extra_feeds(list(median = list(trips = make_extra_trips("X1")$trips))),
    "must be a list with 'trips' and 'stop_times'"
  )
})

test_that("an extra trip_id colliding with a generated one is an error", {
  expect_error(
    extra_feeds(list(median = make_extra_trips("R1_0_am_peak"))),
    "collide with generated frequency trip_id"
  )
})

test_that("a duplicate extra trip_id is an error", {
  expect_error(
    extra_feeds(list(median = make_extra_trips(c("X1", "X1")))),
    "duplicate trip_id"
  )
})

test_that("a dangling stop_id or route_id is an error, not a publish blocker", {
  # The lenient NA-coordinates path in build_stops_table() is for observed data
  # with genuinely unknown coordinates, not for a typo in a caller table.
  bad_stop <- make_extra_trips("X1")
  bad_stop$stop_times$stop_id[[2L]] <- "NOPE"
  expect_error(
    extra_feeds(list(median = bad_stop)),
    "in neither the emitted stop patterns nor 'stops'"
  )
  expect_error(
    extra_feeds(list(median = make_extra_trips("X1", route_id = "R99"))),
    "in neither the emitted routes nor the baseline"
  )
})

test_that("extra stop_times must reference a listed trip and have two stops", {
  orphan <- make_extra_trips("X1")
  orphan$stop_times$trip_id[[2L]] <- "X2"
  expect_error(extra_feeds(list(median = orphan)), "does not list")

  thin <- make_extra_trips("X1")
  thin$stop_times <- thin$stop_times[1L, ]
  expect_error(extra_feeds(list(median = thin)), "fewer than two stop_times")

  no_st <- make_extra_trips("X1")
  no_st$stop_times <- no_st$stop_times[0L, ]
  expect_error(extra_feeds(list(median = no_st)), "no stop times is not a valid feed")

  no_trips <- make_extra_trips("X1")
  no_trips$trips <- no_trips$trips[0L, ]
  expect_error(extra_feeds(list(median = no_trips)), "reference no trip")
})

test_that("extra stop_times must carry parsable, non-decreasing clock times", {
  unparsable <- make_extra_trips("X1")
  unparsable$stop_times$arrival_time[[2L]] <- "half past seven"
  expect_error(extra_feeds(list(median = unparsable)), "clock strings")

  backwards <- make_extra_trips("X1")
  backwards$stop_times$arrival_time[[2L]] <- "06:00:00"
  expect_error(extra_feeds(list(median = backwards)), "not non-decreasing")

  negative_dwell <- make_extra_trips("X1")
  negative_dwell$stop_times$departure_time[[1L]] <- "06:59:00"
  expect_error(extra_feeds(list(median = negative_dwell)), "not non-decreasing")

  # a trip legitimately running past midnight keeps its >= 24 h clock
  overnight <- make_extra_trips("X1")
  overnight$stop_times$arrival_time <- c("23:50:00", "24:05:00", "24:20:00")
  overnight$stop_times$departure_time <- c("23:50:30", "24:05:30", "24:20:00")
  feeds <- extra_feeds(list(median = overnight))
  st <- feeds$median$stop_times[trip_id == "X1"]
  data.table::setorderv(st, "stop_sequence")
  expect_identical(st$arrival_time, c("23:50:00", "24:05:00", "24:20:00"))
})

test_that("extra stop_sequence must be a usable within-trip order", {
  dup_seq <- make_extra_trips("X1")
  dup_seq$stop_times$stop_sequence <- c(1L, 1L, 2L)
  expect_error(extra_feeds(list(median = dup_seq)), "repeats a stop_sequence")

  na_seq <- make_extra_trips("X1")
  na_seq$stop_times$stop_sequence <- c(1L, NA_integer_, 3L)
  expect_error(extra_feeds(list(median = na_seq)), "non-negative whole number")
})

# --- FR-7: caller-supplied headway groups ------------------------------------
# Under pattern_source = "baseline" the pattern comes from the baseline, the
# ratio from scaling= and the headway from headways=, so events= contributes
# nothing to a headway group's output. headway_groups= stops it gating candidacy
# too.

# Baseline-anchored assembly driven entirely by supplied headway groups, with no
# observations at all.
supplied_feeds <- function(
  scenarios = "median",
  windows = list(am_peak = c("06:00", "09:00")),
  groups = make_headway_groups(window = names(windows)),
  ratio = 1,
  headway_secs = 600L,
  service_dates = as.Date("2026-07-14"),
  ...
) {
  rt2s_frequencies(
    events = NULL,
    windows = windows,
    quantiles = stats::setNames(rep(0.5, length(scenarios)), scenarios),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    service_dates = service_dates,
    scaling = make_scaling(scenarios, ratio, window = names(windows)),
    headways = make_headway_overrides(
      scenarios,
      headway_secs,
      window = names(windows)
    ),
    headway_groups = groups,
    ...
  )
}

test_that("a supplied group with no events is emitted (the FR-7 reproducer)", {
  # The reported reproducer, verbatim: events cover the am window only, while
  # scaling= and headways= cover both. Before FR-7 the pm group was not a
  # candidate at all, so it was absent from the grid rather than flagged.
  windows <- list(am = c("06:00:00", "09:00:00"), pm = c("16:00:00", "19:00:00"))
  baseline <- list(
    agency = data.table::data.table(
      agency_id = "A1", agency_name = "Probe",
      agency_url = "http://example.com", agency_timezone = "America/Sao_Paulo"
    ),
    stops = data.table::data.table(
      stop_id = c("S1", "S2", "S3"), stop_name = c("S1", "S2", "S3"),
      stop_lat = c(-22.90, -22.91, -22.92),
      stop_lon = c(-43.10, -43.11, -43.12)
    ),
    routes = data.table::data.table(
      route_id = "R1", route_short_name = "L1",
      route_long_name = "Probe line", route_type = 3L
    ),
    trips = data.table::data.table(
      route_id = "R1", service_id = "WD", trip_id = "T1", direction_id = 0L
    ),
    stop_times = data.table::data.table(
      trip_id = "T1",
      arrival_time = c("07:00:00", "07:10:00", "07:20:00"),
      departure_time = c("07:00:00", "07:10:00", "07:20:00"),
      stop_id = c("S1", "S2", "S3"), stop_sequence = 1:3
    ),
    calendar = data.table::data.table(
      service_id = "WD", monday = 1L, tuesday = 1L, wednesday = 1L,
      thursday = 1L, friday = 1L, saturday = 0L, sunday = 0L,
      start_date = 20260302L, end_date = 20260331L
    )
  )
  mk <- function(day, starts) {
    data.table::rbindlist(lapply(seq_along(starts), function(k) {
      t0 <- as.POSIXct(paste(day, starts[k]), tz = "America/Sao_Paulo")
      data.table::data.table(
        trip_ref = paste0("obs_", day, "_", k), route_ref = "L1",
        shape_ref = NA_character_, direction_id = 0L,
        service_date = as.Date(day), stop_ref = c("S1", "S2", "S3"),
        stop_sequence = 1:3, arrival_time = t0 + c(0, 600, 1200),
        departure_time = t0 + c(0, 600, 1200), provenance = "observed",
        vehicle_ref = paste0("V", k), source = "gps"
      )
    }))
  }
  events <- data.table::rbindlist(list(
    mk("2026-03-02", c("07:00:00", "07:30:00")),
    mk("2026-03-03", c("07:00:00", "07:30:00"))
  ))
  scenarios <- list(scheduled = c(headway = 0.50), median = c(headway = 0.50))
  key <- data.table::CJ(
    route_ref = "L1", direction_id = 0L, window = c("am", "pm"),
    scenario = names(scenarios), sorted = FALSE
  )
  groups <- unique(
    as.data.frame(key)[, c("route_ref", "direction_id", "window")]
  )

  feeds <- suppressWarnings(rt2s_frequencies(
    events = events, windows = windows, quantiles = scenarios,
    baseline = baseline, pattern_source = "baseline",
    route_key = "route_short_name",
    service_dates = as.Date(c("2026-03-02", "2026-03-03")),
    scaling = as.data.frame(data.table::copy(key)[, ratio := 1.0]),
    headways = as.data.frame(data.table::copy(key)[, headway_secs := 900L]),
    headway_groups = groups,
    scaling_missing = "drop"
  ))
  grid <- rt2s_resolved_grid(feeds)

  # The pm group is present in the grid and emitted, in both scenarios.
  expect_identical(sort(unique(grid$window)), c("am", "pm"))
  expect_identical(nrow(grid), 4L)
  expect_true(all(grid$emitted))
  expect_true(all(is.na(grid$drop_reason)))
  expect_setequal(feeds$median$trips$trip_id, c("L1_0_am", "L1_0_pm"))
  expect_setequal(feeds$scheduled$trips$trip_id, c("L1_0_am", "L1_0_pm"))
  # Both windows take the supplied headway, and the pm one is entirely
  # caller-driven: no observation ever fell in it.
  expect_identical(sort(feeds$median$frequencies$headway_secs), c(900L, 900L))
  expect_identical(unique(grid$headway_source), "override")
})

test_that("a supplied group with a ratio and an override headway needs no events", {
  feeds <- supplied_feeds()
  expect_identical(feeds$median$trips$trip_id, "R1_0_am_peak")
  expect_identical(feeds$median$frequencies$headway_secs, 600L)
  # the pattern is the baseline's, unchanged at ratio 1
  st <- feeds$median$stop_times[order(stop_sequence)]
  expect_identical(clock_secs(st$arrival_time), c(0L, 120L, 270L))
  expect_identical(st$stop_id, c("S1", "S2", "S3"))
})

test_that("events = NULL builds a referentially consistent feed", {
  f <- supplied_feeds()$median
  expect_s3_class(f, "gtfs")
  expect_true(all(f$stop_times$stop_id %in% f$stops$stop_id))
  expect_true(all(f$stop_times$trip_id %in% f$trips$trip_id))
  expect_true(all(f$frequencies$trip_id %in% f$trips$trip_id))
  expect_true(all(f$trips$route_id %in% f$routes$route_id))
  expect_true(all(f$trips$service_id %in% f$calendar$service_id))
  expect_true(rt2s_publishable(f)$publishable)
})

test_that("events = NULL without headway_groups is an error", {
  expect_error(
    rt2s_frequencies(windows = list(am_peak = c("06:00", "09:00"))),
    "no candidate headway group"
  )
  expect_error(
    rt2s_frequencies(
      events = NULL,
      windows = list(am_peak = c("06:00", "09:00"))
    ),
    "no candidate headway group"
  )
})

test_that("headway_groups is rejected under pattern_source = \"observed\"", {
  expect_error(
    rt2s_frequencies(
      make_events_clean(),
      windows = list(am_peak = c("06:00", "09:00")),
      headway_groups = make_headway_groups()
    ),
    "only applies to"
  )
})

test_that("a supplied group with no baseline pattern is flagged, not fatal", {
  groups <- rbind(
    make_headway_groups(),
    make_headway_groups(route_ref = "R9")
  )
  scaling <- rbind(
    make_scaling("median", 1),
    make_scaling("median", 1, route_ref = "R9")
  )
  overrides <- rbind(
    make_headway_overrides("median", 600L),
    make_headway_overrides("median", 600L, route_ref = "R9")
  )
  # Two warnings, in pipeline order: R9 loses its pattern, and its override row
  # then names a group that is no longer emitted.
  expect_warning(
    expect_warning(
      feeds <- rt2s_frequencies(
        events = NULL,
        windows = list(am_peak = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        baseline = make_baseline_freq(),
        pattern_source = "baseline",
        service_dates = as.Date("2026-07-14"),
        scaling = scaling,
        headways = overrides,
        headway_groups = groups
      ),
      "no baseline stop pattern"
    ),
    "not emitted"
  )
  grid <- rt2s_resolved_grid(feeds)
  # R9 is present with a reason rather than absent - the FR-6 invariant.
  expect_identical(sort(unique(grid$route_ref)), c("R1", "R9"))
  expect_identical(grid[route_ref == "R9"]$drop_reason, "no_stop_pattern")
  expect_false(grid[route_ref == "R9"]$emitted)
  expect_true(grid[route_ref == "R1"]$emitted)
  expect_false("R9_0_am_peak" %in% feeds$median$trips$trip_id)
})

test_that("a group with no resolvable headway is dropped, not written at midnight", {
  # Two windows, an override for 'early' only, and no events to supply an
  # observed quantile: 'later' has no headway from any source.
  windows <- two_windows()
  expect_warning(
    feeds <- rt2s_frequencies(
      events = NULL,
      windows = windows,
      quantiles = c(median = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      service_dates = as.Date("2026-07-14"),
      scaling = make_scaling("median", 1, window = names(windows)),
      headways = make_headway_overrides("median", 600L, window = "early"),
      headway_groups = make_headway_groups(window = names(windows))
    ),
    "no resolvable headway"
  )
  grid <- rt2s_resolved_grid(feeds)
  expect_identical(nrow(grid), 2L)
  expect_identical(grid[window == "later"]$drop_reason, "no_headway")
  expect_false(grid[window == "later"]$emitted)
  expect_true(is.na(grid[window == "later"]$headway_secs))

  # The load-bearing assertion: the dropped group writes no trip at all, so no
  # frequencies-less trip advertises a phantom 00:00:00 departure.
  expect_identical(feeds$median$trips$trip_id, "R1_0_early")
  expect_false("R1_0_later" %in% feeds$median$stop_times$trip_id)
  expect_setequal(feeds$median$frequencies$trip_id, feeds$median$trips$trip_id)
  # every emitted row carries a positive headway
  expect_true(all(grid[emitted == TRUE]$headway_secs > 0L))
})

test_that("a no_headway drop leaves every scenario, keeping one trip set", {
  windows <- two_windows()
  # scenario 'b' has no override for 'later'; 'a' has one for both.
  overrides <- rbind(
    make_headway_overrides(c("a", "b"), 600L, window = "early"),
    make_headway_overrides("a", 900L, window = "later")
  )
  expect_warning(
    feeds <- rt2s_frequencies(
      events = NULL,
      windows = windows,
      quantiles = c(a = 0.5, b = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      service_dates = as.Date("2026-07-14"),
      scaling = make_scaling(c("a", "b"), 1, window = names(windows)),
      headways = overrides,
      headway_groups = make_headway_groups(window = names(windows))
    ),
    "dropped from every scenario"
  )
  expect_identical(feeds$a$trips$trip_id, "R1_0_early")
  expect_identical(feeds$b$trips$trip_id, "R1_0_early")
  grid <- rt2s_resolved_grid(feeds)
  # row-count invariance holds with supplied groups: 2 groups x 2 scenarios
  expect_identical(nrow(grid), 4L)
  expect_identical(unique(grid[window == "later"]$drop_reason), "no_headway")
  expect_identical(nrow(grid[window == "later"]), 2L)
})

test_that("no_headway is reported after no_stop_pattern", {
  # R9 has neither a pattern nor a headway; the first stage it hits wins, which
  # is what keeps a stage-by-stage funnel's counts summing to the total.
  groups <- rbind(make_headway_groups(), make_headway_groups(route_ref = "R9"))
  scaling <- rbind(
    make_scaling("median", 1),
    make_scaling("median", 1, route_ref = "R9")
  )
  feeds <- suppressWarnings(rt2s_frequencies(
    events = NULL,
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    service_dates = as.Date("2026-07-14"),
    scaling = scaling,
    headways = make_headway_overrides("median", 600L),
    headway_groups = groups
  ))
  grid <- rt2s_resolved_grid(feeds)
  expect_identical(grid[route_ref == "R9"]$drop_reason, "no_stop_pattern")
})

test_that("headway_groups is validated on the three-key", {
  bad <- function(groups, regexp) {
    expect_error(supplied_feeds(groups = groups), regexp)
  }
  bad(make_headway_groups(window = "nope"), "windows' does not define")
  bad(rbind(make_headway_groups(), make_headway_groups()), "duplicate")
  bad(make_headway_groups()[0, ], "no rows")
  bad(make_headway_groups()[, c("route_ref", "window")], "Missing required")
  bad("R1", "must be a data.frame")
})

# --- FR-7: the service span --------------------------------------------------

test_that("service_dates drives calendar.txt and overrides events", {
  feeds <- rt2s_frequencies(
    make_events_clean(), # a single Tuesday, 2026-07-14
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops(),
    service_dates = seq(as.Date("2026-03-11"), as.Date("2026-03-13"), by = "day")
  )
  cal <- feeds$median$calendar
  expect_identical(cal$start_date, 20260311L)
  expect_identical(cal$end_date, 20260313L)
  # Wed/Thu/Fri from service_dates, not the fixture's Tuesday
  expect_identical(cal$wednesday, 1L)
  expect_identical(cal$thursday, 1L)
  expect_identical(cal$friday, 1L)
  expect_identical(cal$tuesday, 0L)
  # feed_info's span comes off the same resolved date set
  expect_identical(feeds$median$feed_info$feed_start_date, 20260311L)
  expect_identical(feeds$median$feed_info$feed_end_date, 20260313L)
  # contiguous dates -> no calendar_dates.txt at all
  expect_false("calendar_dates" %in% names(feeds$median))
})

test_that("calendar_dates.txt appears only when the date set has gaps", {
  span <- seq(as.Date("2026-03-11"), as.Date("2026-03-31"), by = "day")
  dates <- span[!span %in% as.Date(c("2026-03-18", "2026-03-19"))]
  gapped <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops(),
    service_dates = dates
  )
  cd <- gapped$median$calendar_dates
  expect_false(is.null(cd))
  expect_identical(cd$date, c(20260318L, 20260319L))
  expect_identical(unique(cd$exception_type), 2L)
  expect_identical(unique(cd$service_id), "SVC1")
  # the calendar still spans the whole range, which is what the exceptions carve
  expect_identical(gapped$median$calendar$start_date, 20260311L)
  expect_identical(gapped$median$calendar$end_date, 20260331L)

  # the same span with no holes emits nothing
  clean <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops(),
    service_dates = span
  )
  expect_false("calendar_dates" %in% names(clean$median))
})

test_that("a date whose weekday is never served is not an exception", {
  # Weekdays only: calendar.txt's flags already exclude the weekends inside the
  # span, so writing exceptions for them would be redundant.
  span <- seq(as.Date("2026-03-09"), as.Date("2026-03-20"), by = "day")
  weekdays_only <- span[!as.POSIXlt(span)$wday %in% c(0L, 6L)]
  feeds <- rt2s_frequencies(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    agency = list(name = "T", url = "https://t.org", timezone = "UTC"),
    stops = freq_stops(),
    service_dates = weekdays_only
  )
  expect_identical(feeds$median$calendar$saturday, 0L)
  expect_false("calendar_dates" %in% names(feeds$median))
})

test_that("supplied groups that widen the feed past events warn about the span", {
  # 'evening' has a baseline pattern but no observation, so it is a
  # supplied-only group and the observed span understates the feed.
  windows <- c(two_windows(), list(evening = c("20:00", "22:00")))
  expect_warning(
    rt2s_frequencies(
      make_events_clean(),
      windows = windows,
      quantiles = c(median = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = make_scaling("median", 1, window = names(windows)),
      headways = make_headway_overrides("median", 600L, window = names(windows)),
      headway_groups = make_headway_groups(window = names(windows))
    ),
    "Pass 'service_dates'"
  )
})

test_that("service_dates is validated", {
  bad <- function(dates, regexp) {
    expect_error(supplied_feeds(service_dates = dates), regexp)
  }
  bad(as.Date(character()), "non-empty vector of dates")
  bad(as.Date(c("2026-07-14", NA)), "non-empty vector of dates")
  expect_error(supplied_feeds(service_dates = NULL), "no service span")
})

test_that("rt2s_frequencies validates strict_within_window", {
  expect_error(
    rt2s_frequencies(
      make_events_clean(),
      windows = list(am_peak = c("06:00", "09:00")),
      strict_within_window = "invalid"
    ),
    "'strict_within_window' must be TRUE or FALSE"
  )
})

test_that("rt2s_frequencies forwards strict_within_window to headway estimation", {
  # 3 trips:
  # T1 @ 07:00:00 (am_peak)
  # T2 @ 08:30:00 (am_peak)
  # T3 @ 09:30:00 (midday)
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00", T3 = "09:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )
  win <- list(am_peak = c("06:00", "09:00"), midday = c("09:00", "12:00"))
  st <- data.frame(
    stop_id = c("S1", "S2"),
    stop_name = c("Stop 1", "Stop 2"),
    stop_lat = c(40.0, 40.1),
    stop_lon = c(-74.0, -74.1),
    stringsAsFactors = FALSE
  )
  ag <- list(name = "Agency", url = "https://example.com", timezone = "UTC")

  # Under strict_within_window = FALSE (default):
  # Both am_peak and midday have headways estimated from events, so frequencies.txt has 2 rows
  legacy_feeds <- rt2s_frequencies(
    events = ev,
    windows = win,
    quantiles = c(median = 0.5),
    agency = ag,
    stops = st,
    strict_within_window = FALSE
  )
  expect_identical(nrow(legacy_feeds$median$frequencies), 2L)

  # Under strict_within_window = TRUE:
  # am_peak has 1 headway (5400s).
  # midday has only 1 trip start -> no headway can be computed within window.
  # In observed mode, groups with no usable headway are not candidates, so only am_peak is emitted.
  strict_feeds <- rt2s_frequencies(
    events = ev,
    windows = win,
    quantiles = c(median = 0.5),
    agency = ag,
    stops = st,
    strict_within_window = TRUE
  )
  expect_identical(nrow(strict_feeds$median$frequencies), 1L)
  expect_identical(strict_feeds$median$frequencies$headway_secs, 5400L)
  grid <- rt2s_resolved_grid(strict_feeds)
  expect_identical(grid$window, c("am_peak", "midday"))
  expect_identical(grid$emitted, c(TRUE, FALSE))
  expect_identical(
    grid[window == "midday", drop_reason], "no_within_window_headway"
  )

  # In baseline-anchored mode with headway_groups supplied:
  # am_peak estimates 5400s; midday has no valid within-window headway from events,
  # so it becomes a candidate with no resolvable headway and drops with "no_headway".
  expect_warning(
    anchored_strict <- rt2s_frequencies(
      events = ev,
      windows = win,
      quantiles = c(median = 0.5),
      baseline = make_baseline_freq(),
      pattern_source = "baseline",
      scaling = make_scaling("median", 1, window = names(win)),
      headway_groups = make_headway_groups(window = names(win)),
      service_dates = as.Date("2026-07-14"),
      strict_within_window = TRUE
    ),
    "no resolvable headway"
  )
  expect_identical(nrow(anchored_strict$median$frequencies), 1L)
  grid_anchored <- rt2s_resolved_grid(anchored_strict)
  expect_identical(grid_anchored[window == "am_peak", emitted], TRUE)
  expect_identical(grid_anchored[window == "midday", emitted], FALSE)
  expect_identical(grid_anchored[window == "midday", drop_reason], "no_headway")
})

test_that("rt2s_frequencies forwards strict_within_window = TRUE with headway_method = 'passage'", {
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00", T3 = "09:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )
  win <- list(am_peak = c("06:00", "09:00"), midday = c("09:00", "12:00"))
  st <- data.frame(
    stop_id = c("S1", "S2"),
    stop_name = c("Stop 1", "Stop 2"),
    stop_lat = c(40.0, 40.1),
    stop_lon = c(-74.0, -74.1),
    stringsAsFactors = FALSE
  )
  ag <- list(name = "Agency", url = "https://example.com", timezone = "UTC")

  # Under strict_within_window = FALSE (default): 2 headway rows
  legacy_feeds <- rt2s_frequencies(
    events = ev,
    windows = win,
    quantiles = c(median = 0.5),
    agency = ag,
    stops = st,
    headway_method = "passage",
    reference_stops = "S1",
    strict_within_window = FALSE
  )
  expect_identical(nrow(legacy_feeds$median$frequencies), 2L)

  # Under strict_within_window = TRUE: only am_peak has 2 passages (5400s) -> 1 row
  strict_feeds <- rt2s_frequencies(
    events = ev,
    windows = win,
    quantiles = c(median = 0.5),
    agency = ag,
    stops = st,
    headway_method = "passage",
    reference_stops = "S1",
    strict_within_window = TRUE
  )
  expect_identical(nrow(strict_feeds$median$frequencies), 1L)
  expect_identical(strict_feeds$median$frequencies$headway_secs, 5400L)
  grid <- rt2s_resolved_grid(strict_feeds)
  expect_identical(grid$window, c("am_peak", "midday"))
  expect_identical(grid$emitted, c(TRUE, FALSE))
  expect_identical(
    grid[window == "midday", drop_reason], "no_within_window_headway"
  )
})

test_that("rt2s_frequencies maintains positional backwards-compatibility with v0.6.0 callers", {
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300))
  )
  win <- list(am_peak = c("06:00", "09:00"))
  st <- data.frame(
    stop_id = c("S1", "S2"),
    stop_name = c("Stop 1", "Stop 2"),
    stop_lat = c(40.0, 40.1),
    stop_lon = c(-74.0, -74.1),
    stringsAsFactors = FALSE
  )
  ag <- list(name = "Agency", url = "https://example.com", timezone = "UTC")

  # Full 25-argument positional vector matching v0.6.0 argument order:
  args_v060 <- list(
    ev,                                                       # 1: events
    win,                                                      # 2: windows
    c(median = 0.5),                                          # 3: quantiles
    ag,                                                       # 4: agency
    st,                                                       # 5: stops
    3L,                                                       # 6: route_type
    "SVC1",                                                   # 7: service_id
    NULL,                                                     # 8: service_dates
    0L,                                                       # 9: exact_times
    "en",                                                     # 10: feed_lang
    NULL,                                                     # 11: feed_contact_email
    NULL,                                                     # 12: feed_contact_url
    FALSE,                                                    # 13: strict
    3L * 3600L,                                               # 14: max_headway_secs
    "trip_start",                                             # 15: headway_method
    NULL,                                                     # 16: reference_stops
    600L,                                                     # 17: min_revisit_gap_s
    NULL,                                                     # 18: baseline
    "observed",                                               # 19: pattern_source
    NULL,                                                     # 20: scaling
    "error",                                                  # 21: scaling_missing
    NULL,                                                     # 22: headways
    NULL,                                                     # 23: headway_groups
    "route_id",                                               # 24: route_key
    NULL                                                      # 25: extra_trips
  )

  pos_feed <- do.call(rt2s_frequencies, args_v060)
  expect_identical(nrow(pos_feed$median$frequencies), 1L)
  expect_identical(pos_feed$median$frequencies$headway_secs, 5400L)

  # Also test baseline-anchored 25-argument positional call:
  b <- make_baseline_freq()
  args_anchored_v060 <- list(
    ev,                                                       # 1: events
    win,                                                      # 2: windows
    c(median = 0.5),                                          # 3: quantiles
    ag,                                                       # 4: agency
    st,                                                       # 5: stops
    3L,                                                       # 6: route_type
    "SVC1",                                                   # 7: service_id
    as.Date("2026-07-14"),                                    # 8: service_dates
    0L,                                                       # 9: exact_times
    "en",                                                     # 10: feed_lang
    NULL,                                                     # 11: feed_contact_email
    NULL,                                                     # 12: feed_contact_url
    FALSE,                                                    # 13: strict
    3L * 3600L,                                               # 14: max_headway_secs
    "trip_start",                                             # 15: headway_method
    NULL,                                                     # 16: reference_stops
    600L,                                                     # 17: min_revisit_gap_s
    b,                                                        # 18: baseline
    "baseline",                                               # 19: pattern_source
    make_scaling("median", 1, window = "am_peak"),            # 20: scaling
    "error",                                                  # 21: scaling_missing
    NULL,                                                     # 22: headways
    make_headway_groups(window = "am_peak"),                  # 23: headway_groups
    "route_id",                                               # 24: route_key
    NULL                                                      # 25: extra_trips
  )
  pos_anchored <- do.call(rt2s_frequencies, args_anchored_v060)
  expect_identical(nrow(pos_anchored$median$frequencies), 1L)
  expect_identical(pos_anchored$median$frequencies$headway_secs, 5400L)
})

test_that("rt2s_frequencies enforces window rules", {
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300))
  )
  st <- data.frame(
    stop_id = c("S1", "S2"),
    stop_name = c("Stop 1", "Stop 2"),
    stop_lat = c(40.0, 40.1),
    stop_lon = c(-74.0, -74.1),
    stringsAsFactors = FALSE
  )
  ag <- list(name = "Agency", url = "https://example.com", timezone = "UTC")

  # Window named 'other' errors
  expect_error(
    rt2s_frequencies(
      events = ev,
      windows = list(other = c("06:00", "09:00")),
      agency = ag,
      stops = st
    ),
    "Window name 'other' is reserved"
  )

  # Overlapping windows error under strict_within_window = TRUE
  overlap_win <- list(broad = c("06:00", "10:00"), mid = c("08:00", "09:00"))
  expect_error(
    rt2s_frequencies(
      events = ev,
      windows = overlap_win,
      agency = ag,
      stops = st,
      strict_within_window = TRUE
    ),
    "requires non-overlapping configured windows"
  )

  # Overlapping windows succeed under strict_within_window = FALSE
  feed_overlap <- rt2s_frequencies(
    events = ev,
    windows = overlap_win,
    agency = ag,
    stops = st,
    strict_within_window = FALSE
  )
  expect_identical(nrow(feed_overlap$median$frequencies), 1L)
})


test_that("rt2s_frequencies validates windows before eventless bypasses", {
  expect_error(
    supplied_feeds(windows = list(other = c("06:00", "09:00"))),
    "Window name 'other' is reserved"
  )
  for (strict in c(FALSE, TRUE)) {
    expect_error(
      rt2s_frequencies(events = make_events_clean(), strict_within_window = strict),
      "'windows' must be a non-empty named list"
    )
  }
})

test_that("eventless strict frequency construction warns that headway strictness is inert", {
  expect_warning(
    supplied_feeds(strict_within_window = TRUE),
    "no headway-estimation effect when 'events' is NULL"
  )
})
