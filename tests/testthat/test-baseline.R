# Expected numbers are hand-worked from the baseline fixtures in
# helper-fixtures.R. The modal rule and the first-departure rebasing are the two
# decisions worth pinning exactly: both are silent when wrong.

test_that("rt2s_baseline_patterns picks the modal signature and rebases offsets", {
  p <- rt2s_baseline_patterns(make_baseline_freq())

  # 3 trips carry "S1>S2>S3" vs 2 carrying the short turn, so the 3-stop
  # pattern wins and the short turn contributes nothing.
  expect_identical(nrow(p), 3L)
  expect_identical(p$stop_ref, c("S1", "S2", "S3"))
  expect_identical(p$route_ref, rep("R1", 3L))
  expect_identical(p$direction_id, rep(0L, 3L))

  # Template is the lowest trip_id among the winners, and it is reported.
  expect_identical(unique(p$template_trip_id), "T1")
  expect_identical(unique(p$n_pattern_trips), 3L)
  expect_identical(unique(p$n_stops), 3L)

  # stop_sequence renumbered densely from the fixture's 4:6.
  expect_identical(p$stop_sequence, 1:3)

  # Rebased on T1's first departure (06:00:00), so the origin arrival is
  # negative: its arrival precedes the departure the trip is anchored on.
  expect_identical(p$travel_base, c(-30L, 120L, 270L))
  expect_identical(p$dwell_base, c(30L, 30L, 30L))
})

test_that("rt2s_baseline_patterns tie-breaks on stop count, then lexicographically", {
  p <- rt2s_baseline_patterns(make_baseline_tied_patterns())

  # TIE1: 1 trip each, so more stops wins (B1's 3-stop pattern).
  t1 <- p[route_ref == "TIE1"]
  expect_identical(t1$stop_ref, c("S1", "S2", "S3"))
  expect_identical(unique(t1$template_trip_id), "B1")

  # TIE2: equal counts and equal stop counts, so "S1>S2" < "S1>S3" wins. Both
  # losers carry the alphabetically earlier trip_id, so a trip_id-ordered rule
  # would have picked the other pattern here.
  t2 <- p[route_ref == "TIE2"]
  expect_identical(t2$stop_ref, c("S1", "S2"))
  expect_identical(unique(t2$template_trip_id), "B2")
})

test_that("rt2s_baseline_patterns is independent of input row order", {
  b <- make_baseline_freq()
  shuffled <- b
  shuffled$stop_times <- b$stop_times[rev(seq_len(nrow(b$stop_times))), ]
  shuffled$trips <- b$trips[rev(seq_len(nrow(b$trips))), ]
  # as.data.frame() so the comparison is over values, not data.table's
  # internal self-reference attributes.
  expect_identical(
    as.data.frame(rt2s_baseline_patterns(shuffled)),
    as.data.frame(rt2s_baseline_patterns(b))
  )
})

test_that("rt2s_baseline_patterns excludes trips with missing times, not routes", {
  # T1 loses one arrival, so candidacy falls to T2/T3 and the template moves.
  expect_warning(
    p <- rt2s_baseline_patterns(make_baseline_partial_times()),
    "missing arrival or departure"
  )
  expect_identical(unique(p$template_trip_id), "T2")
  expect_identical(unique(p$n_pattern_trips), 2L)
  # T2 is rebased on its own first departure (06:10:00), giving the same shape.
  expect_identical(p$travel_base, c(-30L, 120L, 270L))
})

test_that("rt2s_baseline_patterns errors when a route-direction loses every trip", {
  b <- make_baseline_freq()
  b$stop_times$arrival_time <- NA_character_
  expect_error(
    suppressWarnings(rt2s_baseline_patterns(b)),
    "interpolate"
  )
})

test_that("rt2s_baseline_patterns requires direction_id rather than guessing it", {
  expect_error(
    rt2s_baseline_patterns(make_baseline_no_direction()),
    "direction_id"
  )
})

test_that("rt2s_baseline_patterns warns on and excludes NA direction_id", {
  b <- make_baseline_freq()
  b$trips$direction_id[b$trips$trip_id == "T1"] <- NA_integer_
  expect_warning(
    p <- rt2s_baseline_patterns(b),
    "NA 'direction_id'"
  )
  # T1 is gone, so the template falls to T2 and the winning count drops to 2.
  expect_identical(unique(p$template_trip_id), "T2")
  expect_identical(unique(p$n_pattern_trips), 2L)
})

test_that("rt2s_baseline_patterns honours min_stops", {
  # Raising the bar above the modal pattern's width drops it entirely; here the
  # only remaining candidates would be none, so it errors.
  expect_error(rt2s_baseline_patterns(make_baseline_freq(), min_stops = 4L), "usable")
  expect_error(rt2s_baseline_patterns(make_baseline_freq(), min_stops = 1L), ">= 2")
})

test_that("rt2s_baseline_patterns can key on route_short_name", {
  p <- rt2s_baseline_patterns(make_baseline_freq(), route_key = "route_short_name")
  expect_identical(unique(p$route_ref), "1")

  b <- make_baseline_freq()
  b$routes <- NULL
  expect_error(
    rt2s_baseline_patterns(b, route_key = "route_short_name"),
    "routes.txt"
  )
})

test_that("rt2s_baseline_patterns rejects malformed baseline input", {
  b <- make_baseline_freq()
  b$stop_times <- NULL
  expect_error(rt2s_baseline_patterns(b), "stop_times.txt")

  b2 <- make_baseline_freq()
  b2$trips$trip_id[2] <- "T1"
  expect_error(rt2s_baseline_patterns(b2), "duplicate 'trip_id'")

  b3 <- make_baseline_freq()
  b3$stop_times$stop_sequence[1:3] <- 4L
  expect_error(rt2s_baseline_patterns(b3), "repeated 'stop_sequence'")

  b4 <- make_baseline_freq()
  b4$stop_times$departure_time[1] <- "05:00:00"
  expect_error(rt2s_baseline_patterns(b4), "depart before they arrive")
})

test_that("rt2s_baseline_patterns accepts numeric second columns and >24h clocks", {
  b <- make_baseline_freq()
  # An overnight pattern: the template's own first departure is the anchor, so
  # offsets are unaffected by the absolute hour being past midnight.
  b$stop_times$arrival_time <- sub("^06:", "25:", b$stop_times$arrival_time)
  b$stop_times$departure_time <- sub("^06:", "25:", b$stop_times$departure_time)
  b$stop_times$arrival_time <- sub("^05:59", "24:59", b$stop_times$arrival_time)
  p <- rt2s_baseline_patterns(b)
  expect_identical(p$travel_base, c(-30L, 120L, 270L))
})

test_that("rt2s_baseline_headways summarises the planned trip-start gaps", {
  h <- rt2s_baseline_headways(
    make_baseline_freq(),
    windows = list(am_peak = c("06:00", "09:00"))
  )
  # First departures 06:00/06:10/06:25/06:35/06:50 -> gaps 600/900/600/900.
  expect_identical(nrow(h), 1L)
  expect_identical(h$headway_secs, 750L)
  expect_identical(h$n_sched_trips, 4L)
  expect_identical(h$route_ref, "R1")
  expect_identical(h$window, "am_peak")
  # Distinct from every observed headway of make_events_clean(), so a feed
  # built with it cannot be confused with a quantile-derived one.
  expect_false(h$headway_secs %in% c(612L, 720L, 1044L))

  m <- rt2s_baseline_headways(
    make_baseline_freq(),
    windows = list(am_peak = c("06:00", "09:00")),
    statistic = "mean"
  )
  expect_identical(m$headway_secs, 750L)
})

test_that("rt2s_baseline_headways splits by window and drops single departures", {
  h <- rt2s_baseline_headways(
    make_baseline_freq(),
    windows = list(early = c("06:00", "06:30"), later = c("06:30", "09:00"))
  )
  # early: 06:00/06:10/06:25 -> gaps 600, 900 -> median 750
  # later: 06:35/06:50 -> gap 900
  expect_identical(h$window, c("early", "later"))
  expect_identical(h$headway_secs, c(750L, 900L))
  expect_identical(h$n_sched_trips, c(2L, 1L))
})

test_that("rt2s_baseline_headways excludes gaps above max_headway_secs", {
  h <- rt2s_baseline_headways(
    make_baseline_freq(),
    windows = list(am_peak = c("06:00", "09:00")),
    max_headway_secs = 700L
  )
  # Only the two 600 s gaps survive the cutoff.
  expect_identical(h$headway_secs, 600L)
  expect_identical(h$n_sched_trips, 2L)
})

test_that("rt2s_baseline_headways handles overnight windows past 24:00:00", {
  b <- make_baseline_freq()
  b$stop_times$arrival_time <- sub("^06:", "25:", b$stop_times$arrival_time)
  b$stop_times$departure_time <- sub("^06:", "25:", b$stop_times$departure_time)
  b$stop_times$arrival_time <- sub("^05:59", "24:59", b$stop_times$arrival_time)
  h <- rt2s_baseline_headways(b, windows = list(overnight = c("24:00", "26:00")))
  expect_identical(h$headway_secs, 750L)
  expect_identical(h$window, "overnight")
})

test_that("rt2s_baseline_headways errors when nothing falls inside a window", {
  expect_error(
    rt2s_baseline_headways(
      make_baseline_freq(),
      windows = list(evening = c("20:00", "22:00"))
    ),
    "departs inside any"
  )
  expect_error(rt2s_baseline_headways(make_baseline_freq()), "windows")
})

# --- FR-7: expanding a baseline service into dates ---------------------------

# calendar.txt only, two services with disjoint weekday flags.
service_calendar <- function() {
  list(calendar = data.frame(
    service_id = c("WD", "SA"),
    monday = c(1L, 0L), tuesday = c(1L, 0L), wednesday = c(1L, 0L),
    thursday = c(1L, 0L), friday = c(1L, 0L),
    saturday = c(0L, 1L), sunday = 0L,
    start_date = 20260302L, end_date = 20260315L,
    stringsAsFactors = FALSE
  ))
}

test_that("rt2s_baseline_service_dates expands calendar.txt weekday flags", {
  wd <- rt2s_baseline_service_dates(service_calendar(), "WD")
  expect_s3_class(wd, "Date")
  # 2026-03-02 is a Monday; ten weekdays over the two-week span
  expect_length(wd, 10L)
  expect_identical(min(wd), as.Date("2026-03-02"))
  expect_identical(max(wd), as.Date("2026-03-13"))
  expect_false(any(as.POSIXlt(wd)$wday %in% c(0L, 6L)))

  sa <- rt2s_baseline_service_dates(service_calendar(), "SA")
  expect_identical(sa, as.Date(c("2026-03-07", "2026-03-14")))
})

test_that("rt2s_baseline_service_dates honours calendar_dates exceptions", {
  b <- service_calendar()
  b$calendar_dates <- data.frame(
    service_id = "WD",
    date = c(20260307L, 20260311L),
    exception_type = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  wd <- rt2s_baseline_service_dates(b, "WD")
  # the Saturday is added, the Wednesday removed, and the count is unchanged
  expect_true(as.Date("2026-03-07") %in% wd)
  expect_false(as.Date("2026-03-11") %in% wd)
  expect_length(wd, 10L)
  # exceptions for another service are ignored
  b$calendar_dates$service_id <- "SA"
  expect_length(rt2s_baseline_service_dates(b, "WD"), 10L)
})

test_that("rt2s_baseline_service_dates supports an exception-only service", {
  b <- list(calendar_dates = data.frame(
    service_id = "EXTRA",
    date = c(20260405L, 20260401L),
    exception_type = 1L,
    stringsAsFactors = FALSE
  ))
  # `$calendar` would partially match `calendar_dates`, so this also pins that
  # the lookup is exact.
  expect_identical(
    rt2s_baseline_service_dates(b, "EXTRA"),
    as.Date(c("2026-04-01", "2026-04-05"))
  )
})

test_that("rt2s_baseline_service_dates warns when a service resolves to nothing", {
  b <- service_calendar()
  b$calendar <- b$calendar[b$calendar$service_id == "SA", ]
  b$calendar_dates <- data.frame(
    service_id = "SA",
    date = c(20260307L, 20260314L),
    exception_type = 2L,
    stringsAsFactors = FALSE
  )
  expect_warning(
    dates <- rt2s_baseline_service_dates(b, "SA"),
    "resolves to no dates"
  )
  expect_length(dates, 0L)
  expect_s3_class(dates, "Date")
})

test_that("rt2s_baseline_service_dates rejects an unknown or ambiguous service", {
  expect_error(
    rt2s_baseline_service_dates(service_calendar(), "NOPE"),
    "'SA', 'WD'"
  )
  dup <- service_calendar()
  dup$calendar <- rbind(dup$calendar, dup$calendar[1L, ])
  expect_error(rt2s_baseline_service_dates(dup, "WD"), "at most one")
  expect_error(
    rt2s_baseline_service_dates(list(trips = data.frame(x = 1)), "WD"),
    "neither 'calendar.txt' nor 'calendar_dates.txt'"
  )
  expect_error(
    rt2s_baseline_service_dates(service_calendar(), c("WD", "SA")),
    "single non-empty"
  )
})

test_that("rt2s_baseline_service_dates feeds rt2s_frequencies(service_dates=)", {
  dates <- rt2s_baseline_service_dates(make_baseline_freq(), "weekday")
  feeds <- rt2s_frequencies(
    events = NULL,
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    baseline = make_baseline_freq(),
    pattern_source = "baseline",
    service_dates = dates,
    scaling = make_scaling("median", 1),
    headways = make_headway_overrides("median", 600L),
    headway_groups = make_headway_groups()
  )
  cal <- feeds$median$calendar
  expect_identical(cal$start_date, 20260101L)
  expect_identical(cal$saturday, 0L)
  expect_identical(cal$monday, 1L)
  # weekday-only dates leave no gap that calendar.txt does not already express
  expect_false("calendar_dates" %in% names(feeds$median))
})
