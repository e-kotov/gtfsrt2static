# Expected numbers are hand-worked in
# private/gtfsrt2static-phase0-groundtruth.md (§5), quantile type 7 with
# as.integer() truncation.

test_that("time_window classifies half-open windows, first match wins", {
  w <- list(am = c("06:00", "09:00"), mid = c("09:00", "12:00"))
  x <- as.POSIXct(
    c(
      "2026-07-14 06:00:00", # start inclusive -> am
      "2026-07-14 08:59:59", # -> am
      "2026-07-14 09:00:00", # end exclusive -> mid (not am)
      "2026-07-14 15:00:00" # -> other
    ),
    tz = "UTC"
  )
  expect_identical(time_window(x, w), c("am", "am", "mid", "other"))
  # NULL windows -> single "all" bucket; NA in -> NA out
  expect_identical(time_window(x, NULL), rep("all", 4))
  expect_identical(time_window(as.POSIXct(NA, tz = "UTC"), w), NA_character_)
  # integer seconds-of-day accepted directly
  expect_identical(time_window(c(6L * 3600L, 15L * 3600L), w), c("am", "other"))
  # empty POSIXct in -> empty out (guards the paste(character(0), ...) gotcha)
  expect_identical(
    time_window(as.POSIXct(character(0)), w, service_date = as.Date(character(0))),
    character(0)
  )
})

test_that("time_window uses service-day seconds for post-midnight service", {
  x <- as.POSIXct("2026-07-15 00:30:00", tz = "UTC") # 00:30 next calendar day
  sd <- as.Date("2026-07-14") # attributed to previous service date
  overnight <- list(overnight = c("22:00", "26:00")) # [79200, 93600)
  # wall-clock: 00:30 -> 1800s -> matches nothing -> "other"
  expect_identical(time_window(x, overnight), "other")
  # service-day: 24:30 -> 88200s -> in [79200, 93600) -> "overnight"
  expect_identical(time_window(x, overnight, service_date = sd), "overnight")
  # the exact service-day-seconds value the reviewer specified
  expect_identical(time_of_day_secs(x, service_date = sd), 88200)
})

test_that("obs_headways windows post-midnight service by service day", {
  # two runs of one trip pattern, both departing after midnight but attributed
  # to service_date 2026-07-14; 30-min headway within the overnight window.
  mk <- function(tr, start_ct) {
    arr <- start_ct + c(0, 120)
    data.frame(
      trip_ref = tr, route_ref = "R6", shape_ref = NA_character_,
      direction_id = 0L, service_date = as.Date("2026-07-14"),
      stop_ref = c("S1", "S2"), stop_sequence = NA_integer_,
      arrival_time = arr, departure_time = arr + 15L,
      provenance = "observed", vehicle_ref = "v", source = "positions",
      stringsAsFactors = FALSE
    )
  }
  ev <- rbind(
    mk("N1", as.POSIXct("2026-07-15 00:30:00", tz = "UTC")),
    mk("N2", as.POSIXct("2026-07-15 01:00:00", tz = "UTC"))
  )
  h <- obs_headways(ev, windows = list(overnight = c("22:00", "26:00")))
  expect_identical(h$window, "overnight")
  expect_identical(h$headway_median, 1800L)
  expect_identical(h$n_headways, 1L)
})

test_that("obs_headways validates max_headway_secs", {
  ev <- make_events_clean()
  expect_error(obs_headways(ev, max_headway_secs = c(1, 2)), "single positive")
  expect_error(obs_headways(ev, max_headway_secs = -1), "single positive")
  expect_error(obs_headways(ev, max_headway_secs = NA), "single positive")
})

test_that("obs_headways returns exact quantiles per window", {
  h <- obs_headways(
    make_events_clean(),
    windows = list(am_peak = c("06:00", "09:00")),
    quantiles = c(p05 = 0.05, median = 0.5, p95 = 0.95)
  )
  expect_identical(nrow(h), 1L)
  expect_identical(h$route_ref, "R1")
  expect_identical(h$window, "am_peak")
  # headways {600, 720, 1080}: p05=612, p50=720, p95=1044; 3 headways
  expect_identical(h$headway_p05, 612L)
  expect_identical(h$headway_median, 720L)
  expect_identical(h$headway_p95, 1044L)
  expect_identical(h$n_headways, 3L)
})

test_that("obs_headways ignores skipped/canceled trip starts (stale times)", {
  clean <- make_events_clean() # R1/dir0 served starts 06:00/06:10/06:22/06:40
  win <- list(am = c("06:00", "09:00"))
  base <- obs_headways(clean, windows = win)
  # a canceled trip and a skipped-only 'trip' both carrying stale timestamps
  # that fall between real starts; neither actually served a stop.
  bad <- data.frame(
    trip_ref = c("CANCEL", "SKIP"),
    route_ref = "R1", shape_ref = NA_character_, direction_id = 0L,
    service_date = as.Date("2026-07-14"),
    stop_ref = c(NA_character_, "S1"), stop_sequence = NA_integer_,
    arrival_time = as.POSIXct(
      c("2026-07-14 06:05:00", "2026-07-14 06:15:00"),
      tz = "UTC"
    ),
    departure_time = as.POSIXct(c(NA, NA), tz = "UTC"),
    provenance = c("canceled", "skipped"),
    vehicle_ref = "v", source = "trip_updates", stringsAsFactors = FALSE
  )
  ev <- data.table::rbindlist(list(clean, bad))
  h <- obs_headways(ev, windows = win)
  # stale unserved times must not manufacture extra (shorter) headways
  expect_identical(h$n_headways, base$n_headways)
  expect_identical(h$headway_median, base$headway_median)
  expect_identical(h$headway_p95, base$headway_p95)
})

test_that("obs_headways drops non-positive and over-cutoff gaps", {
  # single run -> no consecutive pair -> no row for that group
  h <- obs_headways(make_events_degenerate())
  expect_false("R3" %in% h$route_ref)
  # R4 has two runs 10 min apart -> one headway of 600s survives
  r4 <- h[route_ref == "R4"]
  expect_identical(r4$n_headways, 1L)
  expect_identical(r4$headway_median, 600L)
})

test_that("obs_travel_times returns exact quantiles and canonical order", {
  tt <- obs_travel_times(make_events_clean())
  expect_identical(tt$stop_ref, c("S1", "S2", "S3"))
  expect_identical(tt$stop_sequence, 1:3)
  # S1 is the anchor: travel 0 at every quantile
  expect_identical(tt[stop_ref == "S1", travel_p50], 0L)
  # S2 {280,300,320,360}: 283 / 310 / 354
  expect_identical(tt[stop_ref == "S2", c(travel_p05, travel_p50, travel_p95)], c(283L, 310L, 354L))
  # S3 {580,600,650,700}: 583 / 625 / 692
  expect_identical(tt[stop_ref == "S3", c(travel_p05, travel_p50, travel_p95)], c(583L, 625L, 692L))
  # dwell 30s everywhere
  expect_identical(unique(tt$dwell_median), 30L)
  expect_identical(unique(tt$n_obs), 4L)
})

test_that("obs_stop_order breaks arrival-second ties by canonical offset", {
  ord <- obs_stop_order(make_events_tied())
  expect_identical(ord$stop_ref, c("S0", "SA", "SB"))
  expect_identical(ord$stop_sequence, 1:3)
  # canonical offsets: S0=0, SA=median(100,90)=95, SB=median(100,110)=105
  expect_identical(ord$canonical_offset, c(0, 95, 105))
})

test_that("obs_stop_order warns on a stop visited twice within a trip (loop)", {
  loop <- make_events_from_offsets(
    route = "R5",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(L = "10:00:00"),
    stops = c("S1", "S2", "S1"), # S1 revisited -> loop
    offsets = list(L = c(0, 120, 300))
  )
  expect_warning(obs_stop_order(loop), "visited more than once")
})

test_that("weekday filtering is the caller's job (DECISION D)", {
  ev <- make_events_weekmix()
  # unfiltered: the Saturday run adds a spurious cross-day-type headway bucket
  # is NOT created (headways are within-date), but its trip enters travel stats
  tt_all <- obs_travel_times(ev)
  expect_identical(unique(tt_all$n_obs), 5L) # 4 weekday + 1 Saturday
  # caller restricts to weekdays -> Fixture A numbers reproduced exactly
  wd <- ev[as.POSIXlt(service_date)$wday %in% 1:5]
  tt_wd <- obs_travel_times(wd)
  expect_identical(tt_wd[stop_ref == "S2", c(travel_p05, travel_p50, travel_p95)], c(283L, 310L, 354L))
  expect_identical(unique(tt_wd$n_obs), 4L)
})

test_that("summary functions reject malformed quantiles", {
  ev <- make_events_clean()
  expect_error(obs_headways(ev, quantiles = c(0.5)), "named")
  expect_error(obs_travel_times(ev, quantiles = c(p = 1.5)), "\\[0, 1\\]")
})

test_that("always-skipped stops are excluded from travel times and stop order", {
  ev <- make_events_with_skipped()
  tt <- obs_travel_times(ev)
  # S3 is skipped on every run -> no served passage -> not in the pattern
  expect_identical(sort(unique(tt$stop_ref)), c("S1", "S2"))
  expect_false(anyNA(tt$travel_p50))
  expect_false(anyNA(tt$dwell_median))
  expect_identical(unique(tt$n_obs), 3L) # 3 served runs, S3 never counts

  ord <- obs_stop_order(ev)
  expect_identical(sort(unique(ord$stop_ref)), c("S1", "S2"))
  expect_true(all(is.finite(ord$canonical_offset)))
})

test_that("dwell_median is 0 (not NA) when no dwell was observed", {
  ev <- make_events_clean()
  ev[, departure_time := arrival_time] # zero dwell everywhere
  ev[stop_ref == "S2", departure_time := as.POSIXct(NA, tz = "UTC")]
  tt <- obs_travel_times(ev)
  # S2 arrivals are still timed -> S2 stays, its dwell collapses to 0 not NA
  expect_true("S2" %in% tt$stop_ref)
  expect_identical(tt[stop_ref == "S2", dwell_median], 0L)
  expect_false(anyNA(tt$dwell_median))
})

test_that("secs_to_clock refuses NA / negative offsets", {
  expect_identical(secs_to_clock(c(0L, 88200L)), c("00:00:00", "24:30:00"))
  expect_error(secs_to_clock(c(0L, NA_integer_)), "NA")
  expect_error(secs_to_clock(c(-1L, 10L)), "negative")
})
