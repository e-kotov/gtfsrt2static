# Expected numbers are hand-worked from the synthetic fixtures, quantile type 7
# with as.integer() truncation.

test_that("rt2s_time_window classifies half-open windows, first match wins", {
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
  expect_identical(rt2s_time_window(x, w), c("am", "am", "mid", "other"))
  # NULL windows -> single "all" bucket; NA in -> NA out
  expect_identical(rt2s_time_window(x, NULL), rep("all", 4))
  expect_identical(rt2s_time_window(as.POSIXct(NA, tz = "UTC"), w), NA_character_)
  # integer seconds-of-day accepted directly
  expect_identical(rt2s_time_window(c(6L * 3600L, 15L * 3600L), w), c("am", "other"))
  # empty POSIXct in -> empty out (guards the paste(character(0), ...) gotcha)
  expect_identical(
    rt2s_time_window(as.POSIXct(character(0)), w, service_date = as.Date(character(0))),
    character(0)
  )
})

test_that("rt2s_time_window uses service-day seconds for post-midnight service", {
  x <- as.POSIXct("2026-07-15 00:30:00", tz = "UTC") # 00:30 next calendar day
  sd <- as.Date("2026-07-14") # attributed to previous service date
  overnight <- list(overnight = c("22:00", "26:00")) # [79200, 93600)
  # wall-clock: 00:30 -> 1800s -> matches nothing -> "other"
  expect_identical(rt2s_time_window(x, overnight), "other")
  # service-day: 24:30 -> 88200s -> in [79200, 93600) -> "overnight"
  expect_identical(rt2s_time_window(x, overnight, service_date = sd), "overnight")
  # the exact service-day-seconds value the reviewer specified
  expect_identical(time_of_day_secs(x, service_date = sd), 88200)
})

test_that("rt2s_obs_headways windows post-midnight service by service day", {
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
  h <- rt2s_obs_headways(ev, windows = list(overnight = c("22:00", "26:00")))
  expect_identical(h$window, "overnight")
  expect_identical(h$headway_median, 1800L)
  expect_identical(h$n_headways, 1L)
})

test_that("rt2s_obs_headways validates max_headway_secs", {
  ev <- make_events_clean()
  expect_error(rt2s_obs_headways(ev, max_headway_secs = c(1, 2)), "single positive")
  expect_error(rt2s_obs_headways(ev, max_headway_secs = -1), "single positive")
  expect_error(rt2s_obs_headways(ev, max_headway_secs = NA), "single positive")
})

test_that("rt2s_obs_headways returns exact quantiles per window", {
  h <- rt2s_obs_headways(
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

test_that("rt2s_obs_headways ignores skipped/canceled trip starts (stale times)", {
  clean <- make_events_clean() # R1/dir0 served starts 06:00/06:10/06:22/06:40
  win <- list(am = c("06:00", "09:00"))
  base <- rt2s_obs_headways(clean, windows = win)
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
  h <- rt2s_obs_headways(ev, windows = win)
  # stale unserved times must not manufacture extra (shorter) headways
  expect_identical(h$n_headways, base$n_headways)
  expect_identical(h$headway_median, base$headway_median)
  expect_identical(h$headway_p95, base$headway_p95)
})

test_that("rt2s_obs_headways drops non-positive and over-cutoff gaps", {
  # single run -> no consecutive pair -> no row for that group
  h <- rt2s_obs_headways(make_events_degenerate())
  expect_false("R3" %in% h$route_ref)
  # R4 has two runs 10 min apart -> one headway of 600s survives
  r4 <- h[route_ref == "R4"]
  expect_identical(r4$n_headways, 1L)
  expect_identical(r4$headway_median, 600L)
})

test_that("passage mode works when trip_ref is block-style", {
  ev <- make_events_shared_trip_ref_passages()
  win <- list(am = c("06:00", "09:00"))

  expect_identical(nrow(rt2s_obs_headways(ev, windows = win)), 0L)

  h <- rt2s_obs_headways(
    ev,
    reference_stops = "S1",
    windows = win,
    quantiles = c(median = 0.5, p95 = 0.95),
    method = "passage"
  )
  expect_identical(h$route_ref, "R9")
  expect_identical(h$direction_id, 0L)
  expect_identical(h$window, "am")
  expect_identical(h$reference_stop_ref, "S1")
  expect_identical(h$headway_median, 750L) # headways {600, 900}
  expect_identical(h$headway_p95, 885L)
  expect_identical(h$n_headways, 2L)
})

test_that("passage mode auto-selects direction-unique stops", {
  h <- rt2s_obs_headways(
    make_events_auto_reference_selection(),
    windows = list(am = c("06:00", "09:00")),
    quantiles = c(median = 0.5),
    method = "passage"
  )
  data.table::setorder(h, route_ref, direction_id)
  expect_identical(h$route_ref, c("R20", "R20", "R21"))
  expect_identical(h$direction_id, c(0L, 1L, 0L))
  expect_identical(h$reference_stop_ref, c("S20_0", "S20_1", "S21_0"))
  expect_identical(h$headway_median, c(600L, 720L, 900L))
  expect_identical(h$n_headways, c(2L, 2L, 2L))
})

test_that("passage mode collapses dwell detections", {
  h <- rt2s_obs_headways(
    make_events_reference_stop_dwell(),
    reference_stops = "S1",
    windows = list(am = c("06:00", "09:00")),
    min_revisit_gap_s = 300L,
    method = "passage"
  )
  expect_identical(h$headway_median, 600L)
  expect_identical(h$headway_p95, 600L)
  expect_identical(h$n_headways, 2L)
})

test_that("passage mode separates same-vehicle revisits after the gap", {
  h <- rt2s_obs_headways(
    make_events_same_vehicle_revisit(),
    reference_stops = "S1",
    min_revisit_gap_s = 600L,
    quantiles = c(median = 0.5),
    method = "passage"
  )
  expect_identical(h$headway_median, 1200L)
  expect_identical(h$n_headways, 1L)
})

test_that("passage mode keeps chained sub-gap detections in one passage", {
  expect_warning(
    h <- rt2s_obs_headways(
      make_events_chained_subgap(),
      reference_stops = "S1",
      min_revisit_gap_s = 600L,
      method = "passage"
    ),
    "only one passage"
  )
  expect_identical(nrow(h), 0L)
})

test_that("passage mode treats missing vehicle ids as separate passages", {
  expect_warning(
    h <- rt2s_obs_headways(
      make_events_missing_vehicle_passages(),
      reference_stops = c("S30", "S31", "S32"),
      windows = list(am = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      method = "passage"
    ),
    "no 'vehicle_ref'"
  )
  data.table::setorder(h, route_ref)
  expect_identical(h$route_ref, c("R30", "R31", "R32"))
  expect_identical(h$reference_stop_ref, c("S30", "S31", "S32"))
  expect_identical(h$headway_median, c(300L, 300L, 720L))
  expect_identical(h$n_headways, c(3L, 2L, 2L))
})

test_that("passage mode drops non-positive and over-cutoff gaps", {
  h <- rt2s_obs_headways(
    make_events_passage_gap_filters(),
    reference_stops = "S1",
    max_headway_secs = 1800L,
    quantiles = c(median = 0.5),
    method = "passage"
  )
  expect_identical(h$headway_median, 1200L)
  expect_identical(h$n_headways, 1L)
})

test_that("passage mode uses an all window by default", {
  h <- rt2s_obs_headways(
    make_events_shared_trip_ref_passages(),
    reference_stops = "S1",
    quantiles = c(median = 0.5),
    method = "passage"
  )
  expect_identical(h$window, "all")
  expect_identical(h$headway_median, 750L)
})

test_that("passage mode keeps service dates separate", {
  h <- rt2s_obs_headways(
    make_events_passage_two_dates(),
    reference_stops = "S1",
    quantiles = c(median = 0.5),
    method = "passage"
  )
  expect_identical(h$headway_median, 600L)
  expect_identical(h$n_headways, 6L)
})

test_that("passage mode returns a typed empty shell for no served rows", {
  empty <- rt2s_obs_headways(
    make_events_skipped_only_trips(),
    method = "passage"
  )
  non_empty <- rt2s_obs_headways(
    make_events_shared_trip_ref_passages(),
    reference_stops = "S1",
    method = "passage"
  )
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), names(non_empty))
  expect_identical(
    vapply(empty, typeof, character(1)),
    vapply(non_empty[, names(empty), with = FALSE], typeof, character(1))
  )
})

test_that("passage mode validates min_revisit_gap_s", {
  ev <- make_events_shared_trip_ref_passages()
  expect_error(
    rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      min_revisit_gap_s = 0,
      method = "passage"
    ),
    "min_revisit_gap_s"
  )
  expect_error(
    rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      min_revisit_gap_s = -1,
      method = "passage"
    ),
    "min_revisit_gap_s"
  )
  expect_error(
    rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      min_revisit_gap_s = NA,
      method = "passage"
    ),
    "min_revisit_gap_s"
  )
  expect_error(
    rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      min_revisit_gap_s = c(1, 2),
      method = "passage"
    ),
    "min_revisit_gap_s"
  )
  expect_error(
    rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      min_revisit_gap_s = "six hundred",
      method = "passage"
    ),
    "min_revisit_gap_s"
  )
})

test_that("passage mode warns when reference stops miss groups", {
  ev <- make_events_missing_reference_group()
  win <- list(am = c("06:00", "09:00"))
  trip_start <- rt2s_obs_headways(ev, windows = win, quantiles = c(median = 0.5))
  expect_setequal(trip_start$route_ref, c("RA", "RB"))

  expect_warning(
    passage <- rt2s_obs_headways(
      ev,
      reference_stops = "SA",
      windows = win,
      quantiles = c(median = 0.5),
      method = "passage"
    ),
    "route_ref=RB, direction_id=0"
  )
  expect_identical(unique(passage$route_ref), "RA")
})

test_that("passage mode warns when every group has one passage", {
  events <- rt2s_events_from_trip_updates(make_updates())
  expect_warning(
    h <- rt2s_obs_headways(events, reference_stops = "S1", method = "passage"),
    "only one passage"
  )
  expect_identical(nrow(h), 0L)
})

test_that("passage mode warns when mixed groups have no headways", {
  events <- data.table::rbindlist(list(
    make_reference_passage_events(
      route = "R40",
      direction_id = 0L,
      times = "06:00:00",
      stop_ref = "S40",
      vehicle_ref = "v1",
      trip_ref = "block_R40"
    ),
    make_reference_passage_events(
      route = "R41",
      direction_id = 0L,
      times = c("06:00:00", "06:20:00"),
      stop_ref = "S41",
      vehicle_ref = c("v1", "v2"),
      trip_ref = "block_R41"
    )
  ))
  expect_warning(
    h <- rt2s_obs_headways(
      events,
      reference_stops = c("S40", "S41"),
      max_headway_secs = 600L,
      method = "passage"
    ),
    "No route-direction/date/reference-stop group produced a usable passage"
  )
  expect_identical(nrow(h), 0L)
})

test_that("passage mode rejects shared reference stops", {
  ev <- make_events_shared_reference_stop()
  expect_error(
    rt2s_obs_headways(ev, reference_stops = "S_shared", method = "passage"),
    "direction-unique"
  )
  expect_error(
    rt2s_obs_headways(ev, method = "passage"),
    "No direction-unique reference stop"
  )
})

test_that("passage mode handles unknown direction rows explicitly", {
  ev <- make_events_partly_unknown_direction()
  expect_warning(
    explicit <- rt2s_obs_headways(
      ev,
      reference_stops = "S1",
      windows = list(am = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      method = "passage"
    ),
    "unknown 'direction_id'"
  )
  expect_identical(nrow(explicit), 1L)
  expect_identical(explicit$route_ref, "R36")
  expect_identical(explicit$direction_id, 0L)
  expect_identical(explicit$headway_median, 600L)

  expect_warning(
    automatic <- rt2s_obs_headways(
      ev,
      windows = list(am = c("06:00", "09:00")),
      quantiles = c(median = 0.5),
      method = "passage"
    ),
    "unknown 'direction_id'"
  )
  expect_identical(nrow(automatic), 1L)
  expect_identical(automatic$route_ref, "R36")
  expect_identical(automatic$direction_id, 0L)
  expect_identical(automatic$reference_stop_ref, "S1")
  expect_identical(automatic$headway_median, 600L)
})

test_that("passage mode errors when every candidate direction is unknown", {
  ev <- make_events_all_unknown_direction()
  expect_warning(
    expect_error(
      rt2s_obs_headways(
        ev,
        reference_stops = "S1",
        windows = list(am = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        method = "passage"
      ),
      "No direction-unique reference stop is available after excluding"
    ),
    "unknown 'direction_id'"
  )

  expect_warning(
    expect_error(
      rt2s_obs_headways(
        ev,
        windows = list(am = c("06:00", "09:00")),
        quantiles = c(median = 0.5),
        method = "passage"
      ),
      "No direction-unique reference stop is available after excluding"
    ),
    "unknown 'direction_id'"
  )
})

test_that("rt2s_obs_headways(method = \"passage\") matches trip_start columns", {
  win <- list(am_peak = c("06:00", "09:00"))
  trip_start <- rt2s_obs_headways(make_events_clean(), windows = win)
  passage <- rt2s_obs_headways(
    make_events_clean(),
    reference_stops = "S1",
    windows = win,
    method = "passage"
  )
  expected_cols <- c(
    "route_ref",
    "direction_id",
    "window",
    "headway_median",
    "headway_p95",
    "n_headways"
  )
  expect_true(all(expected_cols %in% names(passage)))
  expect_identical(
    vapply(passage[, ..expected_cols], typeof, character(1)),
    vapply(trip_start[, ..expected_cols], typeof, character(1))
  )
})

test_that("passage and trip-start values coincide for the clean anchor fixture", {
  win <- list(am_peak = c("06:00", "09:00"))
  trip_start <- rt2s_obs_headways(make_events_clean(), windows = win)
  passage <- rt2s_obs_headways(
    make_events_clean(),
    reference_stops = "S1",
    windows = win,
    method = "passage"
  )
  expected_cols <- c(
    "route_ref",
    "direction_id",
    "window",
    "headway_median",
    "headway_p95",
    "n_headways"
  )
  # Values match because S1 is the first stop and every run has its own vehicle.
  expect_identical(
    passage[, ..expected_cols],
    trip_start[, ..expected_cols]
  )
})

test_that("method = dispatches to the two paths without altering either", {
  # 0.5.0 folded obs_headways_by_passage() into method = "passage". The entry
  # point changed; neither implementation did. These pin that.
  ev <- make_events_shared_trip_ref_passages()
  win <- list(am = c("06:00", "09:00"))

  expect_identical(
    rt2s_obs_headways(
      ev,
      windows = win,
      quantiles = c(median = 0.5, p95 = 0.95),
      method = "passage",
      reference_stops = "S1",
      min_revisit_gap_s = 600L
    ),
    headways_by_passage(
      ev,
      reference_stops = "S1",
      windows = win,
      quantiles = c(median = 0.5, p95 = 0.95),
      min_revisit_gap_s = 600L
    )
  )

  clean <- make_events_clean()
  expect_identical(
    rt2s_obs_headways(clean, windows = win),
    headways_by_trip_start(clean, windows = win)
  )
  expect_identical(
    rt2s_obs_headways(clean, windows = win, method = "trip_start"),
    headways_by_trip_start(clean, windows = win)
  )
})

test_that("passage-only arguments warn under method = \"trip_start\"", {
  ev <- make_events_clean()
  win <- list(am_peak = c("06:00", "09:00"))

  expect_warning(
    rt2s_obs_headways(ev, windows = win, reference_stops = "S1"),
    "'reference_stops' ignored when method = \"trip_start\""
  )
  expect_warning(
    rt2s_obs_headways(ev, windows = win, min_revisit_gap_s = 900L),
    "'min_revisit_gap_s' ignored when method = \"trip_start\""
  )
  expect_warning(
    rt2s_obs_headways(
      ev,
      windows = win,
      reference_stops = "S1",
      min_revisit_gap_s = 900L
    ),
    "'reference_stops', 'min_revisit_gap_s' ignored"
  )

  # Leaving them at their defaults is silent, and passing them under
  # method = "passage" is what they are for.
  expect_silent(rt2s_obs_headways(ev, windows = win))
  expect_silent(
    rt2s_obs_headways(
      ev,
      windows = win,
      method = "passage",
      reference_stops = "S1",
      min_revisit_gap_s = 900L
    )
  )
})

test_that("rt2s_obs_travel_times returns exact quantiles and canonical order", {
  tt <- rt2s_obs_travel_times(make_events_clean())
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

test_that("rt2s_obs_stop_order breaks arrival-second ties by canonical offset", {
  ord <- rt2s_obs_stop_order(make_events_tied())
  expect_identical(ord$stop_ref, c("S0", "SA", "SB"))
  expect_identical(ord$stop_sequence, 1:3)
  # canonical offsets: S0=0, SA=median(100,90)=95, SB=median(100,110)=105
  expect_identical(ord$canonical_offset, c(0, 95, 105))
})

test_that("rt2s_obs_stop_order warns on a stop visited twice within a trip (loop)", {
  loop <- make_events_from_offsets(
    route = "R5",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(L = "10:00:00"),
    stops = c("S1", "S2", "S1"), # S1 revisited -> loop
    offsets = list(L = c(0, 120, 300))
  )
  expect_warning(rt2s_obs_stop_order(loop), "visited more than once")
})

test_that("weekday filtering is the caller's job (DECISION D)", {
  ev <- make_events_weekmix()
  # unfiltered: the Saturday run adds a spurious cross-day-type headway bucket
  # is NOT created (headways are within-date), but its trip enters travel stats
  tt_all <- rt2s_obs_travel_times(ev)
  expect_identical(unique(tt_all$n_obs), 5L) # 4 weekday + 1 Saturday
  # caller restricts to weekdays -> Fixture A numbers reproduced exactly
  wd <- ev[as.POSIXlt(service_date)$wday %in% 1:5]
  tt_wd <- rt2s_obs_travel_times(wd)
  expect_identical(tt_wd[stop_ref == "S2", c(travel_p05, travel_p50, travel_p95)], c(283L, 310L, 354L))
  expect_identical(unique(tt_wd$n_obs), 4L)
})

test_that("summary functions reject malformed quantiles", {
  ev <- make_events_clean()
  expect_error(rt2s_obs_headways(ev, quantiles = c(0.5)), "named")
  expect_error(rt2s_obs_travel_times(ev, quantiles = c(p = 1.5)), "\\[0, 1\\]")
})

test_that("always-skipped stops are excluded from travel times and stop order", {
  ev <- make_events_with_skipped()
  tt <- rt2s_obs_travel_times(ev)
  # S3 is skipped on every run -> no served passage -> not in the pattern
  expect_identical(sort(unique(tt$stop_ref)), c("S1", "S2"))
  expect_false(anyNA(tt$travel_p50))
  expect_false(anyNA(tt$dwell_median))
  expect_identical(unique(tt$n_obs), 3L) # 3 served runs, S3 never counts

  ord <- rt2s_obs_stop_order(ev)
  expect_identical(sort(unique(ord$stop_ref)), c("S1", "S2"))
  expect_true(all(is.finite(ord$canonical_offset)))
})

test_that("dwell_median is 0 (not NA) when no dwell was observed", {
  ev <- make_events_clean()
  ev[, departure_time := arrival_time] # zero dwell everywhere
  ev[stop_ref == "S2", departure_time := as.POSIXct(NA, tz = "UTC")]
  tt <- rt2s_obs_travel_times(ev)
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

test_that("rt2s_obs_headways validates strict_within_window", {
  ev <- make_events_clean()
  expect_error(
    rt2s_obs_headways(ev, strict_within_window = "yes"),
    "'strict_within_window' must be TRUE or FALSE"
  )
  expect_error(
    rt2s_obs_headways(ev, strict_within_window = NA),
    "'strict_within_window' must be TRUE or FALSE"
  )
  expect_error(
    rt2s_obs_headways(ev, strict_within_window = c(TRUE, FALSE)),
    "'strict_within_window' must be TRUE or FALSE"
  )
})

test_that("strict_within_window excludes cross-boundary intervals under method = 'trip_start'", {
  # 3 trips:
  # T1 @ 07:00:00 (am_peak)
  # T2 @ 08:30:00 (am_peak)
  # T3 @ 09:30:00 (midday)
  # Windows: am_peak = c("06:00", "09:00"), midday = c("09:00", "12:00")
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00", T3 = "09:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )
  win <- list(am_peak = c("06:00", "09:00"), midday = c("09:00", "12:00"))

  # Under default / strict_within_window = FALSE:
  # Interval 1 (T1 -> T2): 08:30 - 07:00 = 5400s (assigned to am_peak)
  # Interval 2 (T2 -> T3): 09:30 - 08:30 = 3600s (assigned to midday)
  legacy <- rt2s_obs_headways(ev, windows = win, strict_within_window = FALSE)
  expect_identical(nrow(legacy), 2L)
  expect_identical(legacy[window == "am_peak", headway_median], 5400L)
  expect_identical(legacy[window == "am_peak", n_headways], 1L)
  expect_identical(legacy[window == "midday", headway_median], 3600L)
  expect_identical(legacy[window == "midday", n_headways], 1L)

  # Under strict_within_window = TRUE:
  # am_peak: T1 (07:00) and T2 (08:30) -> 1 interval of 5400s
  # midday: T3 (09:30) is lone trip in window -> 0 intervals (dropped from summary)
  # The 08:30 -> 09:30 interval is across the 09:00 window boundary and is discarded.
  strict <- rt2s_obs_headways(ev, windows = win, strict_within_window = TRUE)
  expect_identical(nrow(strict), 1L)
  expect_identical(strict$window, "am_peak")
  expect_identical(strict$headway_median, 5400L)
  expect_identical(strict$n_headways, 1L)
})

test_that("strict_within_window excludes cross-boundary intervals under method = 'passage'", {
  # 3 passages at reference stop S1:
  # V1 passage @ 07:15:00 (am_peak)
  # V2 passage @ 08:45:00 (am_peak)
  # V3 passage @ 09:45:00 (midday)
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00", T3 = "09:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(900, 1200), T2 = c(900, 1200), T3 = c(900, 1200))
  )
  win <- list(am_peak = c("06:00", "09:00"), midday = c("09:00", "12:00"))

  # Under default / strict_within_window = FALSE:
  legacy <- rt2s_obs_headways(
    ev,
    windows = win,
    method = "passage",
    reference_stops = "S1",
    strict_within_window = FALSE
  )
  expect_identical(nrow(legacy), 2L)
  expect_identical(legacy[window == "am_peak", headway_median], 5400L)
  expect_identical(legacy[window == "am_peak", n_headways], 1L)
  expect_identical(legacy[window == "midday", headway_median], 3600L)
  expect_identical(legacy[window == "midday", n_headways], 1L)

  # Under strict_within_window = TRUE:
  strict <- rt2s_obs_headways(
    ev,
    windows = win,
    method = "passage",
    reference_stops = "S1",
    strict_within_window = TRUE
  )
  expect_identical(nrow(strict), 1L)
  expect_identical(strict$window, "am_peak")
  expect_identical(strict$headway_median, 5400L)
  expect_identical(strict$n_headways, 1L)
})

test_that("strict_within_window discards 'other' window events before interval calculation", {
  # T0 @ 05:00:00 (outside windows -> 'other')
  # T1 @ 07:00:00 (am_peak)
  # T2 @ 08:00:00 (am_peak)
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T0 = "05:00:00", T1 = "07:00:00", T2 = "08:00:00"),
    stops = c("S1", "S2"),
    offsets = list(T0 = c(0, 300), T1 = c(0, 300), T2 = c(0, 300))
  )
  win <- list(am_peak = c("06:00", "09:00"))

  # Under strict_within_window = TRUE:
  # T0 (05:00) is in 'other' and discarded.
  # T1 (07:00) and T2 (08:00) are in am_peak -> 1 headway of 3600s (07:00 -> 08:00).
  # Gap from 05:00 -> 07:00 (7200s) does not enter am_peak.
  strict <- rt2s_obs_headways(ev, windows = win, strict_within_window = TRUE)
  expect_identical(nrow(strict), 1L)
  expect_identical(strict$window, "am_peak")
  expect_identical(strict$headway_median, 3600L)
  expect_identical(strict$n_headways, 1L)
})

test_that("strict_within_window preserves post-midnight service day attribution", {
  # 2 trips on same service date 2026-07-14:
  # T1 @ 23:30:00 (2026-07-14 23:30:00)
  # T2 @ 24:30:00 (2026-07-15 00:30:00, attributed to 2026-07-14)
  # Overnight window: c("22:00", "26:00")
  ev1 <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "23:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300))
  )
  ev2 <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T2 = "00:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T2 = c(0, 300))
  )
  # Set T2's actual arrival timestamp to 2026-07-15 00:30:00 while keeping service_date = 2026-07-14
  ev2$arrival_time <- ev2$arrival_time + 86400
  ev2$departure_time <- ev2$departure_time + 86400
  ev <- data.table::rbindlist(list(ev1, ev2))

  win <- list(overnight = c("22:00", "26:00"))
  strict <- rt2s_obs_headways(ev, windows = win, strict_within_window = TRUE)
  expect_identical(nrow(strict), 1L)
  expect_identical(strict$window, "overnight")
  expect_identical(strict$headway_median, 3600L)
  expect_identical(strict$n_headways, 1L)
})

test_that("rt2s_time_window and rt2s_obs_headways reserve 'other' window name", {
  x <- as.POSIXct("2026-07-14 07:30:00", tz = "UTC")
  expect_error(
    rt2s_time_window(x, windows = list(other = c("06:00", "09:00"))),
    "Window name 'other' is reserved"
  )

  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:00:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300))
  )
  expect_error(
    rt2s_obs_headways(ev, windows = list(other = c("06:00", "09:00"))),
    "Window name 'other' is reserved"
  )
})

test_that("strict_within_window rejects overlapping windows but legacy allows them", {
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "06:30:00", T2 = "08:30:00", T3 = "09:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )
  overlap_win <- list(broad = c("06:00", "10:00"), mid = c("08:00", "09:00"))

  # Trip start method:
  expect_error(
    rt2s_obs_headways(
      ev,
      windows = overlap_win,
      method = "trip_start",
      strict_within_window = TRUE
    ),
    "requires non-overlapping configured windows"
  )
  legacy_ts <- rt2s_obs_headways(
    ev,
    windows = overlap_win,
    method = "trip_start",
    strict_within_window = FALSE
  )
  expect_true(nrow(legacy_ts) > 0L)

  # Passage method:
  expect_error(
    rt2s_obs_headways(
      ev,
      windows = overlap_win,
      method = "passage",
      reference_stops = "S1",
      strict_within_window = TRUE
    ),
    "requires non-overlapping configured windows"
  )
  legacy_ps <- rt2s_obs_headways(
    ev,
    windows = overlap_win,
    method = "passage",
    reference_stops = "S1",
    strict_within_window = FALSE
  )
  expect_true(nrow(legacy_ps) > 0L)
})

test_that("strict_within_window handles windows = NULL and edge cases", {
  ev <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:00:00", T3 = "08:00:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )

  # windows = NULL with strict_within_window = TRUE -> window = "all"
  strict_null_ts <- rt2s_obs_headways(
    ev,
    windows = NULL,
    method = "trip_start",
    strict_within_window = TRUE
  )
  expect_identical(strict_null_ts$window, "all")
  # T2 and T3 are at same time (0s headway) -> filtered out; only T1->T2 (3600s) counted
  expect_identical(strict_null_ts$n_headways, 1L)
  expect_identical(strict_null_ts$headway_median, 3600L)

  strict_null_ps <- rt2s_obs_headways(
    ev,
    windows = NULL,
    method = "passage",
    reference_stops = "S1",
    strict_within_window = TRUE
  )
  expect_identical(strict_null_ps$window, "all")

  # Passage method across midnight with strict_within_window = TRUE:
  ev1 <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "23:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300))
  )
  ev2 <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T2 = "00:30:00"),
    stops = c("S1", "S2"),
    offsets = list(T2 = c(0, 300))
  )
  ev2$arrival_time <- ev2$arrival_time + 86400
  ev2$departure_time <- ev2$departure_time + 86400
  ev_night <- data.table::rbindlist(list(ev1, ev2))

  strict_night_ps <- rt2s_obs_headways(
    ev_night,
    windows = list(overnight = c("22:00", "26:00")),
    method = "passage",
    reference_stops = "S1",
    strict_within_window = TRUE
  )
  expect_identical(nrow(strict_night_ps), 1L)
  expect_identical(strict_night_ps$window, "overnight")
  expect_identical(strict_night_ps$headway_median, 3600L)
})


test_that("reserved 'other' is rejected before empty-passage short-circuiting", {
  expect_error(
    rt2s_obs_headways(
      make_events_skipped_only_trips(),
      windows = list(other = c("06:00", "09:00")),
      method = "passage"
    ),
    "Window name 'other' is reserved"
  )
})

test_that("strict mode rejects one reversed window for both headway methods", {
  ev <- make_events_from_offsets(
    route = "R1", direction_id = 0L, date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:00:00"), stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300))
  )
  bad <- list(am = c("09:00", "06:00"))
  expect_error(
    rt2s_obs_headways(ev, windows = bad, strict_within_window = TRUE),
    "must be strictly after"
  )
  expect_error(
    rt2s_obs_headways(
      ev, windows = bad, method = "passage", reference_stops = "S1",
      strict_within_window = TRUE
    ),
    "must be strictly after"
  )
})

test_that("strict mode retains later-window gaps and honours half-open boundaries", {
  ev <- make_events_from_offsets(
    route = "R1", direction_id = 0L, date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:30:00", T3 = "09:00:00", T4 = "10:00:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300), T4 = c(0, 300))
  )
  win <- list(am = c("06:00", "09:00"), later = c("09:00", "12:00"))
  trip_start <- rt2s_obs_headways(ev, windows = win, strict_within_window = TRUE)
  data.table::setorder(trip_start, window)
  expect_identical(trip_start$window, c("am", "later"))
  expect_identical(trip_start$headway_median, c(5400L, 3600L))

  passage <- rt2s_obs_headways(
    ev, windows = win, method = "passage", reference_stops = "S1",
    strict_within_window = TRUE
  )
  data.table::setorder(passage, window)
  expect_identical(passage$window, c("am", "later"))
  expect_identical(passage$headway_median, c(5400L, 3600L))
})

test_that("strict window validation detects overnight overlap and accepts adjacency", {
  expect_error(
    check_strict_windows(list(overnight = c("23:00", "26:00"), after = c("25:00", "27:00"))),
    "requires non-overlapping configured windows"
  )
  expect_silent(
    check_strict_windows(list(overnight = c("23:00", "25:00"), after = c("25:00", "27:00")))
  )
})

test_that("strict passage mode preserves dwell/revisit collapse and deterministic ties", {
  dwell <- rt2s_obs_headways(
    make_events_reference_stop_dwell(), reference_stops = "S1",
    windows = list(am = c("06:00", "09:00")), min_revisit_gap_s = 300L,
    method = "passage", strict_within_window = TRUE
  )
  expect_identical(dwell$headway_median, 600L)
  expect_identical(dwell$n_headways, 2L)

  tie <- make_events_from_offsets(
    route = "R1", direction_id = 0L, date = "2026-07-14",
    starts = c(T1 = "07:00:00", T2 = "08:00:00", T3 = "08:00:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 300), T2 = c(0, 300), T3 = c(0, 300))
  )
  tied <- rt2s_obs_headways(
    tie, windows = list(am = c("06:00", "09:00")), method = "passage",
    reference_stops = "S1", strict_within_window = TRUE
  )
  expect_identical(tied$n_headways, 1L)
  expect_identical(tied$headway_median, 3600L)
})
