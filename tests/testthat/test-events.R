test_that("snapshot_from_stop_times converts gps2gtfs output to events", {
  events <- snapshot_from_stop_times(make_g2g_stop_times(), route_ref = "B62")

  expect_identical(nrow(events), 4L)
  expect_identical(unique(events$provenance), "observed")
  expect_identical(unique(events$source), "positions")
  expect_identical(unique(as.character(events$service_date)), "2026-07-14")
  # 1/2 direction becomes GTFS 0/1
  expect_setequal(events$direction_id, c(0L, 1L))
  # trip refs are date-suffixed synthetics, unique per trip
  expect_identical(data.table::uniqueN(events$trip_ref), 2L)
  expect_true(all(grepl("^g2g_20260714_", events$trip_ref)))
  # chronological stop_sequence per trip
  expect_identical(
    events[order(trip_ref, stop_sequence), stop_sequence],
    rep(1:2, 2)
  )
})

test_that("snapshot_from_trip_updates reduces polls and labels provenance", {
  events <- snapshot_from_trip_updates(make_updates())

  s1 <- events[stop_ref == "S1"]
  expect_identical(nrow(s1), 1L) # two polls reduced to one
  expect_identical(s1$provenance, "observed") # reported after departure
  expect_identical(format(s1$arrival_time, "%H:%M:%S"), "06:31:05")

  s2 <- events[stop_ref == "S2"]
  expect_identical(s2$provenance, "predicted-last")

  s3 <- events[stop_ref == "S3"]
  expect_identical(s3$provenance, "skipped")
  expect_true(is.na(s3$arrival_time))

  canceled <- events[provenance == "canceled"]
  expect_identical(canceled$trip_ref, "CS_9")
  expect_true(is.na(canceled$stop_ref))
})

test_that("delay-only updates require a baseline and resolve against it", {
  delay_only <- make_updates()[3, ]
  delay_only$arrival_time <- ts(NA)
  delay_only$departure_time <- ts(NA)
  delay_only$arrival_delay <- 90

  expect_error(
    snapshot_from_trip_updates(delay_only),
    "requires the baseline feed's scheduled times"
  )

  events <- snapshot_from_trip_updates(delay_only, baseline = make_baseline())
  # scheduled 06:31:30 + 90 s delay
  expect_identical(format(events$arrival_time, "%H:%M:%S"), "06:33:00")
})

test_that("validate_events catches schema violations", {
  good <- snapshot_from_stop_times(make_g2g_stop_times())
  expect_silent(validate_events(good))

  bad_prov <- data.table::copy(good)[1, provenance := "guessed"]
  expect_error(validate_events(bad_prov), "Unknown 'provenance'")

  bad_date <- data.table::copy(good)
  bad_date[, service_date := as.character(service_date)]
  expect_error(validate_events(bad_date), "must be a Date")

  bad_order <- data.table::copy(good)
  bad_order[1, departure_time := arrival_time - 60]
  expect_error(validate_events(bad_order), "must not precede")

  expect_error(
    validate_events(good[, !"trip_ref"]),
    "Missing required columns"
  )
})