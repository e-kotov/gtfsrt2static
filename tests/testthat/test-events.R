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
test_that("snapshot_from_stop_times accepts POSIXct times, no date column", {
  tz <- "Asia/Colombo"
  st <- data.frame(
    trip_id = c(7L, 7L, 8L, 8L),
    vehicle_id = "7482",
    direction = c(1L, 1L, 2L, 2L),
    stop_id = c("S1", "S2", "S2", "S1"),
    arrival_time = as.POSIXct(
      c(
        "2026-07-14 23:50:10", "2026-07-15 00:05:20", # crosses midnight
        "2026-07-15 06:10:00", "2026-07-15 06:20:00"
      ),
      tz = tz
    ),
    departure_time = as.POSIXct(
      c(
        "2026-07-14 23:50:40", "2026-07-15 00:05:45",
        "2026-07-15 06:10:30", "2026-07-15 06:20:30"
      ),
      tz = tz
    )
  )

  events <- snapshot_from_stop_times(st)

  # The overnight trip keeps one trip_ref on the day it started; its
  # post-midnight stop does not drift to the next service date.
  t7 <- events[trip_ref == "g2g_20260714_7"]
  expect_identical(nrow(t7), 2L)
  expect_identical(unique(as.character(t7$service_date)), "2026-07-14")
  expect_identical(t7[order(stop_sequence), stop_sequence], 1:2)
  expect_identical(
    t7[order(stop_sequence), as.numeric(diff(arrival_time), units = "mins")],
    15 + 10 / 60
  )

  t8 <- events[trip_ref == "g2g_20260715_8"]
  expect_identical(nrow(t8), 2L)
  expect_identical(unique(as.character(t8$service_date)), "2026-07-15")
})

test_that("legacy clock strings repair a stop visit spanning midnight", {
  st <- make_g2g_stop_times()[1, ]
  st$arrival_time <- "23:59:30"
  st$departure_time <- "00:00:20" # wrapped behind the arrival

  events <- snapshot_from_stop_times(st)

  expect_identical(
    as.numeric(events$departure_time - events$arrival_time, units = "secs"),
    50
  )
})

test_that("mixed POSIXct/character stop times are rejected", {
  st <- make_g2g_stop_times()
  st$arrival_time <- as.POSIXct(paste(st$date, st$arrival_time), tz = "UTC")

  expect_error(snapshot_from_stop_times(st), "both be POSIXct")
})

test_that("provided_trip_id becomes the verbatim trip_ref", {
  st <- make_g2g_stop_times()
  st$provided_trip_id <- c("CS_1", "CS_1", "CS_2", "CS_2")

  events <- snapshot_from_stop_times(st, route_ref = "B62")

  # Official ids used verbatim (no g2g_ prefix) so baseline matching works
  expect_setequal(unique(events$trip_ref), c("CS_1", "CS_2"))
  expect_false(any(grepl("^g2g_", events$trip_ref)))
})

test_that("missing provided_trip_id falls back to a synthetic ref per trip", {
  st <- make_g2g_stop_times()
  st$provided_trip_id <- c("CS_1", "CS_1", NA, NA)

  events <- snapshot_from_stop_times(st)

  expect_true("CS_1" %in% events$trip_ref)
  # trip 2 (no official id) gets a date-qualified synthetic ref
  synth <- setdiff(unique(events$trip_ref), "CS_1")
  expect_length(synth, 1L)
  expect_match(synth, "^g2g_20260714_")
})

test_that("trip updates without trip_id are identified by the TripDescriptor", {
  # Two distinct operated trips, neither with a trip_id: distinguished only by
  # route_id / start_time (the GTFS-RT TripDescriptor), each with two stops.
  ts <- function(x) as.POSIXct(x, tz = "UTC")
  updates <- data.frame(
    id = "TU",
    trip_id = NA_character_,
    route_id = c("R1", "R1", "R1", "R1"),
    direction_id = 0L,
    start_date = "20260714",
    start_time = c("06:00:00", "06:00:00", "07:00:00", "07:00:00"),
    trip_schedule_relationship = "SCHEDULED",
    stop_sequence = c(1L, 2L, 1L, 2L),
    stop_id = c("S1", "S2", "S1", "S2"),
    arrival_time = ts(c(
      "2026-07-14 06:02:00", "2026-07-14 06:10:00",
      "2026-07-14 07:02:00", "2026-07-14 07:10:00"
    )),
    departure_time = ts(c(
      "2026-07-14 06:02:30", "2026-07-14 06:10:30",
      "2026-07-14 07:02:30", "2026-07-14 07:10:30"
    )),
    stop_schedule_relationship = "SCHEDULED",
    vehicle_id = c("v1", "v1", "v2", "v2"),
    file_timestamp = ts("2026-07-14 08:00:00"),
    stringsAsFactors = FALSE
  )

  events <- snapshot_from_trip_updates(updates)

  # Two trips recovered (not collapsed into one NA-trip_id blob)
  expect_identical(data.table::uniqueN(events$trip_ref), 2L)
  expect_true(all(grepl("^rtd_", events$trip_ref)))
  # descriptor difference (start_time) separated them
  expect_true(any(grepl("06:00:00", events$trip_ref)))
  expect_true(any(grepl("07:00:00", events$trip_ref)))
  # each trip has its two stops in order
  expect_identical(events[order(trip_ref, stop_sequence), stop_ref],
                   c("S1", "S2", "S1", "S2"))
})

test_that("rows with neither trip_id nor descriptor warn", {
  ts <- function(x) as.POSIXct(x, tz = "UTC")
  bad <- data.frame(
    id = "TU", trip_id = NA_character_, stop_id = "S1", stop_sequence = 1L,
    arrival_time = ts("2026-07-14 06:00:00"),
    departure_time = ts("2026-07-14 06:00:30"),
    stop_schedule_relationship = "SCHEDULED",
    file_timestamp = ts("2026-07-14 08:00:00"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    suppressMessages(try(snapshot_from_trip_updates(bad), silent = TRUE)),
    "cannot be recovered"
  )
})
