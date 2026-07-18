test_that("snapshot_scaffold builds a linked, required-files-complete feed", {
  events <- snapshot_from_trip_updates(make_updates())
  stops <- make_baseline()$stops

  expect_warning(
    feed <- snapshot_scaffold(
      events,
      agency = list(
        name = "Example Transit",
        url = "https://example-transit.org",
        timezone = "UTC"
      ),
      stops = stops
    ),
    NA
  )

  expect_s3_class(feed, "gtfs")
  expect_true(all(
    c(
      "agency",
      "stops",
      "routes",
      "trips",
      "stop_times",
      "calendar_dates",
      "feed_info"
    ) %in%
      names(feed)
  ))

  # Internal ID links hold
  expect_true(all(feed$trips$route_id %in% feed$routes$route_id))
  expect_true(all(feed$trips$service_id %in% feed$calendar_dates$service_id))
  expect_true(all(feed$stop_times$trip_id %in% feed$trips$trip_id))
  expect_true(all(feed$stop_times$stop_id %in% feed$stops$stop_id))
  expect_true(all(feed$routes$agency_id %in% feed$agency$agency_id))

  # Canceled and skipped events never reach stop_times
  expect_false("S3" %in% feed$stop_times$stop_id)
  expect_false("CS_9" %in% feed$trips$trip_id)

  # stop_sequence increases within trips
  seqs <- feed$stop_times[, .(ok = !is.unsorted(stop_sequence)), by = trip_id]
  expect_true(all(seqs$ok))
})

test_that("scaffold warns on placeholders and missing coordinates", {
  events <- snapshot_from_stop_times(make_g2g_stop_times())
  warns <- capture_warnings(feed <- snapshot_scaffold(events))
  expect_match(warns, "placeholder", all = FALSE)
  expect_match(warns, "no coordinates", all = FALSE)
  expect_true(all(is.na(feed$stops$stop_lat)))
})

test_that("post-midnight stops render as >24:00:00 clock strings", {
  events <- snapshot_from_stop_times(make_g2g_stop_times())
  # push one stop past midnight while keeping its service date
  events[1, arrival_time := as.POSIXct("2026-07-15 00:07:52", tz = "UTC")]
  events[1, departure_time := as.POSIXct("2026-07-15 00:08:15", tz = "UTC")]
  events[1, stop_sequence := 99L]

  feed <- suppressWarnings(snapshot_scaffold(events))
  late <- feed$stop_times[stop_sequence == 99L]
  expect_identical(late$arrival_time, "24:07:52")
  expect_identical(late$departure_time, "24:08:15")
})

test_that("scaffold round-trips through gtfsio export/import", {
  events <- snapshot_from_trip_updates(make_updates())
  feed <- suppressWarnings(snapshot_scaffold(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = make_baseline()$stops
  ))

  zip_path <- tempfile(fileext = ".zip")
  gtfsio::export_gtfs(feed, zip_path)
  back <- gtfsio::import_gtfs(zip_path)

  expect_setequal(names(back), names(feed))
  expect_identical(nrow(back$stop_times), nrow(feed$stop_times))
  expect_identical(
    sort(as.character(back$trips$trip_id)),
    sort(as.character(feed$trips$trip_id))
  )
})

test_that("snapshot_assemble in baseline mode keeps official ids", {
  events <- snapshot_from_trip_updates(make_updates())

  expect_warning(
    feed <- snapshot_assemble(events, baseline = make_baseline()),
    NA
  )

  # Official trip id preserved; canceled CS_9 excluded; calendar replaced
  expect_identical(as.character(feed$trips$trip_id), "CS_1")
  expect_identical(feed$trips$service_id, "SVC_20260714")
  expect_false("calendar" %in% names(feed))
  expect_identical(feed$calendar_dates$date, 20260714L)

  # Realized times replace scheduled ones
  s1 <- feed$stop_times[stop_id == "S1"]
  expect_identical(s1$arrival_time, "06:31:05")
  # Baseline numbering (join-aligned): S1 keeps its scheduled sequence 4
  expect_identical(s1$stop_sequence, 4L)

  # Inherited wholesale
  expect_identical(nrow(feed$stops), 3L)
  expect_identical(as.character(feed$routes$route_id), "B62")
})

test_that("snapshot_assemble errors and warns usefully", {
  events <- snapshot_from_trip_updates(make_updates())

  # multi-date events require an explicit service_date
  two_days <- data.table::copy(events)
  two_days[1, service_date := service_date + 1]
  expect_error(
    snapshot_assemble(two_days, baseline = make_baseline()),
    "Pass 'service_date'"
  )

  # unmatched observed trips are dropped with a warning
  renamed <- data.table::copy(events)
  renamed[trip_ref == "CS_1", trip_ref := "UNKNOWN_TRIP"]
  expect_error(
    suppressWarnings(
      snapshot_assemble(renamed, baseline = make_baseline())
    ),
    "None of the observed trips match"
  )

  # scaffold path is used when no baseline is given
  feed <- suppressWarnings(snapshot_assemble(events))
  expect_s3_class(feed, "gtfs")
  expect_true("calendar_dates" %in% names(feed))
})