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
test_that("overnight POSIXct stop times scaffold to >24:00:00 clock strings", {
  tz <- "Asia/Colombo"
  st <- data.frame(
    trip_id = 7L,
    vehicle_id = "7482",
    direction = 1L,
    stop_id = c("S1", "S2"),
    arrival_time = as.POSIXct(
      c("2026-07-14 23:50:10", "2026-07-15 00:05:20"),
      tz = tz
    ),
    departure_time = as.POSIXct(
      c("2026-07-14 23:50:40", "2026-07-15 00:05:45"),
      tz = tz
    )
  )

  events <- snapshot_from_stop_times(st)
  feed <- suppressWarnings(snapshot_scaffold(events, tz = tz))

  expect_identical(unique(feed$calendar_dates$date), 20260714L)
  expect_identical(feed$stop_times$arrival_time, c("23:50:10", "24:05:20"))
  expect_identical(feed$stop_times$departure_time, c("23:50:40", "24:05:45"))
})

test_that("strict mode errors on placeholder agency and missing coordinates", {
  events <- snapshot_from_stop_times(make_g2g_stop_times())

  # No agency, no stops -> strict should error (not warn)
  expect_error(
    snapshot_scaffold(events, strict = TRUE),
    "strict mode"
  )

  # Agency given but stops still missing coordinates -> still errors
  expect_error(
    snapshot_scaffold(
      events,
      agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
      strict = TRUE
    ),
    "no coordinates"
  )

  # Fully specified -> no error, no warning
  expect_no_warning(
    feed <- snapshot_scaffold(
      events,
      agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
      stops = data.frame(
        stop_id = c("S1", "S2"),
        stop_lat = c(7.30, 7.31),
        stop_lon = c(80.64, 80.65)
      ),
      route_type = 3L,
      strict = TRUE
    )
  )
  expect_s3_class(feed, "gtfs")
})

test_that("baseline mode preserves official trip_ids from provided_trip_id", {
  st <- make_g2g_stop_times()
  st$provided_trip_id <- c("CS_1", "CS_1", "CS_2", "CS_2")
  events <- snapshot_from_stop_times(st)

  baseline <- make_baseline()
  # baseline trips.txt must contain the official ids for them to survive
  baseline$trips <- data.table::data.table(
    trip_id = c("CS_1", "CS_2"),
    route_id = "B62",
    service_id = "wk",
    direction_id = c(0L, 1L)
  )

  feed <- suppressWarnings(
    snapshot_assemble(events, baseline = baseline, service_date = "2026-07-14")
  )
  expect_true(all(c("CS_1", "CS_2") %in% feed$trips$trip_id))
  expect_true(all(feed$stop_times$trip_id %in% c("CS_1", "CS_2")))
})

test_that("shapes are linked to trips.shape_id via shape_ref", {
  st <- make_g2g_stop_times() # internal trip_id 1,1,2,2
  events <- snapshot_from_stop_times(st, shape_ref_prefix = "SHP_")
  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1", "SHP_2", "SHP_2"),
    shape_pt_lat = c(40.71, 40.72, 40.72, 40.71),
    shape_pt_lon = c(-74.01, -74.02, -74.02, -74.01),
    shape_pt_sequence = c(1L, 2L, 1L, 2L),
    shape_dist_traveled = c(0, 100, 0, 100)
  )
  feed <- suppressWarnings(snapshot_scaffold(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_lat = c(40.71, 40.72),
                       stop_lon = c(-74.01, -74.02)),
    shapes = shapes,
    route_type = 3L
  ))

  expect_true("shape_id" %in% names(feed$trips))
  expect_setequal(feed$trips$shape_id, c("SHP_1", "SHP_2"))
  # referential integrity: every trip shape_id exists in shapes.txt
  expect_true(all(feed$trips$shape_id %in% feed$shapes$shape_id))
  # and no orphan shapes remain
  expect_setequal(unique(feed$shapes$shape_id), c("SHP_1", "SHP_2"))
})

test_that("a shape reference with no geometry drops shape_id and warns", {
  st <- make_g2g_stop_times()
  events <- snapshot_from_stop_times(st, shape_ref_prefix = "SHP_")
  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1"), # SHP_2 missing
    shape_pt_lat = c(40.71, 40.72),
    shape_pt_lon = c(-74.01, -74.02),
    shape_pt_sequence = c(1L, 2L)
  )
  expect_warning(
    feed <- snapshot_scaffold(
      events,
      agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
      stops = data.frame(stop_id = c("S1", "S2"),
                         stop_lat = c(40.71, 40.72),
                         stop_lon = c(-74.01, -74.02)),
      shapes = shapes,
      route_type = 3L
    ),
    "absent from 'shapes'"
  )
  # SHP_1 trip keeps its link; the SHP_2 trip has NA shape_id
  expect_true("SHP_1" %in% feed$trips$shape_id)
  expect_true(anyNA(feed$trips$shape_id))
  expect_true(all(feed$shapes$shape_id == "SHP_1"))
})

test_that("shapes without shape_ref warn and are dropped; strict errors", {
  st <- make_g2g_stop_times()
  events <- snapshot_from_stop_times(st) # no shape_ref_prefix -> shape_ref NA
  shapes <- data.frame(
    shape_id = "SHP_1", shape_pt_lat = 40.71, shape_pt_lon = -74.01,
    shape_pt_sequence = 1L
  )
  args <- list(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_lat = c(40.71, 40.72),
                       stop_lon = c(-74.01, -74.02)),
    shapes = shapes, route_type = 3L
  )
  expect_warning(
    feed <- do.call(snapshot_scaffold, args),
    "carry no shape_ref"
  )
  expect_false("shapes" %in% names(feed))
  expect_error(
    do.call(snapshot_scaffold, c(args, list(strict = TRUE))),
    "shape_ref"
  )
})

test_that("feed_lang and contact fields are written to feed_info", {
  events <- snapshot_from_stop_times(make_g2g_stop_times())
  feed <- suppressWarnings(snapshot_scaffold(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_lat = c(40.71, 40.72),
                       stop_lon = c(-74.01, -74.02)),
    route_type = 3L,
    feed_lang = "fr",
    feed_contact_email = "ops@example.org",
    feed_contact_url = "https://example.org/contact"
  ))
  expect_identical(feed$feed_info$feed_lang, "fr")
  expect_identical(feed$feed_info$feed_contact_email, "ops@example.org")
  expect_identical(feed$feed_info$feed_contact_url, "https://example.org/contact")
})

test_that("snapshot_publishable records blockers programmatically", {
  events <- snapshot_from_stop_times(make_g2g_stop_times())

  # No agency, no coords -> not publishable, two blockers
  bad <- suppressWarnings(snapshot_scaffold(events))
  st_bad <- snapshot_publishable(bad)
  expect_false(st_bad$publishable)
  expect_true(any(grepl("agency", st_bad$blockers)))
  expect_true(any(grepl("coordinates", st_bad$blockers)))

  # Fully specified -> publishable, no blockers
  good <- snapshot_scaffold(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_lat = c(7.30, 7.31),
                       stop_lon = c(80.64, 80.65)),
    route_type = 3L
  )
  st_good <- snapshot_publishable(good)
  expect_true(st_good$publishable)
  expect_length(st_good$blockers, 0L)

  # Attribute survives on the object and matches the accessor
  expect_identical(attr(good, "publishable"), TRUE)
})

test_that("baseline-mode feeds are publishable when baseline is complete", {
  events <- snapshot_from_trip_updates(make_updates())
  feed <- suppressWarnings(snapshot_assemble(events, baseline = make_baseline()))
  expect_true(snapshot_publishable(feed)$publishable)
})
