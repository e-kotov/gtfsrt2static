test_that("rt2s_scaffold builds a linked, required-files-complete feed", {
  events <- rt2s_events_from_trip_updates(make_updates())
  stops <- make_baseline()$stops

  expect_warning(
    feed <- rt2s_scaffold(
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
  events <- rt2s_events_from_stop_times(make_g2g_stop_times())
  warns <- capture_warnings(feed <- rt2s_scaffold(events))
  expect_match(warns, "placeholder", all = FALSE)
  expect_match(warns, "no coordinates", all = FALSE)
  expect_true(all(is.na(feed$stops$stop_lat)))
})

test_that("post-midnight stops render as >24:00:00 clock strings", {
  events <- rt2s_events_from_stop_times(make_g2g_stop_times())
  # push one stop past midnight while keeping its service date
  events[1, arrival_time := as.POSIXct("2026-07-15 00:07:52", tz = "UTC")]
  events[1, departure_time := as.POSIXct("2026-07-15 00:08:15", tz = "UTC")]
  events[1, stop_sequence := 99L]

  feed <- suppressWarnings(rt2s_scaffold(events))
  late <- feed$stop_times[stop_sequence == 99L]
  expect_identical(late$arrival_time, "24:07:52")
  expect_identical(late$departure_time, "24:08:15")
})

test_that("scaffold round-trips through gtfsio export/import", {
  events <- rt2s_events_from_trip_updates(make_updates())
  feed <- suppressWarnings(rt2s_scaffold(
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

test_that("rt2s_assemble in baseline mode keeps official ids", {
  events <- rt2s_events_from_trip_updates(make_updates())

  expect_warning(
    feed <- rt2s_assemble(events, baseline = make_baseline()),
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

test_that("rt2s_assemble errors and warns usefully", {
  events <- rt2s_events_from_trip_updates(make_updates())

  # multi-date events require an explicit service_date
  two_days <- data.table::copy(events)
  two_days[1, service_date := service_date + 1]
  expect_error(
    rt2s_assemble(two_days, baseline = make_baseline()),
    "Pass 'service_date'"
  )

  # unmatched observed trips are dropped with a warning
  renamed <- data.table::copy(events)
  renamed[trip_ref == "CS_1", trip_ref := "UNKNOWN_TRIP"]
  expect_error(
    suppressWarnings(
      rt2s_assemble(renamed, baseline = make_baseline())
    ),
    "None of the observed trips match"
  )

  # scaffold path is used when no baseline is given
  feed <- suppressWarnings(rt2s_assemble(events))
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

  events <- rt2s_events_from_stop_times(st)
  feed <- suppressWarnings(rt2s_scaffold(events, tz = tz))

  expect_identical(unique(feed$calendar_dates$date), 20260714L)
  expect_identical(feed$stop_times$arrival_time, c("23:50:10", "24:05:20"))
  expect_identical(feed$stop_times$departure_time, c("23:50:40", "24:05:45"))
})

test_that("strict mode errors on placeholder agency and missing coordinates", {
  events <- rt2s_events_from_stop_times(make_g2g_stop_times())

  # No agency, no stops -> strict should error (not warn)
  expect_error(
    rt2s_scaffold(events, strict = TRUE),
    "strict mode"
  )

  # Agency given but stops still missing coordinates -> still errors
  expect_error(
    rt2s_scaffold(
      events,
      agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
      strict = TRUE
    ),
    "no coordinates"
  )

  # Fully specified -> no error, no warning
  expect_no_warning(
    feed <- rt2s_scaffold(
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
  events <- rt2s_events_from_stop_times(st)

  baseline <- make_baseline()
  # baseline trips.txt must contain the official ids for them to survive
  baseline$trips <- data.table::data.table(
    trip_id = c("CS_1", "CS_2"),
    route_id = "B62",
    service_id = "wk",
    direction_id = c(0L, 1L)
  )

  feed <- suppressWarnings(
    rt2s_assemble(events, baseline = baseline, service_date = "2026-07-14")
  )
  expect_true(all(c("CS_1", "CS_2") %in% feed$trips$trip_id))
  expect_true(all(feed$stop_times$trip_id %in% c("CS_1", "CS_2")))
})

test_that("shapes are linked to trips.shape_id via shape_ref", {
  st <- make_g2g_stop_times() # internal trip_id 1,1,2,2
  events <- rt2s_events_from_stop_times(st, shape_ref_prefix = "SHP_")
  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1", "SHP_2", "SHP_2"),
    shape_pt_lat = c(40.71, 40.72, 40.72, 40.71),
    shape_pt_lon = c(-74.01, -74.02, -74.02, -74.01),
    shape_pt_sequence = c(1L, 2L, 1L, 2L),
    shape_dist_traveled = c(0, 100, 0, 100)
  )
  feed <- suppressWarnings(rt2s_scaffold(
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
  events <- rt2s_events_from_stop_times(st, shape_ref_prefix = "SHP_")
  shapes <- data.frame(
    shape_id = c("SHP_1", "SHP_1"), # SHP_2 missing
    shape_pt_lat = c(40.71, 40.72),
    shape_pt_lon = c(-74.01, -74.02),
    shape_pt_sequence = c(1L, 2L)
  )
  expect_warning(
    feed <- rt2s_scaffold(
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
  events <- rt2s_events_from_stop_times(st) # no shape_ref_prefix -> shape_ref NA
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
    feed <- do.call(rt2s_scaffold, args),
    "carry no shape_ref"
  )
  expect_false("shapes" %in% names(feed))
  expect_error(
    do.call(rt2s_scaffold, c(args, list(strict = TRUE))),
    "shape_ref"
  )
})

test_that("feed_lang and contact fields are written to feed_info", {
  events <- rt2s_events_from_stop_times(make_g2g_stop_times())
  feed <- suppressWarnings(rt2s_scaffold(
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

test_that("rt2s_publishable records blockers programmatically", {
  events <- rt2s_events_from_stop_times(make_g2g_stop_times())

  # No agency, no coords -> not publishable, two blockers
  bad <- suppressWarnings(rt2s_scaffold(events))
  st_bad <- rt2s_publishable(bad)
  expect_false(st_bad$publishable)
  expect_true(any(grepl("agency", st_bad$blockers)))
  expect_true(any(grepl("coordinates", st_bad$blockers)))

  # Fully specified -> publishable, no blockers
  good <- rt2s_scaffold(
    events,
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_lat = c(7.30, 7.31),
                       stop_lon = c(80.64, 80.65)),
    route_type = 3L
  )
  st_good <- rt2s_publishable(good)
  expect_true(st_good$publishable)
  expect_length(st_good$blockers, 0L)

  # Attribute survives on the object and matches the accessor
  expect_identical(attr(good, "publishable"), TRUE)
})

test_that("baseline-mode feeds are publishable when baseline is complete", {
  events <- rt2s_events_from_trip_updates(make_updates())
  feed <- suppressWarnings(rt2s_assemble(events, baseline = make_baseline()))
  expect_true(rt2s_publishable(feed)$publishable)
})

# Small synthetic fixtures for the regression tests below: one route, stops
# A/B/C, 90 s between stops, 20 s dwell. `pattern` is the ordered stop visit
# list of a trip (it may repeat a stop, e.g. a loop A, B, A, C).
synthetic_events <- function(trip_ref, day, pattern, stop_sequence = NA_integer_,
                             start = "06:00:00") {
  t0 <- as.POSIXct(paste(day, start), tz = "UTC")
  offsets <- 90 * (seq_along(pattern) - 1L)
  data.table::data.table(
    trip_ref = trip_ref,
    route_ref = "R1",
    shape_ref = NA_character_,
    direction_id = 0L,
    service_date = as.Date(day),
    stop_ref = pattern,
    stop_sequence = stop_sequence,
    arrival_time = t0 + offsets,
    departure_time = t0 + offsets + 20,
    provenance = "observed",
    vehicle_ref = "V1",
    source = "positions"
  )
}

synthetic_baseline <- function(trip_ids, pattern) {
  n <- length(pattern)
  planned <- as.POSIXct("2026-01-01 06:00:00", tz = "UTC") + 90 * (seq_len(n) - 1L)
  list(
    agency = data.frame(
      agency_id = "AG", agency_name = "Synthetic",
      agency_url = "https://example.org", agency_timezone = "UTC"
    ),
    stops = data.frame(
      stop_id = c("A", "B", "C"), stop_name = c("A", "B", "C"),
      stop_lat = c(52, 52.004, 52.008), stop_lon = 9
    ),
    routes = data.frame(
      route_id = "R1", agency_id = "AG", route_short_name = "1",
      route_long_name = "", route_type = 3L
    ),
    trips = data.frame(
      route_id = "R1", service_id = "S", trip_id = trip_ids, direction_id = 0L
    ),
    stop_times = data.frame(
      trip_id = rep(trip_ids, each = n),
      arrival_time = rep(format(planned, "%H:%M:%S"), length(trip_ids)),
      departure_time = rep(format(planned + 20, "%H:%M:%S"), length(trip_ids)),
      stop_id = rep(pattern, length(trip_ids)),
      stop_sequence = rep(seq_len(n), length(trip_ids))
    ),
    calendar = data.frame(
      service_id = "S", monday = 1L, tuesday = 1L, wednesday = 1L,
      thursday = 1L, friday = 1L, saturday = 1L, sunday = 1L,
      start_date = 20260101L, end_date = 20261231L
    )
  )
}

duplicated_keys <- function(stop_times) {
  stop_times[, .N, by = .(trip_id, stop_sequence)][N > 1L]
}

test_that("baseline mode pairs repeated stop visits by rank: one row per event", {
  loop <- c("A", "B", "A", "C")
  baseline <- synthetic_baseline("T1", loop)

  # Event stop_sequence supplied, and NA (baseline numbering must fill it in).
  for (supplied in list(1:4, NA_integer_)) {
    events <- synthetic_events("T1", "2026-07-22", loop, stop_sequence = supplied)
    feed <- rt2s_assemble(
      events, baseline = baseline, service_date = "2026-07-22", tz = "UTC"
    )
    st <- feed$stop_times
    expect_identical(nrow(st), nrow(events))
    expect_identical(nrow(duplicated_keys(st)), 0L)
    # the k-th observed visit at A takes the k-th planned sequence at A
    expect_identical(st$stop_sequence, 1:4)
    expect_identical(st$stop_id, loop)
    expect_identical(
      st$arrival_time,
      c("06:00:00", "06:01:30", "06:03:00", "06:04:30")
    )
  }

  # An observed visit beyond the planned count (a third pass at A) is not
  # multiplied either: it falls through to the chronology fallback.
  events <- synthetic_events("T1", "2026-07-22", c(loop, "A"))
  feed <- rt2s_assemble(
    events, baseline = baseline, service_date = "2026-07-22", tz = "UTC"
  )
  st <- feed$stop_times
  expect_identical(nrow(st), 5L)
  expect_identical(nrow(duplicated_keys(st)), 0L)
  expect_identical(st[stop_sequence > 10000L, stop_id], "A")
  expect_identical(st[stop_sequence > 10000L, arrival_time], "06:06:00")
})

test_that("rt2s_assemble keeps only the requested service_date in baseline mode", {
  events <- rbind(
    synthetic_events("T1", "2026-07-22", c("A", "B"), stop_sequence = 1:2),
    synthetic_events("T2", "2026-07-23", c("A", "B"), stop_sequence = 1:2)
  )
  baseline <- synthetic_baseline(c("T1", "T2"), c("A", "B"))

  feed <- rt2s_assemble(
    events, baseline = baseline, service_date = "2026-07-22", tz = "UTC"
  )
  expect_identical(as.character(feed$trips$trip_id), "T1")
  expect_identical(unique(feed$stop_times$trip_id), "T1")
  expect_identical(feed$stop_times$arrival_time, c("06:00:00", "06:01:30"))
  expect_identical(feed$calendar_dates$date, 20260722L)

  feed2 <- rt2s_assemble(
    events, baseline = baseline, service_date = "2026-07-23", tz = "UTC"
  )
  expect_identical(as.character(feed2$trips$trip_id), "T2")
  expect_identical(unique(feed2$stop_times$trip_id), "T2")
  expect_identical(feed2$calendar_dates$date, 20260723L)
})

test_that("rt2s_assemble keeps only the requested service_date in scaffold mode", {
  events <- rbind(
    synthetic_events("T1", "2026-07-22", c("A", "B"), stop_sequence = 1:2),
    synthetic_events("T2", "2026-07-23", c("A", "B"), stop_sequence = 1:2)
  )
  stops <- synthetic_baseline("T1", c("A", "B"))$stops

  feed <- rt2s_assemble(
    events,
    service_date = "2026-07-22",
    tz = "UTC",
    agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
    stops = stops,
    route_type = 3L
  )
  expect_identical(as.character(feed$trips$trip_id), "T1")
  expect_identical(unique(feed$stop_times$trip_id), "T1")
  expect_identical(feed$calendar_dates$date, 20260722L)
  expect_identical(feed$calendar_dates$service_id, "SVC_20260722")
})

test_that("events on another day are never rendered against the requested service_date", {
  # A trip whose first observed stop falls just after midnight is attributed
  # to 2026-07-23. Asking for the 2026-07-22 feed must not silently render
  # its clocks from 2026-07-22 midnight: nothing falls on that day.
  events <- synthetic_events("T1", "2026-07-23", c("A", "B"), start = "00:00:00")
  baseline <- synthetic_baseline("T1", c("A", "B"))

  expect_error(
    rt2s_assemble(
      events, baseline = baseline, service_date = "2026-07-22", tz = "UTC"
    ),
    "No events on service date"
  )
  expect_error(
    rt2s_assemble(
      events,
      service_date = "2026-07-22",
      tz = "UTC",
      agency = list(name = "X", url = "https://x.org", timezone = "UTC"),
      stops = baseline$stops,
      route_type = 3L
    ),
    "No events on service date"
  )

  # The day the events belong to renders from its own midnight.
  feed <- rt2s_assemble(
    events, baseline = baseline, service_date = "2026-07-23", tz = "UTC"
  )
  expect_identical(feed$stop_times$arrival_time, c("00:00:00", "00:01:30"))
  expect_identical(feed$calendar_dates$date, 20260723L)
})
