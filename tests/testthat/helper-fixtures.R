ts <- function(x) as.POSIXct(x, tz = "UTC")

# gps2gtfs-shaped $stop_times output
make_g2g_stop_times <- function() {
  data.frame(
    trip_id = c(1L, 1L, 2L, 2L),
    vehicle_id = "7482",
    date = "2026-07-14",
    direction = c(1L, 1L, 2L, 2L),
    stop_id = c("S1", "S2", "S2", "S1"),
    arrival_time = c("06:31:07", "06:39:12", "07:10:02", "07:18:44"),
    departure_time = c("06:31:44", "06:39:31", "07:10:21", "07:19:01"),
    dwell_time_in_seconds = c(37, 19, 19, 17)
  )
}

# gtfsrealtime-shaped trip updates: two polls of one trip + extras
make_updates <- function() {
  data.frame(
    id = "TU_1",
    trip_id = c(
      "CS_1", "CS_1", # stop S1: prediction then post-passage report
      "CS_1", # stop S2: prediction only
      "CS_1", # stop S3: skipped
      "CS_9" # canceled trip
    ),
    route_id = "B62",
    direction_id = 0L,
    start_date = "20260714",
    trip_schedule_relationship = c(rep("SCHEDULED", 4), "CANCELED"),
    stop_sequence = c(4L, 4L, 5L, 6L, NA),
    stop_id = c("S1", "S1", "S2", "S3", NA),
    arrival_delay = c(120, 125, 150, NA, NA),
    arrival_time = ts(c(
      "2026-07-14 06:31:00", # prediction (poll 1)
      "2026-07-14 06:31:05", # revised (poll 2, after passage)
      "2026-07-14 06:33:10",
      NA,
      NA
    )),
    departure_delay = c(NA, NA, NA, NA, NA),
    departure_time = ts(c(
      "2026-07-14 06:31:30",
      "2026-07-14 06:31:40",
      "2026-07-14 06:33:26",
      NA,
      NA
    )),
    stop_schedule_relationship = c(
      "SCHEDULED",
      "SCHEDULED",
      "SCHEDULED",
      "SKIPPED",
      NA
    ),
    vehicle_id = "7482",
    file_timestamp = ts(c(
      "2026-07-14 06:25:00", # before arrival -> superseded anyway
      "2026-07-14 06:32:00", # after departure -> observed
      "2026-07-14 06:32:00", # before S2 arrival -> predicted-last
      "2026-07-14 06:32:00",
      "2026-07-14 06:32:00"
    ))
  )
}

# --- C6 observed-stop-events fixtures for the summarise module --------------
# Expected numbers are hand-worked from these synthetic fixtures.

# Assemble one route-direction's events from per-trip start clock strings and
# per-stop offset (seconds from trip start). dwell is constant.
make_events_from_offsets <- function(
  route,
  direction_id,
  date,
  starts, # named char vector trip_ref -> "HH:MM:SS"
  stops, # char vector of stop_ref, in offset order
  offsets, # matrix/data.frame [trip, stop] of second offsets, or list per trip
  dwell = 30L
) {
  base_day <- paste(date, "00:00:00")
  rows <- lapply(names(starts), function(tr) {
    t0 <- as.POSIXct(paste(date, starts[[tr]]), tz = "UTC")
    off <- offsets[[tr]]
    arr <- t0 + off
    data.frame(
      trip_ref = tr,
      route_ref = route,
      shape_ref = NA_character_,
      direction_id = as.integer(direction_id),
      service_date = as.Date(date),
      stop_ref = stops,
      stop_sequence = NA_integer_,
      arrival_time = arr,
      departure_time = arr + dwell,
      provenance = "observed",
      vehicle_ref = paste0("v_", tr),
      source = "positions",
      stringsAsFactors = FALSE
    )
  })
  data.table::rbindlist(rows)
}

# Fixture A: clean line R1/dir0, 4 weekday runs, stops S1,S2,S3, dwell 30s.
make_events_clean <- function() {
  make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-14", # Tuesday
    starts = c(A = "06:00:00", B = "06:10:00", C = "06:22:00", D = "06:40:00"),
    stops = c("S1", "S2", "S3"),
    offsets = list(
      A = c(0, 300, 600),
      B = c(0, 320, 650),
      C = c(0, 280, 580),
      D = c(0, 360, 700)
    )
  )
}

make_reference_passage_events <- function(
  route,
  direction_id,
  times,
  stop_ref = "S1",
  vehicle_ref = paste0("v", seq_along(times)),
  trip_ref = paste0("T", seq_along(times)),
  date = "2026-07-14"
) {
  arr <- if (inherits(times, "POSIXct")) {
    times
  } else {
    ts(paste(date, times))
  }
  n <- length(arr)
  data.frame(
    trip_ref = rep_len(trip_ref, n),
    route_ref = rep_len(route, n),
    shape_ref = NA_character_,
    direction_id = as.integer(rep_len(direction_id, n)),
    service_date = as.Date(rep_len(date, n)),
    stop_ref = rep_len(stop_ref, n),
    stop_sequence = NA_integer_,
    arrival_time = arr,
    departure_time = arr + 20L,
    provenance = "observed",
    vehicle_ref = as.character(rep_len(vehicle_ref, n)),
    source = "gps",
    stringsAsFactors = FALSE
  )
}

# Fixture PB: reference-stop passages whose source trip_ref is an all-day
# block-style id. S1 passages still define headways even though obs_headways()
# sees only one trip start.
make_events_shared_trip_ref_passages <- function() {
  date <- "2026-07-14"
  starts <- ts(c(
    "2026-07-14 06:00:00",
    "2026-07-14 06:10:00",
    "2026-07-14 06:25:00"
  ))
  rows <- lapply(seq_along(starts), function(i) {
    arr <- starts[i] + c(0, 300)
    data.frame(
      trip_ref = "block_R9",
      route_ref = "R9",
      shape_ref = NA_character_,
      direction_id = 0L,
      service_date = as.Date(date),
      stop_ref = c("S1", "S2"),
      stop_sequence = NA_integer_,
      arrival_time = arr,
      departure_time = arr + 20L,
      provenance = "observed",
      vehicle_ref = paste0("v", i),
      source = "gps",
      stringsAsFactors = FALSE
    )
  })
  data.table::rbindlist(rows)
}

# Fixture PD: repeated detections of one vehicle at a reference stop during a
# dwell, followed by two later vehicle passages.
make_events_reference_stop_dwell <- function() {
  times <- ts(c(
    "2026-07-14 06:00:00",
    "2026-07-14 06:00:30",
    "2026-07-14 06:01:00",
    "2026-07-14 06:10:00",
    "2026-07-14 06:20:00"
  ))
  data.frame(
    trip_ref = "block_R10",
    route_ref = "R10",
    shape_ref = NA_character_,
    direction_id = 0L,
    service_date = as.Date("2026-07-14"),
    stop_ref = "S1",
    stop_sequence = NA_integer_,
    arrival_time = times,
    departure_time = times + 10L,
    provenance = "observed",
    vehicle_ref = c("v1", "v1", "v1", "v2", "v3"),
    source = "gps",
    stringsAsFactors = FALSE
  )
}

# Fixture PS: one physical stop used by both directions.
make_events_shared_reference_stop <- function() {
  data.table::rbindlist(list(
    make_events_from_offsets(
      route = "R11",
      direction_id = 0L,
      date = "2026-07-14",
      starts = c(A = "06:00:00", B = "06:10:00"),
      stops = "S_shared",
      offsets = list(A = 0L, B = 0L)
    ),
    make_events_from_offsets(
      route = "R11",
      direction_id = 1L,
      date = "2026-07-14",
      starts = c(C = "06:05:00", D = "06:15:00"),
      stops = "S_shared",
      offsets = list(C = 0L, D = 0L)
    )
  ))
}

# Fixture PA: several route-directions where automatic reference-stop selection
# must skip a shared stop and choose the direction-unique alternative.
make_events_auto_reference_selection <- function() {
  data.table::rbindlist(list(
    make_events_from_offsets(
      route = "R20",
      direction_id = 0L,
      date = "2026-07-14",
      starts = c(A = "06:00:00", B = "06:10:00", C = "06:20:00"),
      stops = c("S_shared", "S20_0"),
      offsets = list(A = c(0, 100), B = c(0, 100), C = c(0, 100))
    ),
    make_events_from_offsets(
      route = "R20",
      direction_id = 1L,
      date = "2026-07-14",
      starts = c(D = "06:05:00", E = "06:17:00", F = "06:29:00"),
      stops = c("S_shared", "S20_1"),
      offsets = list(D = c(0, 100), E = c(0, 100), F = c(0, 100))
    ),
    make_events_from_offsets(
      route = "R21",
      direction_id = 0L,
      date = "2026-07-14",
      starts = c(G = "06:03:00", H = "06:18:00", I = "06:33:00"),
      stops = "S21_0",
      offsets = list(G = 0L, H = 0L, I = 0L)
    )
  ))
}

# Fixture PR: one vehicle returns to the reference stop after the revisit gap.
make_events_same_vehicle_revisit <- function() {
  make_reference_passage_events(
    route = "R22",
    direction_id = 0L,
    times = c("06:00:00", "06:20:00"),
    stop_ref = "S1",
    vehicle_ref = "v1",
    trip_ref = "block_R22"
  )
}

# Fixture PC: a long chain of sub-gap detections is still one passage.
make_events_chained_subgap <- function() {
  make_reference_passage_events(
    route = "R23",
    direction_id = 0L,
    times = c("06:00:00", "06:09:50", "06:19:40"),
    stop_ref = "S1",
    vehicle_ref = "v1",
    trip_ref = "block_R23"
  )
}

# Fixture PN: unknown vehicles must not collapse into one shared vehicle key.
make_events_missing_vehicle_passages <- function() {
  data.table::rbindlist(list(
    make_reference_passage_events(
      route = "R30",
      direction_id = 0L,
      times = c("06:00:00", "06:05:00", "06:10:00", "06:15:00"),
      stop_ref = "S30",
      vehicle_ref = c("known_1", NA_character_, "known_2", NA_character_),
      trip_ref = "block_R30"
    ),
    make_reference_passage_events(
      route = "R31",
      direction_id = 0L,
      times = c("06:00:00", "06:05:00", "06:10:00"),
      stop_ref = "S31",
      vehicle_ref = NA_character_,
      trip_ref = "block_R31"
    ),
    make_reference_passage_events(
      route = "R32",
      direction_id = 0L,
      times = c("06:00:00", "06:12:00", "06:24:00"),
      stop_ref = "S32",
      vehicle_ref = NA_character_,
      trip_ref = "block_R32"
    )
  ))
}

# Fixture PG: duplicate and over-cutoff passage gaps.
make_events_passage_gap_filters <- function() {
  make_reference_passage_events(
    route = "R34",
    direction_id = 0L,
    times = c("06:00:00", "06:00:00", "06:20:00", "12:00:00"),
    stop_ref = "S1",
    vehicle_ref = c("v1", "v2", "v3", "v4"),
    trip_ref = "block_R34"
  )
}

# Fixture PD2: four passages on each of two service dates.
make_events_passage_two_dates <- function() {
  data.table::rbindlist(list(
    make_reference_passage_events(
      route = "R35",
      direction_id = 0L,
      times = c("06:00:00", "06:10:00", "06:20:00", "06:30:00"),
      stop_ref = "S1",
      trip_ref = "block_R35a",
      date = "2026-07-14"
    ),
    make_reference_passage_events(
      route = "R35",
      direction_id = 0L,
      times = c("06:00:00", "06:10:00", "06:20:00", "06:30:00"),
      stop_ref = "S1",
      trip_ref = "block_R35b",
      date = "2026-07-15"
    )
  ))
}

# Fixture PM: both routes have trip-start headways, but only one route serves
# the explicitly supplied passage reference stop.
make_events_missing_reference_group <- function() {
  data.table::rbindlist(list(
    make_events_from_offsets(
      route = "RA",
      direction_id = 0L,
      date = "2026-07-14",
      starts = c(A1 = "06:00:00", A2 = "06:10:00", A3 = "06:20:00"),
      stops = c("SA", "SA2"),
      offsets = list(A1 = c(0, 120), A2 = c(0, 120), A3 = c(0, 120))
    ),
    make_events_from_offsets(
      route = "RB",
      direction_id = 0L,
      date = "2026-07-14",
      starts = c(B1 = "06:00:00", B2 = "06:12:00", B3 = "06:24:00"),
      stops = c("SB", "SB2"),
      offsets = list(B1 = c(0, 120), B2 = c(0, 120), B3 = c(0, 120))
    )
  ))
}

# Fixture PU: a candidate reference stop has known-direction and
# unknown-direction rows. Unknown direction must not make the stop shared.
make_events_partly_unknown_direction <- function() {
  data.table::rbindlist(list(
    make_reference_passage_events(
      route = "R36",
      direction_id = 0L,
      times = c("06:00:00", "06:10:00", "06:20:00"),
      stop_ref = "S1",
      vehicle_ref = c("k1", "k2", "k3"),
      trip_ref = "block_R36_known"
    ),
    make_reference_passage_events(
      route = "R36",
      direction_id = NA_integer_,
      times = c("06:05:00", "06:15:00"),
      stop_ref = "S1",
      vehicle_ref = c("u1", "u2"),
      trip_ref = "block_R36_unknown"
    )
  ))
}

# Fixture PU2: every candidate reference-stop row lacks direction.
make_events_all_unknown_direction <- function() {
  make_reference_passage_events(
    route = "R37",
    direction_id = NA_integer_,
    times = c("06:00:00", "06:10:00", "06:20:00"),
    stop_ref = "S1",
    vehicle_ref = c("u1", "u2", "u3"),
    trip_ref = "block_R37_unknown"
  )
}

# Fixture B: tied arrival seconds. Anchor S0 at +0; SA/SB tie at +100 in P1 but
# their median offsets differ (SA 95 < SB 105), so canonical order is S0,SA,SB.
make_events_tied <- function() {
  make_events_from_offsets(
    route = "R2",
    direction_id = 0L,
    date = "2026-07-15",
    starts = c(P1 = "08:00:00", P2 = "08:30:00"),
    stops = c("S0", "SA", "SB"),
    offsets = list(
      P1 = c(0, 100, 100),
      P2 = c(0, 90, 110)
    ),
    dwell = 10L
  )
}

# Fixture C: Fixture A plus one Saturday run of the same line.
make_events_weekmix <- function() {
  wd <- make_events_clean()
  sat <- make_events_from_offsets(
    route = "R1",
    direction_id = 0L,
    date = "2026-07-18", # Saturday
    starts = c(E = "06:05:00"),
    stops = c("S1", "S2", "S3"),
    offsets = list(E = c(0, 310, 640))
  )
  data.table::rbindlist(list(wd, sat))
}

# Fixture D: single run (no headway) + an all-stationary trip (all offsets 0).
make_events_degenerate <- function() {
  single <- make_events_from_offsets(
    route = "R3",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(S = "09:00:00"),
    stops = c("S1", "S2"),
    offsets = list(S = c(0, 120))
  )
  stationary <- make_events_from_offsets(
    route = "R4",
    direction_id = 0L,
    date = "2026-07-14",
    starts = c(T1 = "09:00:00", T2 = "09:10:00"),
    stops = c("S1", "S2"),
    offsets = list(T1 = c(0, 0), T2 = c(0, 0))
  )
  data.table::rbindlist(list(single, stationary))
}

# Fixture SK: served S1,S2 across 3 weekday runs plus a stop S3 that is SKIPPED
# on every run (non-NA stop_ref, NA times, provenance "skipped"). Exercises the
# served-only filter: S3 must never enter the representative pattern - if it
# did, its all-NA quantiles would render as "NA:NA:NA" GTFS clock strings.
make_events_with_skipped <- function() {
  starts <- c(A = "06:00:00", B = "06:10:00", C = "06:22:00")
  s2off <- c(A = 300, B = 320, C = 280)
  date <- "2026-07-14" # Tuesday
  rows <- lapply(names(starts), function(tr) {
    t0 <- as.POSIXct(paste(date, starts[[tr]]), tz = "UTC")
    served <- data.frame(
      trip_ref = tr, route_ref = "R7", shape_ref = NA_character_,
      direction_id = 0L, service_date = as.Date(date),
      stop_ref = c("S1", "S2"), stop_sequence = NA_integer_,
      arrival_time = t0 + c(0, s2off[[tr]]),
      departure_time = t0 + c(0, s2off[[tr]]) + 30L,
      provenance = "observed", vehicle_ref = paste0("v_", tr),
      source = "positions", stringsAsFactors = FALSE
    )
    skipped <- data.frame(
      trip_ref = tr, route_ref = "R7", shape_ref = NA_character_,
      direction_id = 0L, service_date = as.Date(date),
      stop_ref = "S3", stop_sequence = NA_integer_,
      arrival_time = as.POSIXct(NA, tz = "UTC"),
      departure_time = as.POSIXct(NA, tz = "UTC"),
      provenance = "skipped", vehicle_ref = paste0("v_", tr),
      source = "positions", stringsAsFactors = FALSE
    )
    rbind(served, skipped)
  })
  data.table::rbindlist(rows)
}

# Fixture NP: two "runs" whose only timed observation is a SKIPPED stop bearing
# a stale timestamp. Unserved rows never count as trip starts, so no headway is
# produced - snapshot_frequencies must reject the input (no usable headway)
# rather than treat the stale times as service.
make_events_skipped_only_trips <- function() {
  mk <- function(tr, start) {
    t0 <- as.POSIXct(paste("2026-07-14", start), tz = "UTC")
    data.frame(
      trip_ref = tr, route_ref = "R8", shape_ref = NA_character_,
      direction_id = 0L, service_date = as.Date("2026-07-14"),
      stop_ref = "S1", stop_sequence = NA_integer_,
      arrival_time = t0, departure_time = as.POSIXct(NA, tz = "UTC"),
      provenance = "skipped", vehicle_ref = "v", source = "trip_updates",
      stringsAsFactors = FALSE
    )
  }
  data.table::rbindlist(list(mk("T1", "06:00:00"), mk("T2", "06:10:00")))
}

make_baseline <- function() {
  list(
    agency = data.frame(
      agency_id = "AG1",
      agency_name = "Example Transit",
      agency_url = "https://example-transit.org",
      agency_timezone = "UTC"
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      stop_name = c("Stop 1", "Stop 2", "Stop 3"),
      stop_lat = c(40.71, 40.72, 40.73),
      stop_lon = c(-74.01, -74.02, -74.03)
    ),
    routes = data.frame(
      route_id = "B62",
      agency_id = "AG1",
      route_short_name = "B62",
      route_long_name = "",
      route_type = 3L
    ),
    trips = data.frame(
      route_id = "B62",
      service_id = "weekday",
      trip_id = c("CS_1", "CS_9"),
      direction_id = 0L
    ),
    stop_times = data.frame(
      trip_id = rep(c("CS_1", "CS_9"), each = 3),
      arrival_time = rep(c("06:29:00", "06:31:30", "06:34:00"), 2),
      departure_time = rep(c("06:29:30", "06:32:00", "06:34:30"), 2),
      stop_id = rep(c("S1", "S2", "S3"), 2),
      stop_sequence = rep(4:6, 2)
    ),
    calendar = data.frame(
      service_id = "weekday",
      monday = 1, tuesday = 1, wednesday = 1, thursday = 1, friday = 1,
      saturday = 0, sunday = 0,
      start_date = 20260101L,
      end_date = 20261231L
    )
  )
}
