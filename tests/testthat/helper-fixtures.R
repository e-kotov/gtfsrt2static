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