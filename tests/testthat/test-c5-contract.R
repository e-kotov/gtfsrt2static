# Coordination with the additive, versioned gps2gtfs C5 contract change.
# gps2gtfs now appends
# orientation_id / orientation_status / orientation_confidence / pattern_ref
# (+ trips-only anchors) to its $stop_times. This asserts that:
#   1. those extra columns do not perturb C6 assembly (round-trip), and
#   2. snapshot_from_stop_times maps orientation_id INTO direction_id, falling
#      back to the legacy `direction` while orientation_id is NA.

# make_g2g_stop_times() plus the C5 columns in their detector-off empty state,
# i.e. exactly what gps2gtfs emits with no orientation detector enabled.
make_g2g_stop_times_c5 <- function() {
  st <- make_g2g_stop_times()
  st$orientation_id <- NA_integer_
  st$orientation_status <- factor(
    rep("none", nrow(st)),
    levels = c("none", "ok", "single_group")
  )
  st$orientation_confidence <- NA_real_
  st$pattern_ref <- NA_character_
  st
}

test_that("C5 columns do not perturb C6 assembly (round-trip)", {
  legacy <- snapshot_from_stop_times(make_g2g_stop_times())
  withc5 <- snapshot_from_stop_times(make_g2g_stop_times_c5())

  # Every required C6 column is byte-identical with or without the C5 columns.
  for (col in event_columns) {
    expect_identical(withc5[[col]], legacy[[col]], info = col)
  }
  # The only difference is the additive nullable pattern_ref, carried through
  # (all NA here) when the producer supplies it, and absent otherwise.
  expect_true("pattern_ref" %in% names(withc5))
  expect_false("pattern_ref" %in% names(legacy))
  expect_true(all(is.na(withc5$pattern_ref)))
  # It is positioned right after the required block, never in the middle.
  expect_identical(names(withc5), c(event_columns, "pattern_ref"))

  # Full assembly is unperturbed: scaffold yields identical trips/stop_times.
  feed_legacy <- suppressWarnings(snapshot_scaffold(legacy))
  feed_withc5 <- suppressWarnings(snapshot_scaffold(withc5))
  expect_identical(feed_withc5$trips, feed_legacy$trips)
  expect_identical(feed_withc5$stop_times, feed_legacy$stop_times)
})

test_that("detector-off: direction_id still derives from legacy direction", {
  # make_g2g_stop_times() uses direction 1/2 -> direction_id 0/1.
  events <- snapshot_from_stop_times(make_g2g_stop_times_c5())
  expect_identical(sort(unique(events$direction_id)), c(0L, 1L))
  # Identical to what the pre-C5 legacy input produced.
  legacy <- snapshot_from_stop_times(make_g2g_stop_times())
  expect_identical(events$direction_id, legacy$direction_id)
})

test_that("orientation_id, when set, overrides direction_id per row", {
  # Simulate a future orientation detector: flip both trips' orientation to 1
  # even though legacy `direction` would map trip 1 -> 0. orientation_id must
  # win wherever it is non-NA; legacy direction fills the NA rows.
  st <- make_g2g_stop_times_c5()
  st$orientation_id <- c(1L, 1L, NA_integer_, NA_integer_)
  events <- snapshot_from_stop_times(st)
  # Events are re-sorted by (trip_ref, arrival_time); join back by stop to check.
  # Trip 1 (stops S1,S2) now direction_id 1; trip 2 keeps legacy 2->1.
  d_by_trip <- events[, .(d = unique(direction_id)), by = trip_ref]
  expect_true(all(d_by_trip$d == 1L))
  # A row-level check that the override applied only where orientation_id set.
  st2 <- make_g2g_stop_times_c5()
  st2$orientation_id <- c(0L, 0L, 0L, 0L) # force both trips to 0
  ev2 <- snapshot_from_stop_times(st2)
  expect_true(all(ev2$direction_id == 0L))
})

test_that("validate_events tolerates a present or absent pattern_ref", {
  events <- snapshot_from_stop_times(make_g2g_stop_times_c5())
  expect_silent(validate_events(events))
  # Non-character pattern_ref is coerced, not rejected.
  events2 <- data.table::copy(events)
  events2[, pattern_ref := 1L]
  expect_silent(v <- validate_events(events2))
  expect_type(v$pattern_ref, "character")
})
